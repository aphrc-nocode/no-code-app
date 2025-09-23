from fastapi import FastAPI, UploadFile, File, Form
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import FileResponse, JSONResponse
import shap
import numpy as np
import pandas as pd
import pycaret
from pycaret.clustering import setup as clu_setup, create_model as clu_create, pull as clu_pull
import uvicorn
import matplotlib
from io import BytesIO
import csv
import io
import base64
import matplotlib.pyplot as plt
import tempfile, os
from sklearn.metrics import confusion_matrix, roc_curve, auc
import math
#from pycaret.classification import load_model
import tempfile
import os
 
from pycaret.classification import (
    compare_models as clf_compare,
    setup as clf_setup,
    create_model as clf_create,
    evaluate_model as clf_evaluate,
    pull as clf_pull,
    plot_model,
    predict_model,
    save_model,
    get_config,
    load_model,
    interpret_model
)

from pycaret.classification import setup as clf_setup, compare_models as clf_compare, pull as clf_pull, save_model
from pycaret.clustering import setup as clu_setup, create_model as clu_create, pull as clu_pull

import os, tempfile, math, base64, csv, io
from concurrent.futures import ThreadPoolExecutor

# Répertoire racine du projet (là où se trouve ce fichier)
BASE_DIR = os.path.dirname(os.path.abspath(__file__)) 
MODELS_DIR = os.path.join(BASE_DIR, "..", "models")
MODELS_DIR = os.path.abspath(MODELS_DIR)




def read_csv_flexible(path):
    """Try to read fole with comma (,) or semi comma(;) if one column was detected """
    df = pd.read_csv(path)
    if len(df.columns) == 1:
        df = pd.read_csv(path, sep=';')
    return df


app = FastAPI()

# Autoriser CORS si nécessaire pour Shiny
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # Remplace "*" par l'URL de ton app Shiny si tu veux restreindre
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

@app.post("/automl")
async def automl(
    file: UploadFile = File(...),
    target: str = Form(None),
    session_id: int = Form(123),
    analysis_type: str = Form("supervised"),
    n_models: int = Form(10)
):
    # lecture rapide en tmp
    with tempfile.NamedTemporaryFile(delete=False, suffix=".csv") as tmp:
        tmp.write(await file.read())
        tmp_path = tmp.name
    df = pd.read_csv(tmp_path)
    os.remove(tmp_path)

    if analysis_type == "supervised":

        n_models = int(n_models)
        clf_setup(data=df, target=target, session_id=session_id, html=False, n_jobs=-1)
        top_models = clf_compare(n_select=n_models)
        best = top_models[0] if isinstance(top_models, list) else top_models

        leaderboard = clf_pull().reset_index(drop=True).head(n_models)
        # essaie de produire un id exploitable pour /deploy_model
        if "Model" in leaderboard.columns and "id" not in leaderboard.columns:
            leaderboard["id"] = leaderboard["Model"].str.extract(r"^(\w+)").iloc[:,0].str.lower()

        # déploiement "par défaut"
        save_model(best, os.path.join(MODELS_DIR, "deployed_model"))

        return JSONResponse(content={
            "best_model_name": str(best),
            "leaderboard": leaderboard.to_dict(orient="records")
        })

    elif analysis_type == "unsupervised":
        clu_setup(data=df, session_id=session_id)
        model = clu_create('kmeans')
        clustering_result = clu_pull()
        return {
            "best_model_name": str(model),
            "clustering_result": clustering_result.to_dict(orient="records")
        }

    else:
        return {"error": "Invalid analysis type. Must be 'supervised' or 'unsupervised'."}


# Fonction for cleaning JSON data
# This function will replace NaN and Inf values with None
def clean_json(obj):
    if isinstance(obj, dict):
        return {k: clean_json(v) for k, v in obj.items()}
    elif isinstance(obj, list):
        return [clean_json(x) for x in obj]
    elif isinstance(obj, float):
        if math.isnan(obj) or math.isinf(obj):
            return None
        return obj
    else:
        return obj

# Model evaluation endpoint
@app.post("/evaluate_model")
async def evaluate_model(
    file: UploadFile = File(...),
    target: str = Form(...),
    model_name: str = Form(...),
    session_id: int = Form(123)
):
    with tempfile.NamedTemporaryFile(delete=False, suffix=".csv") as tmp:
        tmp.write(await file.read())
        tmp_path = tmp.name
    df = pd.read_csv(tmp_path)
    os.remove(tmp_path)

    clf_setup(data=df, target=target, session_id=session_id, html=False, n_jobs=-1)
    model = clf_create(model_name)
    pred_df = predict_model(model, data=df)

    # y_true / y_pred
    y_true = df[target]
    for col in ['Label','prediction_label','prediction','predicted_label']:
        if col in pred_df.columns:
            y_pred = pred_df[col]
            break
    else:
        raise ValueError("No prediction label column found in predict_model().")

    # y_score (proba) si dispo
    y_score = None
    for col in ['prediction_score','Score','Probability_1','Probability']:
        if col in pred_df.columns:
            y_score = pred_df[col]
            break

    metrics = clf_pull().to_dict(orient="records")

    # Feature importance
    try:
        features = get_config("X_train_transformed").columns
        if hasattr(model, "feature_importances_"):
            vals = model.feature_importances_
        elif hasattr(model, "coef_"):
            coef = model.coef_
            vals = coef.mean(axis=0) if getattr(coef, "ndim", 1) > 1 else coef
        else:
            vals, features = [], []
        if len(vals) == len(features):
            feature_importance = pd.DataFrame({"Feature": features, "Importance": vals}).sort_values("Importance", ascending=False).to_dict(orient="records")
        else:
            feature_importance = []
    except Exception:
        feature_importance = []

    # Confusion matrix + ROC
    cm = confusion_matrix(y_true, y_pred).tolist()
    if y_score is not None:
        try:
            fpr, tpr, thresholds = roc_curve(y_true, y_score)
            roc_data = {"fpr": fpr.tolist(), "tpr": tpr.tolist(), "thresholds": thresholds.tolist(), "auc": float(auc(fpr, tpr))}
        except Exception:
            roc_data = None
    else:
        roc_data = None

    # SHAP — échantillonnage pour éviter les explosions de temps/mémoire
    try:
        X = get_config("X_train_transformed")
        if X.shape[0] > 100:
            X = X.sample(100, random_state=42)
        explainer = shap.Explainer(model, X)
        shap_values = explainer(X)
        arr = shap_values.values
        feats = shap_values.feature_names
        if arr.ndim == 2 and arr.shape[1] == len(feats):
            mean_abs = np.abs(arr).mean(axis=0)
        elif arr.ndim == 3:
            # (n_obs, n_classes, n_features) OU (n_obs, n_features, n_classes)
            if arr.shape[2] == len(feats):
                mean_abs = np.abs(arr).mean(axis=(0,1))
            elif arr.shape[1] == len(feats):
                mean_abs = np.abs(np.transpose(arr, (0,2,1))).mean(axis=(0,1))
            else:
                raise ValueError("Unexpected SHAP shape")
        else:
            mean_abs = []
            feats = []
        shap_values_json = pd.DataFrame({"Feature": feats, "Importance": mean_abs}).sort_values("Importance", ascending=False).to_dict(orient="records")
    except Exception:
        shap_values_json = []

    # Step 10 : Save model on py folder
    #model_path = f"models/{model_name}"
    #os.makedirs("models", exist_ok=True)
    #save_model(model, model_path)

    # Chemin final du modèle
    model_path = os.path.join(MODELS_DIR, model_name)
    print("Dossier modèles absolu :", MODELS_DIR)
    save_model(model, model_path)


    # Step 11 : Clean and return
    result = {
        "model_name": str(model),
        "metrics": metrics,
        "plots_data": {
            "confusion_matrix": cm,
            "roc_curve": roc_data,
            "feature_importance": feature_importance,
            "shap_values": shap_values_json,
            "y_true": y_true.tolist(),
            "y_pred": y_pred.tolist(),
            "y_score": None if y_score is None else y_score.tolist(),
        },
        "message": f"Model '{model_name}' saved in {MODELS_DIR}/{model_name}.pkl"
    })


# Prediction endpoint
@app.post("/predict_model")
async def run_prediction(
    train_file: UploadFile = File(None),
    test_file: UploadFile = File(...),
    target: str = Form(None),
    model_name: str = Form(None),  # si fourni, on le charge ; sinon "deployed_model"
    session_id: int = Form(123)
):
    # Lire le test
    with tempfile.NamedTemporaryFile(delete=False, suffix=".csv") as tmp_test:
        tmp_test.write(await test_file.read())
        path_test = tmp_test.name
    df_test = read_csv_flexible(path_test)
    os.remove(path_test)

    # Charger le modèle (priorité à model_name)
    try:
        to_load = model_name if model_name else os.path.join(MODELS_DIR, "deployed_model")
        model = load_model(to_load)  # PyCaret accepte le chemin sans .pkl
    except Exception as e:
        return {"error": f"Error API (predict_model) : {str(e)}"}
    
    

from pycaret.classification import load_model, predict_model
import csv
import pandas as pd
import tempfile
import os
from fastapi import UploadFile, File
from pycaret.classification import load_model, predict_model

@app.post("/predict_deployed_model")
async def predict_deployed_model(file: UploadFile = File(...)):
    # Sauvegarder temporairement le fichier reçu
    with tempfile.NamedTemporaryFile(delete=False, suffix=".csv") as tmp:
        contents = await file.read()
        tmp.write(contents)
        tmp_path = tmp.name

    # Détecter le séparateur automatiquement
    with open(tmp_path, "r", encoding="utf-8") as f:
        sample = f.read(2048)
        f.seek(0)
        try:
            dialect = csv.Sniffer().sniff(sample, delimiters=",;")
            sep = dialect.delimiter
        except csv.Error:
            sep = ","  # fallback par défaut

    # Lire le fichier avec le bon séparateur
    try:
        df = pd.read_csv(tmp_path, sep=sep)
        os.remove(tmp_path)
    except Exception as e:
        return {"error": f"CSV file read error : {str(e)}"}

    # Charger le modèle
    try:
        model = load_model("deployed_model")
        predictions = predict_model(model, data=df)
        return predictions.to_json(orient="records")
    except Exception as e:
        return {
            "error": f"Prediction error : {str(e)}",
            "columns_received": df.columns.tolist()
        }


@app.post("/deploy_model")
async def deploy_model(model_id: str = Form(...)):
    import shutil
    import os
    # Chemins des modèles
    model_source_path = f"models/{model_id}.pkl"
    model_dest_path = "deployed_model.pkl"

    # Vérification existence
    if not os.path.exists(model_source_path):
        return {"error": f"Modèle '{model_id}' non trouvé. Avez-vous bien évalué ce modèle ?"}

    # Copier le fichier vers le modèle déployé
    shutil.copyfile(model_source_path, model_dest_path)

    return {"message": f"✅ Modèle '{model_id}' déployé avec succès."}
