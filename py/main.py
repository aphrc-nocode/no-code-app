from fastapi import FastAPI, UploadFile, File, Form
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import FileResponse, JSONResponse
import shap
import numpy as np
import pandas as pd
import pycaret
from pycaret.clustering import setup as clu_setup, create_model as clu_create, pull as clu_pull
import tempfile
import os
import uvicorn
import base64
import matplotlib
from io import BytesIO
import csv
import io
import base64
import matplotlib.pyplot as plt
import tempfile, os
import pandas as pd
from sklearn.metrics import confusion_matrix, roc_curve, auc
import math
 
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
    interpret_model
)



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
    n_models: int = Form(10)  # Default value => 10
):
        # Lire le fichier temporairement
    with tempfile.NamedTemporaryFile(delete=False, suffix=".csv") as tmp:
        contents = await file.read()
        tmp.write(contents)
        tmp_path = tmp.name
    # Lire le CSV
    df = pd.read_csv(tmp_path)
    print("Columns:", df.columns.tolist())
    print(df.head())
    print("Target:", target)

    if analysis_type == "supervised":
        # Setup + compare_models pour apprentissage supervisé (classification ici)
        clf_setup(data=df, target=target, session_id=session_id)
        #top_models = clf_compare(n_select=10)
        #leaderboard = clf_pull()
        # Manage index reset to get internal IDs
        #top_models = clf_compare(n_select=10)
        top_models = clf_compare(n_select=n_models)
        n_models = int(n_models)  # Force la conversion (protection)
        top_models = clf_compare(n_select=n_models)
        # Check if top_models is empty
        print("No of models for training :", n_models)
        leaderboard = clf_pull() # important pour récupérer l'identifiant interne
        leaderboard = leaderboard.reset_index()  # assure que l'index est une colonne
        # Limit the leaderboard
        leaderboard = leaderboard.iloc[:n_models]
        # L'index du leaderboard contient les model_ids
        if "index" in leaderboard.columns:
            leaderboard["model_id"] = leaderboard["index"].str.lower()
        else:
            # fallback au nom du modèle, peu fiable mais au cas où
            leaderboard["model_id"] = leaderboard["Model"].str.extract(r"^(\w+)").iloc[:,0].str.lower()
        print(leaderboard[["index", "Model"]])
        # Nettoyage
        os.remove(tmp_path)
        return JSONResponse(content={
            "best_model_name": str(top_models[0]),
            "leaderboard": leaderboard.to_dict(orient="records")
        })

    elif analysis_type == "unsupervised":
        # Setup + create_model for clustering (exemple: kmeans)
        clu_setup(data=df#, silent=True
                  , session_id=session_id)
        model = clu_create('kmeans')  # We can specify other clustering models here
        print("Clustering model created:", model)
        clustering_result = clu_pull()
        # Cleaning
        os.remove(tmp_path)
        return {
            #"type": "unsupervised",
            "best_model_name": str(top_models[0]),
            #"model_used": str(model),
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

@app.post("/evaluate_model")
async def evaluate_model(
    file: UploadFile = File(...),
    target: str = Form(...),
    model_name: str = Form(...),
    session_id: int = Form(123)
):
    # Step 1 : Read CSV
    with tempfile.NamedTemporaryFile(delete=False, suffix=".csv") as tmp:
        contents = await file.read()
        tmp.write(contents)
        tmp_path = tmp.name

    df = pd.read_csv(tmp_path)
    os.remove(tmp_path)

    # Step 2 : Setup
    clf_setup(data=df, target=target, session_id=session_id, verbose=False)

    # Step 3 : Train model
    model = clf_create(model_name)

    # Step 4 : Predictions
    pred_df = predict_model(model, data=df)
    print("Columns in pred_df :", pred_df.columns.tolist())
    print("Overview of pred_df :\n", pred_df.head().to_string())

    y_true = df[target]

    # Find y_pred
    possible_label_cols = ['Label', 'prediction_label', 'prediction', 'predicted_label']
    y_pred = None
    for col in possible_label_cols:
        if col in pred_df.columns:
            y_pred = pred_df[col]
            print(f"y_pred founded in : {col}")
            break
    if y_pred is None:
        raise ValueError("Any prediction column has founded in predict_model().")

    # Trouver y_score pour ROC
    score_cols = ['prediction_score', 'Score', 'Probability_1', 'Probability']
    y_score = None
    for col in score_cols:
        if col in pred_df.columns:
            y_score = pred_df[col]
            print(f"y_score found in column : {col}")
            break
    if y_score is not None:
        print("y_score (sample):", y_score.head().tolist())

    # Step 5 : Metrics
    metrics = clf_pull().to_dict(orient="records")

    # Step 6 : Feature importance
    try:
        # Utiliser les vraies colonnes transformées utilisées pour entraîner le modèle
        features = get_config("X_train_transformed").columns

        if hasattr(model, "feature_importances_"):
            importance_values = model.feature_importances_

        elif hasattr(model, "coef_"):
            coef = model.coef_
            if coef.ndim > 1:
                importance_values = coef.mean(axis=0)
            else:
                importance_values = coef

        else:
            print("Model without attribut feature_importances_ ni coef_.")
            importance_values = []
            features = []

        if len(importance_values) == len(features):
            feature_importance = pd.DataFrame({
                "Feature": features,
                "Importance": importance_values
            }).sort_values(by="Importance", ascending=False).to_dict(orient="records")
            print("Feature importance get.")
        else:
            print(f"Mismatch length corrected : {len(features)} features vs {len(importance_values)} importances")
            feature_importance = []

    except Exception as e:
        print("Error feature importance :", e)
        feature_importance = []

    # Step 7 : Confusion matrix
    cm = confusion_matrix(y_true, y_pred).tolist()

    # Step 8 : ROC curve
    if y_score is not None:
        try:
            fpr, tpr, thresholds = roc_curve(y_true, y_score)
            roc_auc = auc(fpr, tpr)
            roc_data = {
                "fpr": fpr.tolist(),
                "tpr": tpr.tolist(),
                "thresholds": thresholds.tolist(),
                "auc": roc_auc
            }
            print(f"ROC data generated with AUC = {roc_auc:.3f}")
        except Exception as e:
            print("ROC generation error :", e)
            roc_data = None
    else:
        print("ROC not generated : y_score is None")
        roc_data = None
        
    # Step 9 : SHAP values
    try:
        X = get_config("X_train_transformed")

        explainer = shap.Explainer(model, X)
        shap_values = explainer(X)

        shap_arr = shap_values.values
        feature_names = shap_values.feature_names

        print("SHAP values shape:", shap_arr.shape)
        print("Feature names:", len(feature_names))

        # Case 1 : (n_obs, n_features)
        if shap_arr.ndim == 2 and shap_arr.shape[1] == len(feature_names):
            mean_abs_shap = np.abs(shap_arr).mean(axis=0)

        # Case 2 : (n_obs, n_classes, n_features)
        elif shap_arr.ndim == 3 and shap_arr.shape[2] == len(feature_names):
            mean_abs_shap = np.abs(shap_arr).mean(axis=(0, 1))

        # Case 3 : (n_obs, n_features, n_classes) switch to (n_obs, n_classes, n_features)
        elif shap_arr.ndim == 3 and shap_arr.shape[1] == len(feature_names):
            print("Transposing SHAP array: (n_obs, n_feat, n_class) → (n_obs, n_class, n_feat)")
            shap_arr = np.transpose(shap_arr, axes=(0, 2, 1))
            mean_abs_shap = np.abs(shap_arr).mean(axis=(0, 1))

        else:
            raise ValueError(f"SHAP shape incompatible: {shap_arr.shape} vs {len(feature_names)} features")

        shap_df = pd.DataFrame({
            "Feature": feature_names,
            "Importance": mean_abs_shap
        }).sort_values(by="Importance", ascending=False)

        shap_values_json = shap_df.to_dict(orient="records")
        print("SHAP values generated.")

    except Exception as e:
        print("SHAP not generated :", e)
        shap_values_json = []

    # Step 10 : Save model
    model_path = f"models/{model_name}"
    os.makedirs("models", exist_ok=True)
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
            "y_score": y_score.tolist() if y_score is not None else None
        },
        "message": f"Model '{model_name}' saved in {model_path}.pkl"
    }

    return clean_json(result)


# Prediction endpoint
@app.post("/predict_model")
async def run_prediction(
    train_file: UploadFile = File(...),
    test_file: UploadFile = File(...),
    target: str = Form(...),
    model_name: str = Form(...),  # Not yet
    session_id: int = Form(123)
):
    try:
        # Store temp files
        with tempfile.NamedTemporaryFile(delete=False, suffix=".csv") as tmp_train:
            tmp_train.write(await train_file.read())
            path_train = tmp_train.name

        with tempfile.NamedTemporaryFile(delete=False, suffix=".csv") as tmp_test:
            tmp_test.write(await test_file.read())
            path_test = tmp_test.name

        # Read CSV files with flexible separator
        df_train = read_csv_flexible(path_train)
        df_test = read_csv_flexible(path_test)

        os.remove(path_train)
        os.remove(path_test)

        print(" Train columnns :", df_train.columns.tolist())
        print("Test columns :", df_test.columns.tolist())

        # Setup PyCaret
        clf_setup(data=df_train, target=target, session_id=session_id, html=False, verbose=False)
        model = clf_compare()


        # Predictions
        # Drop target column from test set if it exists
        if target in df_test.columns:
            df_test = df_test.drop(columns=[target])
        pred = predict_model(model, data=df_test)
        print("Prediction Exemple :", pred.head())
        print("Prediciton columns :", pred.columns.tolist())
        print("No of rows :", pred.shape[0])


        # Store in CSV format
        result_file = tempfile.NamedTemporaryFile(delete=False, suffix=".csv")
        pred.to_csv(result_file.name, index=False)
        print("Prediction file stored :", result_file.name)

        # Affiche son contenu brut pour être sûr :
        with open(result_file.name, "r") as f:
            print("Check Raw content of CSV file :\n", f.read())
        return FileResponse(path=result_file.name, media_type="text/csv", filename="predictions.csv")

    except Exception as e:
        return {"error": f"Erreur API (predict_model) : {str(e)}"}
    
    
