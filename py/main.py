from fastapi import FastAPI, UploadFile, File, Form
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import FileResponse, JSONResponse

import pandas as pd
import pycaret
from pycaret.clustering import setup as clu_setup, create_model as clu_create, pull as clu_pull
import tempfile
import os
import uvicorn
import base64
import io
import matplotlib
#matplotlib.use('Agg') 
import matplotlib.pyplot as plt
from io import BytesIO
#matplotlib.use("Agg")  # Important: désactive les fenêtres GUI
import matplotlib.pyplot as plt
import csv
from pycaret.classification import save_model

from pycaret.classification import (
    compare_models as clf_compare,
    setup as clf_setup,
    create_model as clf_create,
    evaluate_model as clf_evaluate,
    pull as clf_pull,
    plot_model,
    predict_model,
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
    analysis_type: str = Form("supervised")
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
        top_models = clf_compare(n_select=10)
        leaderboard = clf_pull() # important pour récupérer l'identifiant interne
        leaderboard = leaderboard.reset_index()  # assure que l'index est une colonne
        # L'index du leaderboard contient les model_ids
        if "index" in leaderboard.columns:
            leaderboard["model_id"] = leaderboard["index"].str.lower()
        else:
            # fallback au nom du modèle, peu fiable mais au cas où
            leaderboard["model_id"] = leaderboard["Model"].str.extract(r"^(\w+)").iloc[:,0].str.lower()
        print(leaderboard[["index", "Model"]])


        # Nettoyage
        os.remove(tmp_path)
        #return {
            #"type": "supervised",
         #   "best_model_name": str(top_models[0]),
          #  "leaderboard": leaderboard.to_dict(orient="records")
        #}
        return JSONResponse(content={
            "best_model_name": str(top_models[0]),
            "leaderboard": leaderboard.to_dict(orient="records")
        })

    elif analysis_type == "unsupervised":
        # Setup + create_model pour clustering (exemple: kmeans)
        clu_setup(data=df#, silent=True
                  , session_id=session_id)
        model = clu_create('kmeans')  # Tu peux aussi recevoir le nom du modèle via un paramètre
        clustering_result = clu_pull()
        # Nettoyage
        os.remove(tmp_path)
        return {
            #"type": "unsupervised",
            "best_model_name": str(top_models[0]),
            #"model_used": str(model),
            "clustering_result": clustering_result.to_dict(orient="records")
        }

    else:
        return {"error": "Invalid analysis type. Must be 'supervised' or 'unsupervised'."}




@app.post("/evaluate_model")
async def evaluate_model(
    file: UploadFile = File(...),
    target: str = Form(...),
    model_name: str = Form(...),
    session_id: int = Form(123)
):
    # Step 1 : Read data
    with tempfile.NamedTemporaryFile(delete=False, suffix=".csv") as tmp:
        contents = await file.read()
        tmp.write(contents)
        tmp_path = tmp.name

    df = pd.read_csv(tmp_path)
    os.remove(tmp_path)

    # Step 2 : Setup
    clf_setup(data=df, target=target, session_id=session_id#, silent=True
              , verbose=False)

    # Step 3 : Create and train model requested
    model = clf_create(model_name)
    
    import matplotlib.pyplot as plt
    import base64
    from io import BytesIO
    from pycaret.classification import interpret_model

    # Generation SHAP summary plot
    #plt.figure()
    #interpret_model(model, plot='summary')
    #buf = BytesIO()
    #plt.savefig(buf, format="png", bbox_inches="tight")
    #buf.seek(0)
    #shap_summary_base64 = base64.b64encode(buf.read()).decode('utf-8')
    #plt.close()


    # Step 4 : Get metrics
    metrics = clf_pull().to_dict(orient="records")
    
    # Step  : Save model choosen
    model_path = f"models/{model_name}"
    os.makedirs("models", exist_ok=True)
    save_model(model, model_path)


    # Step 5 : Generate plots (Matplotlib) => Need review choose may be seaborn or another library
    def save_plot_to_base64(plot_type):
            img_buf = io.BytesIO()
            try:
                plot_model(model, plot=plot_type, save=False)
                plt.savefig(img_buf, format='png')
                plt.close()
                img_buf.seek(0)
                return base64.b64encode(img_buf.read()).decode("utf-8")
            except:
                return None

    # I can add more plots if needed
    # Available plots: "auc", "confusion_matrix", "feature", "learning", "threshold", "pr", "boundary", "classification_report", "rfe", "manifold", "error", "calibration", "residuals", "vc", "feature_interaction"
    plots = {
        "confusion_matrix": save_plot_to_base64("confusion_matrix"),
        "roc_curve": save_plot_to_base64("auc"),
        "feature_importance": save_plot_to_base64("feature")
        #"shap_summary": None  # Valeur par défaut
    }

    return {
        "model_name": str(model),
        "metrics": metrics,
        "plots": plots,
        "message": f"Modèle '{model_name}' sauvegardé dans {model_path}.pkl"
    }


# Prediction endpoint
@app.post("/predict_model")
async def run_prediction(
    train_file: UploadFile = File(...),
    test_file: UploadFile = File(...),
    target: str = Form(...),
    model_name: str = Form(...),  # Pas encore utilisé ici
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
        print("prédiction Exemple :", pred.head())
        print("Prediciton columns :", pred.columns.tolist())
        print("No of rows :", pred.shape[0])


        # Store in CSV format
        result_file = tempfile.NamedTemporaryFile(delete=False, suffix=".csv")
        pred.to_csv(result_file.name, index=False)
        print("Prediction file stored :", result_file.name)

        # Affiche son contenu brut pour être sûr :
        with open(result_file.name, "r") as f:
            print("🔍 Check Raw content of CSV file :\n", f.read())
        return FileResponse(path=result_file.name, media_type="text/csv", filename="predictions.csv")

    except Exception as e:
        return {"error": f"Erreur API (predict_model) : {str(e)}"}
    
    
