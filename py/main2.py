from fastapi import FastAPI, UploadFile, File, Form
from fastapi.middleware.cors import CORSMiddleware
import pandas as pd
from pycaret.classification import setup as clf_setup, compare_models as clf_compare, pull as clf_pull
from pycaret.clustering import setup as clu_setup, create_model as clu_create, pull as clu_pull

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
    # Lire le CSV
    df = pd.read_csv(file.file)

    if analysis_type == "supervised":
        # Setup + compare_models pour apprentissage supervisé (classification ici)
        clf_setup(data=df, target=target, silent=True, session_id=session_id)
        top_models = clf_compare(n_select=10)
        leaderboard = clf_pull()
        return {
            "type": "supervised",
            "best_model_name": str(top_models[0]),
            "leaderboard": leaderboard.to_dict(orient="records")
        }

    elif analysis_type == "unsupervised":
        # Setup + create_model pour clustering (exemple: kmeans)
        clu_setup(data=df, silent=True, session_id=session_id)
        model = clu_create('kmeans')  # Tu peux aussi recevoir le nom du modèle via un paramètre
        clustering_result = clu_pull()
        return {
            "type": "unsupervised",
            "model_used": str(model),
            "clustering_result": clustering_result.to_dict(orient="records")
        }

    else:
        return {"error": "Invalid analysis type. Must be 'supervised' or 'unsupervised'."}
