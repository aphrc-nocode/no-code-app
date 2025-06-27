from fastapi import FastAPI, UploadFile, Form
from fastapi.middleware.cors import CORSMiddleware
import pandas as pd
from pycaret.classification import setup, compare_models, pull
import uvicorn
import os
import tempfile

app = FastAPI()

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"], allow_methods=["*"], allow_headers=["*"]
)

@app.post("/automl")
async def run_automl(file: UploadFile, target: str = Form(...)):
    # Lire le fichier temporairement
    with tempfile.NamedTemporaryFile(delete=False, suffix=".csv") as tmp:
        contents = await file.read()
        tmp.write(contents)
        tmp_path = tmp.name

    df = pd.read_csv(tmp_path)

    # AutoML avec PyCaret
    setup(data=df, target=target,  session_id=123)
    best_model = compare_models(sort="Accuracy", n_select=5)
    leaderboard = pull()

    # Nettoyage
    os.remove(tmp_path)

    return leaderboard.to_dict(orient="records")
