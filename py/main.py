# -------------------------------
# Standard library
# -------------------------------
import os, re, csv, io, json, math, time, shutil, tempfile
import base64
from pathlib import Path
from typing import Optional, Dict, Any, List
from concurrent.futures import ThreadPoolExecutor
from io import BytesIO 
import uvicorn, multiprocessing, time, socket


from pydantic import BaseModel
from scipy import sparse
import shap

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

# -------------------------------
# FastAPI & responses
# -------------------------------
from fastapi import FastAPI, UploadFile, File, Form, Body, HTTPException, Request, Query
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import FileResponse, JSONResponse

# -------------------------------
# Core scientific stack
# -------------------------------
import numpy as np
import pandas as pd

# Optional
import binascii
import uvicorn


from sklearn.metrics import (
    accuracy_score, f1_score, precision_score, recall_score,
    roc_auc_score, log_loss, confusion_matrix, roc_curve, auc,
    cohen_kappa_score, matthews_corrcoef,
)


# -------------------------------
# PyCaret
# -------------------------------

from pycaret.classification import (
    setup as clf_setup,
    compare_models as clf_compare,
    create_model as clf_create,
    predict_model as clf_predict,
    pull as clf_pull,
    models as clf_models,          # list of models and codes
    plot_model as clf_plot,
    get_config as clf_get_config,   # <<— IMPORTANT
    evaluate_model as clf_evaluate,
    interpret_model as clf_interpret,
    save_model, load_model, get_config,
    finalize_model as clf_finalize,
)

# Optional: clustering (remove if you don't use it)
from pycaret.clustering import (
    setup as clu_setup,
    create_model as clu_create,
    pull as clu_pull,
)

from typing import Optional, Dict, Any, List

# Project root directory (where this file is located)
BASE_DIR = os.path.dirname(os.path.abspath(__file__)) 
MODELS_DIR = os.path.join(BASE_DIR, "..", "models")
MODELS_DIR = os.path.abspath(MODELS_DIR)


REG_PATH = os.path.join("logs", "deployments.json")

def _unwrap_estimator(model):
    from sklearn.pipeline import Pipeline
    base = model
    if isinstance(base, Pipeline):
        base = base.steps[-1][1]
    if hasattr(base, "base_estimator_"):  # CalibratedClassifierCV
        base = base.base_estimator_
    if hasattr(base, "base_estimator"):
        base = base.base_estimator
    if hasattr(base, "estimator"):
        base = base.estimator
    return base


# ---- helpers ready/stop checks ----
import socket, http.client

def _wait_http_ok(host: str, port: int, path: str = "/openapi.json", timeout_s: float = 6.0) -> bool:
    """Wait for GET path to respond with 200 OK (Swagger ready)."""
    t0 = time.time()
    while time.time() - t0 < timeout_s:
        try:
            conn = http.client.HTTPConnection(host, port, timeout=1.2)
            conn.request("GET", path)
            resp = conn.getresponse()
            ok = (resp.status == 200)
            conn.close()
            if ok: return True
        except Exception:
            time.sleep(0.2)
    return False

def _port_is_closed(port: int) -> bool:
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.settimeout(0.5)
    try:
        return s.connect_ex(("127.0.0.1", port)) != 0
    finally:
        s.close()



def _is_valid_b64(s: str) -> bool:
    # lowered threshold (1000 was too strict for small images)
    if not isinstance(s, str) or len(s) < 100:
        return False
    try:
        base64.b64decode(s, validate=True)
        return True
    except Exception:
        return False

def fig_to_base64(fig, dpi=150):
    buf = io.BytesIO()
    fig.savefig(buf, format="png", bbox_inches="tight", dpi=dpi)
    plt.close(fig)
    return base64.b64encode(buf.getvalue()).decode()

def _b64(path: str) -> str | None:
    if not os.path.exists(path):
        return None
    with open(path, "rb") as f:
        return base64.b64encode(f.read()).decode()


def _is_valid_b64(s):
    if not isinstance(s, str) or len(s) < 1000:
        return False
    try:
        base64.b64decode(s, validate=True)
        return True
    except Exception:
        return False

def fig_to_base64(fig, dpi=150):
    buf = io.BytesIO()
    fig.savefig(buf, format="png", bbox_inches="tight", dpi=dpi)
    plt.close(fig)
    return base64.b64encode(buf.getvalue()).decode()


def _read_reg() -> Dict[str, Any]:
    if os.path.exists(REG_PATH):
        with open(REG_PATH, "r", encoding="utf-8") as f:
            return json.load(f)
    return {"items": []}

def _write_reg(d: Dict[str, Any]) -> None:
    os.makedirs(os.path.dirname(REG_PATH), exist_ok=True)
    with open(REG_PATH, "w", encoding="utf-8") as f:
        json.dump(d, f, indent=2)

def _upsert_item(it: Dict[str, Any]) -> None:
    reg = _read_reg()
    by_id = {x["model_id"]: x for x in reg["items"]}
    by_id[it["model_id"]] = it
    reg["items"] = list(by_id.values())
    _write_reg(reg)

def _remove_item(model_id: str) -> None:
    reg = _read_reg()
    reg["items"] = [x for x in reg["items"] if x.get("model_id") != model_id]
    _write_reg(reg)

def fig_to_base64(fig, dpi=150):
    buf = io.BytesIO()
    fig.savefig(buf, format="png", bbox_inches="tight", dpi=dpi)
    plt.close(fig)
    return base64.b64encode(buf.getvalue()).decode()


def _b64(path: str):
    if not os.path.exists(path):
        return None
    with open(path, "rb") as f:
        return base64.b64encode(f.read()).decode()

import base64, binascii

def _is_valid_b64(s: str) -> bool:
    try:
        if not isinstance(s, str) or not s:
            return False
        base64.b64decode(s, validate=True)
        return True
    except (binascii.Error, ValueError, TypeError):
        return False


def shap_simple_b64(model):
    """
    SHAP universel (Explainer 'permutation') – compact output in BAR PLOT (Top-N).
    """
    try:
        import shap, numpy as np, pandas as pd
        from scipy import sparse
        import matplotlib.pyplot as plt

        # ---------- Transformed data (PyCaret pipeline) ----------
        X = get_config("X_train_transformed")
        if X is None:
            return None

        # Sample & densify (to stay fast)
        if sparse.issparse(X):
            X = X.tocsr()
            n = min(200, X.shape[0])
            Xs = pd.DataFrame(X[:n].toarray())
        elif isinstance(X, pd.DataFrame):
            Xs = X.sample(min(200, len(X)), random_state=42) if len(X) > 200 else X.copy()
        else:
            X = np.asarray(X)
            Xs = pd.DataFrame(X[: min(200, X.shape[0])])

        # ---------- Prediction function ----------
        f = model.predict_proba if hasattr(model, "predict_proba") else model.predict

        # ---------- Explainer permutation ----------
        masker = shap.maskers.Independent(Xs)
        explainer = shap.Explainer(f, masker, algorithm="permutation")
        sv = explainer(Xs)  # shap.Explanation

        # ---------- Bar plot (Top-N) propre ----------
        topn = min(20, Xs.shape[1])   # adjust the “20” if you want more/fewer variables

        # Compact size & legible font
        plt.figure(figsize=(7.5, 5.2), dpi=120)
        plt.rcParams.update({"font.size": 9})

        # Bar chart: easier to read than the beeswarm chart
        shap.summary_plot(
            sv,
            Xs,
            show=False,
            plot_type="bar",
            max_display=topn
        )

        # Optimized backup (uses your existing helper _fig_to_b64_scaled)
        return _fig_to_b64_scaled(max_width_px=900, dpi_save=120)

    except Exception:
        return None


def _models_table():
    """
    Returns a DataFrame with standardized columns:
    [‘model_id’, ‘Model’] regardless of the format returned by PyCaret.
    """
    md = clf_models().reset_index()  # different versions => variable columns
    cols = {c.lower(): c for c in md.columns}

    # id column (pycaret short code)
    if 'id' in cols:
        id_col = cols['id']
    elif 'index' in md.columns:
        id_col = 'index'     # sometimes the ID is in the reset index
    else:
        id_col = md.columns[0]  # fallback

    # readable label column
    if 'model' in cols:
        name_col = cols['model']
    elif 'name' in cols:
        name_col = cols['name']
    elif 'estimator' in cols:
        name_col = cols['estimator']
    else:
        # no label? duplicate the ID
        md['Model'] = md[id_col].astype(str)
        name_col = 'Model'

    md = md.rename(columns={id_col: 'model_id', name_col: 'Model'})
    return md[['model_id', 'Model']]

def _extract_base_estimator(model):
    # Try to retrieve the underlying estimator if the model is wrapped
    for attr in ("base_estimator", "estimator", "model", "classifier"):
        if hasattr(model, attr):
            try:
                return getattr(model, attr)
            except Exception:
                pass
    return model

def _pretty_model_name(model):
    base = _extract_base_estimator(model)
    name = type(base).__name__
    # CamelCase -> "Camel Case"
    pretty = re.sub(r'(?<!^)(?=[A-Z])', ' ', name).strip()
    # Common corrections
    pretty = (pretty
              .replace("S V C", "SVC")
              .replace("K Neighbors", "k-Nearest Neighbors")
              .replace("X G Boost", "XGBoost")
              .replace("L G B M Classifier", "LightGBM")
              .replace("Cat Boost Classifier", "CatBoost"))
    return pretty


def _resolve_model_id(code: str | None, label: str | None):
    """
    Robustly solves a PyCaret model code:
    - accepts code or label (case insensitive),
    - handles common aliases (svm/lsvm, rf/random_forest, dt/decision_tree, etc.).
    """
    # Official table from PyCaret
    md = _models_table()
    ids_lower = {str(x).lower(): str(x) for x in md['model_id'].astype(str)}
    name_to_id = {str(r['Model']).strip().lower(): str(r['model_id'])
                  for _, r in md.iterrows()}

    # common aliases -> canonical code
    ALIAS = {
        "lr": ["lr", "logistic", "logisticregression", "logistic_regression"],
        "ridge": ["ridge", "ridgeclassifier", "ridge_classifier", "rc"],
        "lda": ["lda", "lineardiscriminantanalysis"],
        "qda": ["qda", "quadraticdiscriminantanalysis"],
        "nb": ["nb", "naive_bayes", "naivebayes"],
        "knn": ["knn", "knearest", "k_nearest_neighbors", "kneighbors"],
        "dt": ["dt", "decisiontree", "decision_tree"],
        "rf": ["rf", "randomforest", "random_forest"],
        "et": ["et", "extratrees", "extra_trees"],
        "gbc": ["gbc", "gradientboosting", "gradient_boosting"],
        "ada": ["ada", "adaboost", "ada_boost"],
        "svm": ["svm", "lsvm", "linear_svm", "svm_linear"],
        "rbfsvm": ["rbfsvm", "svm_rbf", "svm-rbf"],
        "lightgbm": ["lightgbm", "lgbm"],
        "xgboost": ["xgboost", "xgb"],
        "catboost": ["catboost", "cb"]
    }
    alias_lookup = {alias: canon for canon, aliases in ALIAS.items() for alias in aliases}

    # 1) If a code is provided, standardize + verify
    if code:
        c = str(code).strip().lower().strip('"').strip("'")
        # if we receive “rf_1”, extract the prefix
        import re
        m = re.match(r"([a-z]+)", c)
        if m:
            c = m.group(1)
        c = alias_lookup.get(c, c)  # alias -> canon
        # case-insensitive matching
        if c in ids_lower:
            return ids_lower[c]
        # last resort: try create_model directly
        try:
            _ = clf_create(c)
            return c
        except Exception:
            pass

    # 2) If a label is provided, resolve by the displayed name
    if label:
        lab = str(label).strip().lower()
        if lab in name_to_id:
            return name_to_id[lab]

    raise HTTPException(status_code=400, detail=f"Unknown model '{code or label}'")


def shap_summary_b64(model, max_rows_bg=400, max_rows_plot=600, max_display=20):
    # 1) Retrieve X transformed otherwise X
    X = None
    for key in ("X_train_transformed", "X_transformed", "X_train", "X"):
        try:
            X = clf_get_config(key)
            if X is not None:
                break
        except Exception:
            pass
    if X is None:
        return None, "no_X_in_config"

    # 2) DataFrame dense & sampling
    if sparse.issparse(X):
        X = X.tocsr()
        n = min(X.shape[0], max_rows_plot)
        Xt = pd.DataFrame(X[:n].toarray())
        dbg = ["sparse->dense(sample)"]
    elif isinstance(X, pd.DataFrame):
        Xt = X.copy()
        dbg = ["df"]
        if Xt.shape[0] > max_rows_plot:
            Xt = Xt.sample(max_rows_plot, random_state=42)
            dbg.append("sample")
        # non-numeric columns -> cast where possible
        for c in Xt.columns:
            if Xt[c].dtype == "object":
                try:
                    Xt[c] = pd.to_numeric(Xt[c], errors="coerce")
                except Exception:
                    pass
        Xt = Xt.fillna(0)
    else:
        X = np.asarray(X)
        n = min(X.shape[0], max_rows_plot)
        Xt = pd.DataFrame(X[:n])
        dbg = ["ndarray->df(sample)"]

    Xt_bg = Xt.sample(min(len(Xt), max_rows_bg), random_state=42)

    # 3) Choix explainer
    base = _unwrap_estimator(model)
    cname = type(base).__name__.lower()
    try:
        if any(k in cname for k in ("forest","tree","gradientboost","xgb","lightgbm","catboost","adaboost","extra")):
            expl = shap.TreeExplainer(base)
            sv = expl.shap_values(Xt_bg)
            if isinstance(sv, list):
                try:
                    sv = np.stack([np.asarray(s) for s in sv], axis=0).mean(axis=0)
                    dbg.append("tree:multiclass-mean")
                except Exception:
                    sv = sv[0]
                    dbg.append("tree:first-class")
        elif any(k in cname for k in ("logistic","linear","ridge","lasso","linearsvc","sgd")):
            expl = shap.LinearExplainer(base, Xt_bg)
            sv = expl.shap_values(Xt_bg)
            dbg.append("linear")
        else:
            # Fallback universel (SVM, KNN, NaiveBayes, etc.)
            f_pred = None
            if hasattr(base, "predict_proba"):
                f_pred = base.predict_proba
            else:
                f_pred = base.predict
            expl = shap.KernelExplainer(f_pred, shap.sample(Xt_bg, min(100, len(Xt_bg)), random_state=42))
            sv = expl.shap_values(shap.sample(Xt, min(200, len(Xt)), random_state=42))
            if isinstance(sv, list):
                # positive class if available, otherwise first
                sv = sv[1] if len(sv) > 1 else sv[0]
            dbg.append("kernel")

        # 4) Explicit figure
        fig, ax = plt.subplots(figsize=(8, 5), dpi=150)
        shap.summary_plot(sv, Xt_bg, show=False, max_display=max_display, plot_type="dot")
        return fig_to_base64(fig), ";".join(dbg)

    except Exception as e:
        return None, f"shap_fail:{type(e).__name__}:{str(e)[:160]}"


# --- ADD this helper just BEFORE reading/filtering ids ---
def _parse_ids_robust(raw):
    """
    Accepte:
      - '["lr","rf"]'   (JSON array)
      - '"[\"lr\",\"rf\"]"' (JSON doublement encodé)
      - ['lr','rf']     (python list-like mise en string)
      - 'lr'            (single code)
    Retourne: ['lr','rf'] nettoyé.
    """
    if raw is None:
        return []
    s = str(raw).strip()
    # 1) Try JSON
    try:
        obj = json.loads(s)
    except Exception:
        obj = s
    # 2) if it's still a string that looks like a JSON array → re-parse
    if isinstance(obj, str):
        t = obj.strip()
        if (t.startswith("[") and t.endswith("]")) or (t.startswith('"[') and t.endswith(']"')):
            try:
                obj = json.loads(t.strip('"'))
            except Exception:
                obj = [t]
    # 3) if it is not a list → make it a list with 1 item
    if not isinstance(obj, list):
        obj = [obj]
    # 4) cleaning element by element
    out = []
    for x in obj:
        if x is None:
            continue
        sx = str(x).strip().strip('"').strip("'")
        # remove residual brackets of type “[lr]” -> “lr”
        if sx.startswith("[") and sx.endswith("]") and "," not in sx:
            sx = sx[1:-1].strip()
        if sx:
            out.append(sx)
    return out


# Fonction for cleaning JSON data
# This function will replace NaN and Inf values with None

def clean_json(obj):
    """Replaces NaN/Inf with None recursively for JSON."""
    if isinstance(obj, dict):
        return {k: clean_json(v) for k, v in obj.items()}
    elif isinstance(obj, list):
        return [clean_json(x) for x in obj]
    elif isinstance(obj, float):
        if math.isnan(obj) or math.isinf(obj):
            return None
        return obj
    return obj


def read_csv_flexible(path):
    """Try to read fole with comma (,) or semi comma(;) if one column was detected """
    df = pd.read_csv(path)
    if len(df.columns) == 1:
        df = pd.read_csv(path, sep=';')
    return df


app = FastAPI()

@app.get("/health")
def health():
    return {"status": "ok"}


# Enable CORS if necessary for Shiny
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # Replace “*” with the URL of your Shiny app if you want to restrict access.
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.post("/automl")
async def automl(
    file: UploadFile = File(...),
    target: str = Form(...),
    session_id: int = Form(123),
    analysis_type: str = Form("supervised"),
    train_size: float = Form(0.8)
):
    # Read uploaded CSV
    with tempfile.NamedTemporaryFile(delete=False, suffix=".csv") as tmp:
        tmp.write(await file.read())
        tmp_path = tmp.name
    df = read_csv_flexible(tmp_path)
    os.remove(tmp_path)

    if analysis_type != "supervised":
        return JSONResponse(status_code=400, content={"error": "Only 'supervised' supported here."})

    # Setup + compare (we want ALL possible PyCaret models for the installed version)
    clf_setup(
        data=df,
        target=target,
        session_id=int(session_id),
        train_size=float(train_size),
        fold=5,
        html=False,
        verbose=False
    )
    clf_compare(turbo=False)  # trains and fills the pull()

    # Leaderboard CV
    lb = clf_pull().reset_index(drop=True)

    # === Build a RELIABLE label -> code mapping based on the installed version of PyCaret ===
    # Official table of models (codes + labels) for YOUR version
    md = _models_table()  # returns columns [‘model_id’,'Model'] (already defined above)
    # Dictionary label(lower) -> code
    name2code = {str(r["Model"]).strip().lower(): str(r["model_id"]).strip()
                 for _, r in md.iterrows()}

    # Harmonize the ‘Model’ column of the leaderboard
    if "Model" not in lb.columns:
        # Some versions use different names; at worst, we put a default label.
        lb["Model"] = lb.get("model", lb.get("model_name", "Unknown"))

    # For each line in the leaderboard, associate the code via name2code
    codes = []
    for nm in lb["Model"].astype(str).tolist():
        key = nm.strip().lower()
        codes.append(name2code.get(key, None))  # None if not found
    lb["model_id"] = codes  # <-- HERE: we do not add suffixes _1, _2, etc. We keep the actual code.

    # Cleaning: we also keep a dict label->code for the selector on the R side
    model_map = {}
    for nm, code in zip(lb["Model"].astype(str).tolist(), lb["model_id"].tolist()):
        if code is not None and str(code):
            model_map[str(nm)] = str(code)

    payload = {
        "leaderboard": lb.to_dict(orient="records"),
        "models": model_map  # expected on the R side: names=labels, values=codes
    }
    return JSONResponse(content=clean_json(payload))


# Model evaluation endpoint
@app.post("/evaluate_model")
async def evaluate_model(
    file: UploadFile = File(...),
    target: str = Form(...),
    model_id: str | None = Form(None),
    model_name: str | None = Form(None),
    session_id: int = Form(123),
    train_size: float = Form(0.8)
):
    # Read CSV
    with tempfile.NamedTemporaryFile(delete=False, suffix=".csv") as tmp:
        tmp.write(await file.read())
        path = tmp.name
    df = pd.read_csv(path)
    os.remove(path)

    # re-setup
    clf_setup(
        data=df,
        target=target,
        session_id=session_id,
        train_size=float(train_size),
        fold=5,
        html=False,
        verbose=False
    )

    # model code (robust)
    code = _resolve_model_id(model_id, model_name)

    # fit + predict
    model = clf_create(code)
    pred = clf_predict(model, data=df)  # also calculates metrics -> pull()
    metrics = clf_pull().to_dict(orient="records")

    # y_true / y_pred / y_score
    y_true = df[target]
    y_pred = None
    for col in ['Label', 'prediction_label', 'prediction', 'predicted_label']:
        if col in pred.columns:
            y_pred = pred[col]
            break
    if y_pred is None:
        raise HTTPException(status_code=500, detail="No prediction label column in predict_model output.")

    y_score = None
    for col in ['prediction_score', 'Score', 'Probability_1', 'Probability']:
        if col in pred.columns:
            y_score = pred[col]
            break

    # Confusion
    try:
        cm = confusion_matrix(y_true, y_pred).tolist()
    except Exception:
        cm = None

    # ROC (if binary + proba)
    roc_data = None
    try:
        if y_score is not None and len(np.unique(y_true)) == 2:
            fpr, tpr, thresholds = roc_curve(y_true, y_score)
            roc_data = {"fpr": fpr.tolist(), "tpr": tpr.tolist(),
                        "thresholds": thresholds.tolist(), "auc": float(auc(fpr, tpr))}
    except Exception:
        roc_data = None

    # Plots recorded by PyCaret (encoded in base64)
    plots = {}
    debug={}
    
    try:
        clf_plot(model, plot='auc', save=True)
        plots["roc"] = _b64("AUC.png")
    except Exception:
        pass
    try:
        clf_plot(model, plot='confusion_matrix', save=True)
        plots["confusion"] = _b64("Confusion Matrix.png")
    except Exception:
        pass
    try:
        clf_plot(model, plot='feature', save=True)
        plots["importance"] = _b64("Feature Importance.png")
    except Exception:
        pass
        # ---- SHAP summary: first PyCaret interpret_model, otherwise fallback shap_summary_b64 ----
        # ---- SHAP summary: first via PyCaret, otherwise in-house fallback ----
# ---- SHAP summary: PyCaret then in-house fallback ----
    try:
        shap_b64 = None
        shap_dbg = []

        # 1) PyCaret attempt (file)
        try:
            if os.path.exists("SHAP Summary Plot.png"):
                os.remove("SHAP Summary Plot.png")
        except Exception:
            pass
        try:
            clf_interpret(model, plot='summary', save=True)
            shap_b64 = _b64("SHAP Summary Plot.png")
            if _is_valid_b64(shap_b64):
                shap_dbg.append("pycaret_interpret_model")
        except Exception as e:
            shap_dbg.append(f"interpret_fail:{type(e).__name__}:{str(e)[:120]}")

        # 2) in-house fallback
        if not _is_valid_b64(shap_b64):
            b64_img, dbg = shap_summary_b64(model)
            shap_b64 = b64_img
            if dbg: shap_dbg.append(dbg)

        if _is_valid_b64(shap_b64):
            plots["shap_summary"] = shap_b64
        else:
            debug["shap"] = {"notes": shap_dbg}
    except Exception as e:
        debug["shap_error"] = f"{type(e).__name__}:{str(e)[:160]}"


    # Saving the template (for later deployment)
    save_model(model, os.path.join(MODELS_DIR, code))

    resp = {
        "metrics": metrics,     # list[dict]
        "plots": plots,         # dict of base64
        "extras": {
            "confusion_matrix": cm,    # list[list] or None
            "roc_curve": roc_data,      # dict or None
            "debug":debug
        }
    }
    return JSONResponse(content=clean_json(resp))



# --- Robust helpers for PyCaret code ---

def _available_model_codes():
    """Retourne (codes_disponibles: set[str], mapping_nom->code)."""
    md = _models_table()
    codes = set(md["model_id"].astype(str).str.lower().tolist())
    name_to_id = {str(r["Model"]).strip().lower(): str(r["model_id"])
                  for _, r in md.iterrows()}
    return codes, name_to_id

# Common aliases -> list of tests (from most likely to least likely)
ALIAS_TRIALS = {
    "lr":      ["lr", "logistic", "logisticregression", "logistic_regression"],
    "ridge":   ["ridge", "ridgeclassifier", "ridge_classifier", "rc"],
    "lda":     ["lda", "lineardiscriminantanalysis"],
    "qda":     ["qda", "quadraticdiscriminantanalysis"],
    "nb":      ["nb", "naive_bayes", "naivebayes"],
    "knn":     ["knn", "kneighbors", "knearest", "k_nearest_neighbors"],
    "dt":      ["dt", "decisiontree", "decision_tree"],
    "rf":      ["rf", "randomforest", "random_forest"],
    "et":      ["et", "extratrees", "extra_trees"],
    "gbc":     ["gbc", "gradientboostingclassifier", "gradientboosting", "gradient_boosting"],
    "ada":     ["ada", "adaboost", "ada_boost"],
    "svm":     ["svm", "lsvm", "linear_svm", "svm_linear"],
    "rbfsvm":  ["rbfsvm", "svm_rbf", "svm-rbf"],
    "lightgbm":["lightgbm", "lgbm"],
    "xgboost": ["xgboost", "xgb"],
    "catboost":["catboost", "cb"],
}

def _base_from_id(s: str) -> str:
    """'rf' ou 'rf_1' -> 'rf' (basic cleaning)."""
    import re
    ss = str(s).strip().strip('"').strip("'").lower()
    m = re.match(r"([a-z]+)", ss)
    return m.group(1) if m else (ss.split("_", 1)[0] if "_" in ss else ss)

def _canonical_code_or_raise(code_or_label: str) -> str:
    """
    Try successively: code/label as is, known aliases, then return the first code
    for which create_model(...) works. Raise if nothing works.
    """
    # 1) short base since the id (rf_1 -> rf)
    base = _base_from_id(code_or_label)

    #2) Test list: base + aliases + simple variations
    trials = []
    # a) predefined aliases (if known)
    trials.extend(ALIAS_TRIALS.get(base, []))
    # b) basic variations
    if base not in trials:
        trials.insert(0, base)
    if base.replace("_", "") not in trials:
        trials.append(base.replace("_", ""))
    if base.replace("-", "") not in trials:
        trials.append(base.replace("-", ""))

    # 3) Finally, if the complete label resembles a model name, we will attempt resolution via the table.
    codes_available, name_to_id = _available_model_codes()

    #4) Try each candidate in order
    last_exc = None
    for cand in trials:
        c = cand.strip().lower()
        # if the table declares exactly this code, we try it
        if c in codes_available:
            try:
                _ = clf_create(c)
                return c
            except Exception as e:
                last_exc = e
                continue
        # otherwise, we try directly (some versions do not expose all aliases)
        try:
            _ = clf_create(c)
            return c
        except Exception as e:
            last_exc = e
            continue

    #5) Last chance: resolution by full label if known
    lab = str(code_or_label).strip().lower()
    if lab in name_to_id:
        c = name_to_id[lab].lower()
        try:
            _ = clf_create(c)
            return c
        except Exception as e:
            last_exc = e

    # Nothing worked
    raise HTTPException(status_code=400, detail=f"Unknown/unsupported model '{code_or_label}' ({type(last_exc).__name__ if last_exc else 'NoTrial'})")



@app.post("/test_leaderboard")
async def test_leaderboard(
    file: UploadFile = File(...),
    target: str = Form(...),
    session_id: int = Form(123),
    train_size: float = Form(0.8),
    model_ids: str = Form(None)   # JSON: ["lr","ridge","rf", ...] (codes)
):
    import json

    try:
        # 1) Read CSV
        with tempfile.NamedTemporaryFile(delete=False, suffix=".csv") as tmp:
            tmp.write(await file.read())
            p = tmp.name
        df = read_csv_flexible(p)
        os.remove(p)

        # 2) Re-setup (same split as TRAIN)
        clf_setup(
            data=df,
            target=target,
            session_id=int(session_id),
            train_size=float(train_size),
            fold=5,
            html=False,
            verbose=False
        )

        # 3) Decode the requested IDs
        ids = _parse_ids_robust(model_ids)

        # 4) Codes actually available in your version
        md = _models_table()
        available_codes = [str(x).strip() for x in md["model_id"].astype(str).tolist()]
        available_set   = set([c.lower() for c in available_codes])

        # normalize received IDs and filter
        req_norm = [str(c).strip().strip('"').strip("'").lower() for c in ids]
        keep     = [c for c in req_norm if c in available_set]
        dropped  = [c for c in req_norm if c not in available_set]

        if not keep:
            return JSONResponse(content=clean_json({
                "test_leaderboard": [],
                "debug": {
                    "reason": "no_supported_models",
                    "received_ids": ids,
                    "unsupported_received": dropped,
                    "available_codes": available_codes
                }
            }))


        # if nothing to evaluate → clearly explain what to send
        if not keep:
            return JSONResponse(content=clean_json({
                "test_leaderboard": [],
                "debug": {
                    "reason": "no_supported_models",
                    "received_ids": ids,
                    "unsupported_received": dropped,
                    "available_codes": available_codes
                }
            }))

        # 6) TEST set
        X_test = get_config("X_test")
        y_test = get_config("y_test")
        if isinstance(X_test, pd.DataFrame) and target in X_test.columns:
            X_test = X_test.drop(columns=[target])

        # 7) mapping code -> better name
        code2name = {str(r["model_id"]).strip(): str(r["Model"]).strip() for _, r in md.iterrows()}

        rows, skipped = [], []

        def _err_str(ex: Exception, maxlen: int = 220):
            s = f"{type(ex).__name__}: {str(ex)}"
            return s[:maxlen]

        for code_l in keep:
            # restore the damage/original as listed by PyCaret if available
            code = next((c for c in available_codes if c.lower() == code_l), code_l)
            try:
                m = clf_create(code)
            except Exception as ex:
                skipped.append({"id": code, "stage": "create_model", "reason": _err_str(ex)})
                continue

            try:
                m_final = clf_finalize(m)
                t0 = time.perf_counter()
                preds = clf_predict(m_final, data=X_test, raw_score=True)
                elapsed = time.perf_counter() - t0

                # y_pred
                if 'prediction_label' in preds.columns:
                    y_pred = preds['prediction_label']
                elif 'Label' in preds.columns:
                    y_pred = preds['Label']
                elif 'prediction' in preds.columns:
                    y_pred = preds['prediction']
                else:
                    y_pred = preds.iloc[:, -1]

                # proba binary
                y_prob = preds['prediction_score'] if 'prediction_score' in preds.columns else None

                acc   = accuracy_score(y_test, y_pred)
                rec_w = recall_score(y_test, y_pred, average="weighted",  zero_division=0)
                pre_w = precision_score(y_test, y_pred, average="weighted", zero_division=0)
                f1_w  = f1_score(y_test, y_pred,    average="weighted")
                kap   = cohen_kappa_score(y_test, y_pred)
                mcc   = matthews_corrcoef(y_test, y_pred)

                auc_val = None
                try:
                    if y_prob is not None and pd.Series(y_test).nunique() == 2:
                        classes = sorted(pd.Series(y_test).unique().tolist())
                        pos_class = classes[-1]
                        y_bin = (pd.Series(y_test) == pos_class).astype(int)
                        auc_val = roc_auc_score(y_bin, y_prob)
                except Exception:
                    auc_val = None

                rows.append({
                    "Model": code2name.get(code, code),
                    "n_test": int(len(y_test)),
                    "Accuracy": float(acc),
                    "AUC": (None if auc_val is None else float(auc_val)),
                    "Recall": float(rec_w),
                    "Prec.": float(pre_w),
                    "F1": float(f1_w),
                    "Kappa": float(kap),
                    "MCC": float(mcc),
                    "TT (Sec)": float(round(elapsed, 3))
                })
            except Exception as ex:
                skipped.append({"id": code, "stage": "evaluate", "reason": _err_str(ex)})

        if not rows:
            return JSONResponse(content=clean_json({
                "test_leaderboard": [],
                "debug": {
                    "received_ids": ids,
                    "unsupported_received": dropped,
                    "available_codes": available_codes,
                    "skipped": skipped
                }
            }))

        rows = sorted(rows, key=lambda r: (r["Accuracy"] if r["Accuracy"] is not None else -1), reverse=True)
        return JSONResponse(content=clean_json({"test_leaderboard": rows}))

    except Exception as e:
        return JSONResponse(status_code=500, content={"error": str(e)})

from pydantic import BaseModel

class DeployPayload(BaseModel):
    model_id: str

# Resolve absolute models directory: <repo_root>/models next to this file
BASE_DIR = Path(__file__).resolve().parent
MODELS_DIR = Path(os.getenv("MODELS_DIR", BASE_DIR.parent / "models")).resolve()
# If your main.py is already at repo root, use:
# MODELS_DIR = Path(os.getenv("MODELS_DIR", BASE_DIR / "models")).resolve()

def safe_model_id(s: str) -> str:
    # keep simple ids like rf, lr, etc.
    s = s.strip()
    if not re.fullmatch(r"[A-Za-z0-9_\-\.]+", s):
        raise HTTPException(status_code=422, detail="Invalid model_id.")
    return s


def _public_scheme(req: Request) -> str:
    # allows you to override via env if you are behind a reverse proxy in HTTPS
    return os.getenv("PUBLIC_SCHEME") or req.headers.get("x-forwarded-proto") or req.url.scheme or "http"

def _public_host(req: Request) -> str:
    # priority order: env > X-Forwarded-Host > Host > Client IP
    override = os.getenv("PUBLIC_HOST")
    if override: 
        return override.strip()

    xf = req.headers.get("x-forwarded-host")
    if xf:
        return xf.split(",")[0].strip()

    host = req.headers.get("host")
    if host:
        # remove any port present in Host
        return host.split(":")[0]

    return req.client.host  # fallback

def _make_public_url(req: Request, port: int, path: str) -> str:
    scheme = _public_scheme(req)
    host   = _public_host(req)
    return f"{scheme}://{host}:{port}{path}"

def _rewrite_to_public(req: Request, url: str) -> str:
    """If a URL begins with http://127.0.0.1:<port>, replace it with the public host."""
    if not url:
        return url
    m = re.match(r"^http://127\.0\.0\.1:(\d+)(/.*)$", url)
    if not m:
        return url
    port, path = int(m.group(1)), m.group(2)
    return _make_public_url(req, port, path)



REGISTRY = {}  # model_id -> {"port": int, "proc": Process, "status": "Deployed"/"Stopped"}

def find_free_port(start=8001, end=8999):
    for p in range(start, end+1):
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            if s.connect_ex(("127.0.0.1", p)) != 0:
                return p
    raise RuntimeError("No free port found")

def serve_model(port: int, model_id: str):
    """
    Launch a FastAPI sub-server dedicated to a PyCaret model.
    - model_id is an opaque key (e.g., “lr__ma_session”). We deduce the code (“lr”) from it.
    - We load the model at boot time and expose /health, / (-> /docs), /predict.
    """
    # --- Deduce the “code” from the composite key ---
    code = model_id.split("__", 1)[0] if "__" in model_id else model_id

    # --- Local imports to avoid parent-side collisions ---
    from fastapi import FastAPI, UploadFile, File, Form, HTTPException
    from fastapi.middleware.cors import CORSMiddleware
    from fastapi.responses import FileResponse, RedirectResponse
    import pandas as pd, tempfile, io, json

    # --- FastAPI sub-server app ---
    sub = FastAPI(title=f"Model {model_id}", version="1.0.0")
    sub.add_middleware(CORSMiddleware, allow_origins=["*"], allow_methods=["*"], allow_headers=["*"])

    @sub.get("/")
    def root():
        # Open Swagger directly
        return RedirectResponse(url="/docs")

    # --- Load the model once at boot ---
    #    Try classification first, otherwise regression.
    model = None
    is_classification = True
    try:
        try:
            from pycaret.classification import load_model as _load_model_cls
            _ = _load_model_cls  # silence linter
            is_classification = True
            load_model_fn = _load_model_cls
        except Exception:
            from pycaret.regression import load_model as _load_model_reg
            is_classification = False
            load_model_fn = _load_model_reg

        # PyCaret 3: accepts “basename” without .pkl
        model_path_base = str((MODELS_DIR / code).resolve())
        model = load_model_fn(model_path_base)
    except Exception as e:
        # In case of clear failure, we raise: the parent will cut the process
        raise RuntimeError(f"Failed to load model '{code}': {e}")

    @sub.get("/health")
    def health():
        return {"ok": True, "model_id": model_id, "code": code, "is_classification": is_classification}

    @sub.post("/predict")
    async def predict(
        file: UploadFile = File(...)
    ):
        """
        Upload CSV -> CSV predictions (without target).
        - Robust reading (separators , ; \t |, BOM, common encodings)
        - Adds columns returned by PyCaret predict_model to the input CSV
        - Returns a downloadable CSV file
        """
        import json

        # --- helpers ---
        def read_csv_flexible_bytes(raw: bytes):
            from io import BytesIO
            # Tests with auto detection (sep=None) and some encodings
            for enc in ("utf-8", "utf-8-sig", "latin1"):
                try:
                    return pd.read_csv(BytesIO(raw), sep=None, engine="python", encoding=enc)
                except Exception:
                    pass
            # Tests with explicit separators
            for enc in ("utf-8", "utf-8-sig", "latin1"):
                for sep in (",", ";", "\t", "|"):
                    try:
                        return pd.read_csv(BytesIO(raw), sep=sep, encoding=enc)
                    except Exception:
                        continue
            # Last resort
            return pd.read_table(BytesIO(raw), encoding="utf-8", sep=None, engine="python")

        # --- read CSV (robust) ---
        raw = await file.read()
        try:
            X = read_csv_flexible_bytes(raw)
            import re
            def _clean_colname(c: str) -> str:
                # remove BOM, non-breakable spaces, trim, compress spaces
                c = str(c).replace("\ufeff", "").replace("\u200b", "").replace("\xa0", " ")
                c = re.sub(r"\s+", " ", c).strip()
                return c

            # normalize all column names in the uploaded CSV file
            X.columns = [_clean_colname(c) for c in X.columns]

        except Exception as e:
            raise HTTPException(status_code=422, detail=f"Invalid CSV: {e}")

        # --- predict (no probability required) ---
        try:
            if is_classification:
                from pycaret.classification import predict_model as _predict_model
                preds = _predict_model(model, data=X, raw_score=False)
            else:
                from pycaret.regression import predict_model as _predict_model
                preds = _predict_model(model, data=X)
        except Exception as e:
            raise HTTPException(status_code=500, detail=f"Prediction failed: {e}")

        # --- build the output: X + prediction columns ---
        out = X.copy()
        for col in preds.columns:
            if col not in out.columns:
                out[col] = preds[col].values

        # --- send a CSV back ---
        tmp = tempfile.NamedTemporaryFile(delete=False, suffix=".csv")
        tmp_path = tmp.name
        tmp.close()
        out.to_csv(tmp_path, index=False, encoding="utf-8")

        return FileResponse(
            tmp_path,
            media_type="text/csv",
            filename=f"predictions_{code}.csv"
        )


    # --- start the sub-server ---
    uvicorn.run(sub, host="127.0.0.1", port=port, log_level="info")



@app.post("/deploy_model")
def deploy_model(
    model_id: str = Form(...),
    model_name: str = Form(""),
    session_id: str = Form("")
):
    # if already deployed → return existing information
    if model_id in REGISTRY and REGISTRY[model_id]["status"] == "Deployed":
        port = REGISTRY[model_id]["port"]
        base = f"http://127.0.0.1:{port}"
        return {"model_id": model_id, "status": "Deployed", "url": f"{base}/predict", "api": f"{base}/docs"}

    port = find_free_port()
    proc = multiprocessing.Process(target=serve_model, args=(port, model_id), daemon=True)
    proc.start()
    # wait for /openapi.json to respond (Swagger loaded)
    ready = _wait_http_ok("127.0.0.1", port, "/openapi.json", timeout_s=8.0)
    if not ready:
        # security: shut down if it doesn't boot
        try:
            if proc.is_alive():
                proc.terminate()
                proc.join(timeout=1.0)
        except Exception:
            pass
        raise HTTPException(status_code=500, detail="Model server failed to start.")

    REGISTRY[model_id] = {"port": port, "proc": proc, "status": "Deployed", "code": (model_id.split("__",1)[0] if "__" in model_id else model_id)}
    base = f"http://127.0.0.1:{port}"
    return {"model_id": model_id, "status": "Deployed", "url": f"{base}/predict", "api": f"{base}/docs"}

    #time.sleep(0.8)  # short boot time

    #REGISTRY[model_id] = {"port": port, "proc": proc, "status": "Deployed"}
    #base = f"http://127.0.0.1:{port}"
    #return {"model_id": model_id, "status": "Deployed", "url": f"{base}/predict", "api": f"{base}/docs"}

@app.post("/undeploy_model")
def undeploy_model(model_id: str = Form(None)):
    targets = [model_id] if model_id else list(REGISTRY.keys())
    stopped = []
    for mid in targets:
        item = REGISTRY.get(mid)
        if not item: 
            continue
        proc = item.get("proc")
        port = item.get("port")
        try:
            if proc and proc.is_alive():
                proc.terminate()
                proc.join(timeout=1.5)
            # au besoin on force
            if proc and proc.is_alive():
                proc.kill()
                proc.join(timeout=1.0)
        except Exception:
            pass

        # double check: port must be closed
        if port is not None:
            t0 = time.time()
            while time.time() - t0 < 2.0 and not _port_is_closed(port):
                time.sleep(0.1)

        # completely remove from the registry
        REGISTRY.pop(mid, None)
        stopped.append(mid)
    return {"stopped": stopped}


@app.get("/deployments")
def deployments():
    items = []
    for mid, v in REGISTRY.items():
        base = f"http://127.0.0.1:{v['port']}"
        items.append({
            "model_id": mid,
            "model": mid,
            "url": f"{base}/predict",
            "api": f"{base}/docs",
            "status": v["status"]
        })
    return {"items": items}



# … keep BASE_DIR / MODELS_DIR defined as before …

def _to_bool(x) -> bool:
    if isinstance(x, bool): return x
    if x is None: return False
    return str(x).strip().lower() in ("true", "1", "yes", "y")

@app.post("/predict_deployed_model")
async def predict_deployed_model(
    file: UploadFile = File(...),
    proba: Optional[str] = Form("false"),
    # optional and ignored if not used by you:
    session_id: Optional[str] = Form(None),
    target: Optional[str] = Form(None),
):
    proba = _to_bool(proba)

    model_path = (MODELS_DIR / "deployed_model.pkl").resolve()
    if not model_path.exists():
        raise HTTPException(status_code=400, detail=f"Deployed model not found at {model_path}")

    # read the uploaded CSV file
    raw = await file.read()
    try:
        df = pd.read_csv(io.BytesIO(raw))
    except Exception as e:
        raise HTTPException(status_code=422, detail=f"Invalid CSV: {e}")

    # Load the deployed model (PyCaret)
    # -> if you are doing classification: from pycaret.classification import load_model, predict_model
    # -> otherwise regression: from pycaret.regression import load_model, predict_model
    try:
        from pycaret.classification import load_model, predict_model
        is_classification = True
    except Exception:
        from pycaret.regression import load_model, predict_model
        is_classification = False

    try:
        model = load_model(str(model_path).replace(".pkl", ""))  # PyCaret 3 accepts base names without .pkl
    except Exception:
        # fallback if the file is already a complete pickle
        import pickle
        with open(model_path, "rb") as f:
            model = pickle.load(f)

    try:
        preds = predict_model(model, data=df, raw_score=proba if is_classification else False)
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Prediction failed: {e}")

    # Save a temporary CSV file to be sent back
    tmp = tempfile.NamedTemporaryFile(delete=False, suffix=".csv")
    tmp_path = tmp.name
    tmp.close()
    preds.to_csv(tmp_path, index=False, encoding="utf-8")

    # Return a CSV file (Content-Type text/csv)
    filename = "predictions.csv"
    return FileResponse(
        tmp_path,
        media_type="text/csv",
        filename=filename
    )

