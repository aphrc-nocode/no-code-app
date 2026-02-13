import os
import tempfile
import math
import shutil
import time
from pathlib import Path
import pandas as pd
from fastapi import HTTPException
from celery_app import celery_app
from celery.schedules import crontab
from pycaret.classification import (
    setup as clf_setup,
    compare_models as clf_compare,
    pull as clf_pull,
    models as clf_models,
)

def read_csv_flexible(path):
    df = pd.read_csv(path)
    if len(df.columns) == 1:
        df = pd.read_csv(path, sep=';')
    return df

def _decide_stratify_and_validate(df, target: str, train_size: float):
    if target not in df.columns:
        raise HTTPException(status_code=400, detail=f"Target '{target}' not in columns.")
    y = df[target].dropna()
    n = len(y)
    if n < 3:
        raise HTTPException(status_code=400, detail=f"Dataset too small (n={n}). Need >= 3 rows.")
    vc = y.value_counts()
    n_classes = vc.shape[0]
    if n_classes < 2:
        raise HTTPException(status_code=400, detail="Only one class present in target. Need at least 2 classes.")
    min_count = int(vc.min())
    if min_count < 2:
        return False, f"data_split_stratify disabled automatically (minority class has {min_count} sample)."
    test_size = 1.0 - float(train_size)
    if min_count * test_size < 1.0:
        return True, "Stratified split kept. Note: dataset is small; consider larger test size."
    return True, None

def _models_table():
    md = clf_models().reset_index()
    cols = {c.lower(): c for c in md.columns}
    id_col = cols.get('id', 'index')
    if id_col not in md.columns: id_col = md.columns[0]
    if 'model' in cols: name_col = cols['model']
    elif 'name' in cols: name_col = cols['name']
    elif 'estimator' in cols: name_col = cols['estimator']
    else:
        md['Model'] = md[id_col].astype(str); name_col = 'Model'
    md = md.rename(columns={id_col: 'model_id', name_col: 'Model'})
    return md[['model_id','Model']]

def clean_json(obj):
    if isinstance(obj, dict):
        return {k: clean_json(v) for k, v in obj.items()}
    elif isinstance(obj, list):
        return [clean_json(x) for x in obj]
    elif isinstance(obj, float):
        if math.isnan(obj) or math.isinf(obj):
            return None
        return obj
    return obj

@celery_app.task(bind=True, name="tasks.train_automl_task")
def train_automl_task(self, file_data: bytes, target: str, session_id: int, 
                      analysis_type: str, train_size: float):
    try:
        self.update_state(state="PROCESSING", meta={"progress": 0, "message": "Reading data"})
        
        with tempfile.NamedTemporaryFile(delete=False, suffix=".csv") as tmp:
            tmp.write(file_data)
            tmp_path = tmp.name
        
        df = read_csv_flexible(tmp_path)
        os.remove(tmp_path)
        
        if analysis_type != "supervised":
            raise ValueError("Only 'supervised' supported here.")
        
        self.update_state(state="PROCESSING", meta={"progress": 10, "message": "Validating data"})
        stratify_flag, note = _decide_stratify_and_validate(df, target, train_size)
        
        self.update_state(state="PROCESSING", meta={"progress": 20, "message": "Setting up PyCaret"})
        clf_setup(
            data=df,
            target=target,
            session_id=int(session_id),
            train_size=float(train_size),
            data_split_shuffle=True,
            data_split_stratify=stratify_flag,
            fold=5,
            html=False,
            verbose=False,
        )
        
        self.update_state(state="PROCESSING", meta={"progress": 50, "message": "Training models"})
        clf_compare(turbo=False)
        
        self.update_state(state="PROCESSING", meta={"progress": 80, "message": "Collecting results"})
        lb = clf_pull().reset_index(drop=True)
        
        if lb is None or lb.empty:
            n_rows = df.shape[0]
            n_cols = df.shape[1]
            n_classes = df[target].nunique(dropna=True)
            raise ValueError(
                f"compare_models() returned an empty leaderboard. "
                f"Check your data: n_rows={n_rows}, n_cols={n_cols}, "
                f"n_classes(target)={n_classes}."
            )
        
        md = _models_table()
        name2code = {str(r["Model"]).strip().lower(): str(r["model_id"]).strip()
                     for _, r in md.iterrows()}
        
        if "Model" not in lb.columns:
            lb["Model"] = lb.get("model", lb.get("model_name", "Unknown"))
        
        codes = []
        for nm in lb["Model"].astype(str).tolist():
            key = nm.strip().lower()
            codes.append(name2code.get(key, None))
        lb["model_id"] = codes
        
        model_map = {}
        for nm, code in zip(lb["Model"].astype(str).tolist(), lb["model_id"].tolist()):
            if code is not None and str(code):
                model_map[str(nm)] = str(code)
        
        result = {
            "leaderboard": lb.to_dict(orient="records"),
            "models": model_map
        }
        
        return clean_json(result)
        
    except Exception as e:
        error_msg = f"{type(e).__name__}: {str(e)}"
        try:
            self.update_state(state="FAILURE", meta={"error": error_msg})
        except Exception:
            pass
        raise
    finally:
        _cleanup_pycaret_session(session_id)

def _cleanup_pycaret_session(session_id: int):
    try:
        base_dir = Path(os.getcwd())
        session_dirs = [
            base_dir / "pycaret_sessions" / str(session_id),
            base_dir / f"pycaret_sessions_{session_id}",
        ]
        for session_dir in session_dirs:
            if session_dir.exists() and session_dir.is_dir():
                if "pycaret_sessions" in str(session_dir):
                    shutil.rmtree(session_dir, ignore_errors=True)
    except Exception:
        pass

@celery_app.task(name="tasks.cleanup_old_sessions")
def cleanup_old_sessions():
    try:
        base_dir = Path(os.getcwd())
        pycaret_base = base_dir / "pycaret_sessions"
        
        if not pycaret_base.exists():
            return {"cleaned": 0, "message": "No pycaret_sessions directory found"}
        
        if "models" in str(pycaret_base) or "datasets" in str(pycaret_base):
            return {"cleaned": 0, "error": "Safety check: cleanup path contains protected directories"}
        
        cleaned_count = 0
        current_time = time.time()
        max_age_hours = 168
        
        for session_dir in pycaret_base.iterdir():
            if session_dir.is_dir():
                try:
                    dir_name = str(session_dir)
                    if "models" in dir_name or "datasets" in dir_name or "outputs" in dir_name:
                        continue
                    dir_age = current_time - session_dir.stat().st_mtime
                    if dir_age > (max_age_hours * 3600):
                        shutil.rmtree(session_dir, ignore_errors=True)
                        cleaned_count += 1
                except Exception:
                    continue
        
        return {"cleaned": cleaned_count, "message": f"Cleaned {cleaned_count} old session directories"}
    except Exception as e:
        return {"cleaned": 0, "error": str(e)}

celery_app.conf.beat_schedule = {
    "cleanup-old-sessions": {
        "task": "tasks.cleanup_old_sessions",
        "schedule": crontab(hour=2, minute=0),
    },
}
