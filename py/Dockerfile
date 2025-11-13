# 1) Image de base
FROM python:3.11-slim

# 2) Répertoire de travail
WORKDIR /app

# 3) Copier le code et les requirements
COPY main.py /app/
COPY requirements.txt /app/

# 4) Dépendances système pour PyCaret / LightGBM / matplotlib
#    (on enlève libatlas-base-dev qui n'existe pas sur cette image)
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    gcc \
    g++ \
    gfortran \
    libgomp1 \
    && rm -rf /var/lib/apt/lists/*

# 5) Dossier de logs / modèles (utilisés par main.py via LOGS_DIR)
ENV LOGS_DIR=/logs
RUN mkdir -p /logs/models

# 6) Installer les dépendances Python
RUN pip install --upgrade pip && \
    pip install -r requirements.txt

# 7) Port exposé (3760 demandé par le lead technique)
EXPOSE 3760

# 8) Commande de démarrage (6 workers)
CMD ["uvicorn", "main:app", \
     "--host", "0.0.0.0", \
     "--port", "3760", \
     "--workers", "6", \
     "--timeout-keep-alive", "120"]
