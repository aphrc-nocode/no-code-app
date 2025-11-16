FROM python:3.11-slim

ENV PYTHONUNBUFFERED=1 PIP_NO_CACHE_DIR=1

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential gfortran libgomp1 libopenblas-dev \
    libfreetype6-dev libpng-dev fonts-dejavu-core \
  && rm -rf /var/lib/apt/lists/*


WORKDIR /app/py
COPY requirements.txt /tmp/requirements.txt

# Déclarer les dossiers utilisés pour les volumes (facultatif mais propre)
VOLUME ["/app/logs", "/app/models"]

RUN pip install --upgrade pip setuptools wheel && \
    pip install -r /tmp/requirements.txt

COPY main.py /app/py/

EXPOSE 3760
CMD ["uvicorn","main:app","--host","0.0.0.0","--port","3760"]
