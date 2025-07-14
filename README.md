# Low-/No- code platform for AI/ML


## Why the APP?

This app allows users to apply a variety of AI/ML tools without the need for coding. It integrates multiple frameworks designed specifically for AI/ML workflows, streamlining complex processes into a user-friendly interface. The app also includes predefined functions for data management, enabling users to easily handle data preparation, cleaning, and transformation tasks. Additionally, it offers robust visualization capabilities, allowing users to explore and interpret results effectively, all within a seamless, integrated environment.

## APP components 

The app is designed to provide end-to-end AI/ML workflow. Specifically:
- Upload
- Exploration
- Transformation
- Merging
- Visualization
- Transformation
- Data partitioning
- Feature engineering
- Model selection
- Hyperparameter tuning
- Model comparison 
- Prediction
- Deployment


## Setup

The APP depends on the R package [Rautoml](https://github.com/aphrc-nocode/Rautoml) which handles most of the back-end R functionalities. To start the app, you would first need to have the package installed.

```
devtools::install_github("aphrc-nocode/Rautoml")
```

## Usage

Clone the repo first :)

### R studio

Open either [server.R](./server.R) or [ui.R](./ui.R) from R Studio and then R the app.

### Python
Prepare your Python environment (FastAPI + PyCaret)
Step 1: Create a new virtual environment
- Create a new virtual environment
```
python -m venv env1
```
- Load virtual environment created on Windows
```
env1\Scripts\activate 
```
- Load virtual environment created on Linux
```
source env1/bin/activate
```
Step 2: Install libraries needed
```
pip install fastapi uvicorn pycaret pandas python-multipart shap
```

The APP depends on some Python libraries :
- FastAPI : Create API easily and fastly
- Pycaret : Module for ML no-code
- Uvicorn : It help to launch FastAPI
- Python-multipart : 

To start the app, you would first need to have the package installed.

## Launch FastAPI
main.py file contains code to create an API with FastAPI and Pycaret.
Go to command line, move to py folder and run this following command to Launch FastAPI.
```
uvicorn main:app --reload
```

### Makefile 

From the terminal, run:

```
make runapp
```

## Git workflow

Our workflow uses `make` pipelines to sync various components of the app. But you can also use standard Git commands or GUIs. See [Makefile](./Makefile). 

- To get started, run:

```
make Makefile
```

- To update (pull or push), run the command below. This will show a pop-up to update the commit message or use the default (discouraged :()

```
make sync
```

## License

- [GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007](./LICENSE)


## Contributors

- Steve Cygu
- Michael Ochola
- John Lunalo

## Maintainer(s)

- Steve Cygu


