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

Open either [server/server.R](./server/server.R) or (ui/ui.R)[./ui.R] from R Studio and then R the app.

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

## Licence

- [GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007](./LICENCE)


## Contributors

- Steve Cygu
- Michael Ochola
- John Lunalo

## Maintainer(s)

- Steve Cygu


