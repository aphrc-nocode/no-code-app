##### ---- Folder setup  ------------------------------------------------####
## Folders
### Folder to save datasets
create_dir(paste0(app_username, "/datasets"))
### Create logs
create_dir(paste0(app_username, "/.log_files"))
create_dir(paste0(app_username, "/logs"))
### Models
create_dir(paste0(app_username, "/models"))
### Recipes
create_dir(paste0(app_username, "/recipes"))
### Create outputs
create_dir(paste0(app_username, "/outputs"))
create_dir(paste0(app_username, "/output")) # Achilles only works with output

