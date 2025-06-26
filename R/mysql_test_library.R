library(DBI)
library(RMySQL)

# Test simple de connexion (remplace les infos par les tiennes)
con <- dbConnect(
  MySQL(),
  dbname = "world",
  host = "localhost",
  port = 3305,
  user = "root",
  password = "root"
)
# Voir les tables disponibles
dbListTables(con)
# Déconnexion
dbDisconnect(con)
