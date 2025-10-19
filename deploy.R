# Charger le package rsconnect
library(rsconnect)

# Configurer ton compte shinyapps.io
rsconnect::setAccountInfo(
  name   = "diagnosticenedis", 
  token  = "2C1CDC344B3AFD39CF944E9C8ABCB108",
  secret = "7AUIV0flA1O98TQpfkd5SFxGYvdbkj0cmW9AUjYY"
)

# Chemin vers le dossier contenant app.R
appDir <- "C:/Users/louis/OneDrive - univ-lyon2.fr/Pièces jointes/S3/SAE R Shiny DPE/Projet_Enedis"

# Déployer l'application
rsconnect::deployApp(appDir = appDir)
