library(shiny)
library(httr)
library(jsonlite)
library(DT)
library(ggplot2)
library(rsconnect)

# Fonction pour récupérer les données ADEME filtrées sur le département 23, max 500 lignes
get_ademe_data <- function() {
  base_url <- 'https://data.ademe.fr/data-fair/api/v1/datasets/dpe03existant/lines'
  
  params <- list(
    page = 1,
    size = 500,
    select = 'emission_ges_chauffage,nom_commune_brut,code_departement_ban',
    qs = 'code_departement_ban:"23"'
  )
  
  url <- httr::modify_url(base_url, query = params)
  response <- httr::GET(url)
  
  if (response$status_code != 200) {
    stop("Erreur lors de l'appel à l'API ADEME.")
  }
  
  data <- jsonlite::fromJSON(rawToChar(response$content))
  data$result
}

# UI Shiny
ui <- fluidPage(
  titlePanel("Données ADEME - Département 23"),
  DTOutput("table_ademe"),
  br(),
  plotOutput("histogram_ges")
)

# Serveur Shiny
server <- function(input, output, session) {
  data <- reactive({
    get_ademe_data()
  })
  
  output$table_ademe <- renderDT({
    datatable(data(), options = list(pageLength = 50), rownames = FALSE)
  })
  
  output$histogram_ges <- renderPlot({
    df <- data()
    ges <- as.numeric(df$emission_ges_chauffage)
    ges <- ges[!is.na(ges)]
    
    ggplot(data.frame(emission_ges_chauffage = ges), aes(x = emission_ges_chauffage)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
      labs(title = "Histogramme des émissions GES chauffage",
           x = "Emission GES chauffage",
           y = "Nombre d'observations") +
      theme_minimal()
  })
}

# Configure ton compte shinyapps.io (à faire une seule fois)
rsconnect::setAccountInfo(name = 'diagnosticenedis',
                          token = '2C1CDC344B3AFD39CF944E9C8ABCB108',
                          secret = '7AUIV0flA1O98TQpfkd5SFxGYvdbkj0cmW9AUjYY')

# Chemin absolu vers le dossier contenant ce fichier app.R
appDir <- "C:/Users/lnugier/OneDrive - univ-lyon2.fr/Pièces jointes/S3/SAE R Shiny DPE/Projet_Enedis"

# Déploiement de l'application uniquement sur shinyapps.io
rsconnect::deployApp(appDir = appDir)

