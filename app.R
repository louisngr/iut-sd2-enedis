# --- INSTALLATION DES PACKAGES (si nécessaire) ---
# install.packages("shiny")
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("DT")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("shinyjs")
# install.packages("shinythemes") 
# install.packages("leaflet")

library(shiny)
library(httr)
library(jsonlite)
library(DT)
library(ggplot2)
library(dplyr)
library(shinyjs)
library(shinythemes) 
library(leaflet) 

# --- FONCTION DE RÉCUPÉRATION DES DONNÉES ---
get_ademe_data_multi <- function(departements = c("23", "19", "87"),
                                 size = 500,
                                 dataset = "dpe03existant") {
  base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets"
  full_url <- paste0(base_url, "/", dataset, "/lines")
  
  departements <- as.character(departements)
  
  quoted <- paste0('"', departements, '"')
  qs_filter <- paste0("code_departement_ban:(", paste(quoted, collapse = " OR "), ")")
  
  message("DEBUG — qs_filter = ", qs_filter)
  
  params <- list(
    page = 1,
    size = size * length(departements),
    select = "etiquette_dpe,emission_ges_chauffage,nom_commune_brut,code_departement_ban,surface_habitable_logement,conso_5_usages_par_m2_ep,type_batiment,annee_construction,etiquette_ges,date_etablissement_dpe,_geopoint",
    qs = qs_filter
  )
  
  url <- httr::modify_url(full_url, query = params)
  message("DEBUG — URL construite = ", url)
  
  response <- tryCatch(
    httr::GET(url, timeout(20)),
    error = function(e) {
      message("Erreur lors de la requête API : ", conditionMessage(e))
      return(NULL)
    }
  )
  
  if (is.null(response) || response$status_code != 200) {
    warning("Échec requête API, status = ", ifelse(is.null(response), "NULL", response$status_code))
    return(NULL)
  }
  
  content_text <- rawToChar(response$content)
  message("DEBUG — début de la réponse JSON : ", substr(content_text, 1, 200))
  
  parsed <- tryCatch(
    jsonlite::fromJSON(content_text),
    error = function(e) {
      message("Erreur parsing JSON : ", conditionMessage(e))
      return(NULL)
    }
  )
  
  if (is.null(parsed) || !"results" %in% names(parsed)) {
    warning("Problème de parsing JSON ou champ 'results' manquant.")
    return(NULL)
  }
  
  return(parsed$results)
}

# --- UI (User Interface) ---
ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  # CSS personnalisé
  tags$head(
    tags$style(HTML("
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
        background-color: #007bff !important;
        color: white !important;
        border-color: #007bff !important;
      }
      h4 { color: #007bff; font-weight: 600; font-size: 1.4em; border-bottom: 3px solid #007bff; padding-bottom: 5px; margin-bottom: 15px; }
      .well { background-color: white; border: 1px solid #ced4da; border-radius: 8px; padding: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); margin-bottom: 25px; }
      .btn-primary { background-color: #28a745; border-color: #28a745; }
      .btn-primary:hover { background-color: #218838; border-color: #1e7e34; }
      body { background-color: #e9ecef; }
    "))
  ),
  
  # --- 1. Écran de Connexion (Visible au départ) ---
  div(
    id = "login_page",
    wellPanel(
      style = "width: 400px; margin: 100px auto; padding: 30px;",
      h3(icon("lock"), "Authentification Requise"),
      textInput("userName", "Nom d'utilisateur:", value = "admin"),
      passwordInput("password", "Mot de passe:", value = "admin"),
      actionButton("loginButton", "Se connecter", class = "btn-primary"),
      br(),
      br(),
      textOutput("loginMessage"),
      
      # LOGOS
      fluidRow(
        column(6, align = "center",
               tags$img(src = "https://www.univ-lyon2.fr/uas/ksup/LOGO_PIED_DE_PAGE/Footer_800px_281_marque-etat-logo-rouge.png", 
                        height = "80px", style = "margin-top: 20px;"),
               tags$br() 
        ),
        column(6, align = "center",
               tags$img(src = "https://upload.wikimedia.org/wikipedia/fr/archive/7/77/20220206181136!Logo_enedis_header.png", 
                        height = "80px", style = "margin-top: 20px;")
        )
      )
    )
  ),
  
  # --- 2. Application Principale (Masquée au départ) ---
  div(
    id = "main_app",
    style = "display: none;",
    
    navbarPage(
      title = "Diagnostic de Performance Énergétique (DPE) – Analyse",
      id = "main_nav",
      
      header = tagList(
        br(),
        fluidRow(
          column(4,
                 actionButton("refresh_data", "Rafraîchir les Données", icon = icon("sync-alt"), class = "btn-info")
          ),
          column(4, 
                 offset = 4, 
                 textOutput("last_update")
          )
        ),
        br()
      ),
      
      # PAGE 1 : Contexte et Données Brutes (Fusion)
      tabPanel("Contexte et Données Brutes",
               fluidPage(
                 wellPanel(
                   h4("Contexte, Objectifs et Structure de l'Application"),
                   tags$p("Les données sont issues de l'**API Data ADEME** et concernent les diagnostics de performance énergétique (DPE). L'objectif est d'appliquer des méthodes de **Science des Données** pour analyser les tendances de consommation et d'émission de GES."),
                   tags$p("Le DPE est noté par deux étiquettes : l'**Étiquette Énergie (DPE)** et l'**Étiquette Climat (GES)**."),
                   tags$hr(),
                   tags$p("L'application est structurée en 4 onglets :"),
                   tags$ul(
                     tags$li("Analyse Unidimensionnelle et Temporelle : DPE, GES, Type de bâtiment et impact de l'année de construction pour le département sélectionné."),
                     tags$li("Analyse Bi-variée et Géographique : Corrélation, Régression, Cartographie et comparaison régionale."),
                     tags$li("Synthèse Multicritère : Classement par communes, Boxplot Conso/GES avec filtrage."),
                     tags$li("Contexte et Données Brutes (cet onglet) : Informations générales et accès aux jeux de données brutes.")
                   )
                 ),
                 # TABLEAUX DE DONNÉES BRUTES (Fusionnés)
                 wellPanel(
                   h4("Tableaux de Données Brutes"),
                   fluidRow(
                     column(4, 
                            selectInput("raw_data_selector", "Afficher les données :",
                                        choices = c("Département d'Analyse" = "1dep", "Comparaison Régionale (23 vs 19)" = "multi"),
                                        selected = "1dep")
                     ),
                     column(4, offset = 4, style = "margin-top: 25px;",
                            downloadButton("export_raw_csv", "Exporter CSV des données affichées")
                     )
                   ),
                   DTOutput("table_ademe_raw")
                 )
               )
      ),
      
      # PAGE 2 : Analyse Unidimensionnelle et Temporelle (Fusion)
      tabPanel("Analyse Unidimensionnelle et Temporelle",
               fluidPage(
                 wellPanel(
                   fluidRow(
                     column(4,
                            selectInput("dep_select_1", "Département à analyser :", 
                                        choices = c("Creuse (23)" = "23", "Corrèze (19)" = "19", "Haute-Vienne (87)" = "87", "Tous (23, 19, 87)" = "all"),
                                        selected = "23")
                     ),
                     column(4,
                            selectInput("type_bat_1", "Type de Bâtiment :",
                                        choices = c("Tous" = "Tous", "Maison" = "maison", "Appartement" = "appartement"),
                                        selected = "Tous")
                     )
                   )
                 ),
                 fluidRow(
                   column(6, wellPanel(h4("Répartition étiquettes DPE"), plotOutput("histogram_dpe"), downloadButton("export_dpe_png", "Exporter PNG"))),
                   column(6, wellPanel(h4("Distribution des émissions de GES (Histogramme)"), plotOutput("histogram_ges_freq"), downloadButton("export_ges_png", "Exporter PNG")))
                 ),
                 fluidRow(
                   column(6, wellPanel(h4("Distribution de la consommation : Maison vs Appartement"), plotOutput("density_conso_by_type"), downloadButton("export_conso_type_png", "Exporter PNG"))),
                   column(6, wellPanel(
                     fluidRow(
                       column(12, 
                              h4("Consommation EP selon Période de Construction"),
                              # Radio buttons déplacés sous le titre du graphique
                              radioButtons("periode_type", "Métrique :",
                                           choices = c("Moyenne (Barres)" = "bar", "Distribution (Boxplot)" = "box"),
                                           selected = "bar", inline = TRUE)
                       )
                     ),
                     plotOutput("boxplot_dpe_annee"), 
                     downloadButton("export_annee_png", "Exporter PNG")
                   ))
                 )
               )
      ),
      
      # PAGE 3 : Analyse Bi-variée et Géographique (Fusion)
      tabPanel("Analyse Bi-variée et Géographique",
               fluidPage(
                 wellPanel(
                   h4("Analyse de Corrélation et Régression Linéaire"),
                   fluidRow(
                     # Groupe X
                     column(3, selectInput("cor_x", "Variable X (Numérique) :", 
                                           choices = c("Consommation EP" = "conso_5_usages_par_m2_ep", "Surface habitable" = "surface_habitable_logement",
                                                       "Émission GES" = "emission_ges_chauffage", "Année de construction" = "annee_construction"))
                     ),
                     # Groupe Y
                     column(3, selectInput("cor_y", "Variable Y (Numérique) :", 
                                           choices = c("Émission GES" = "emission_ges_chauffage", "Consommation EP" = "conso_5_usages_par_m2_ep", 
                                                       "Surface habitable" = "surface_habitable_logement", "Année de construction" = "annee_construction"),
                                           selected = "conso_5_usages_par_m2_ep")
                     ),
                     # Groupe Département
                     column(3, selectInput("cor_dep_select", "Département(s) pour la corrélation :",
                                           choices = c("Creuse (23)" = "23", "Corrèze (19)" = "19", "Haute-Vienne (87)" = "87", "Tous (23, 19, 87)" = "all"),
                                           selected = "23")
                     ),
                     column(3, style = "margin-top: 25px;",
                            actionButton("calc_correlation", "Calculer & Afficher Corrélation", class = "btn-primary")
                     )
                   ),
                   textOutput("correlation_text"),
                   plotOutput("correlation_plot"),
                   downloadButton("export_correlation_png", "Exporter PNG")
                 ),
                 wellPanel(
                   h4("Analyse Géographique et Comparaison Régionale"),
                   fluidRow(
                     column(4,
                            selectInput("dep_map_select", "Département pour la carte :", 
                                        choices = c("Creuse (23)" = "23", "Corrèze (19)" = "19", "Haute-Vienne (87)" = "87", "Tous (23, 19, 87)" = "all"),
                                        selected = "23")
                     ),
                     column(8,
                            selectInput("dep_compare", "Comparaison Régionale (Dépt A vs B) :",
                                        choices = c("23 vs 19" = "23;19", "19 vs 87" = "19;87", "23 vs 87" = "23;87"),
                                        selected = "23;19")
                     )
                   ),
                   fluidRow(
                     column(6, wellPanel(h4("Cartographie des DPE"), leafletOutput("dpe_map", height = 450))),
                     column(6, wellPanel(h4("Comparaison Statistique des DPE"), plotOutput("comparison_dpe_regions"), downloadButton("export_regions_png", "Exporter PNG")))
                   )
                 )
               )
      ),
      
      # PAGE 4 : Synthèse Multicritère
      tabPanel("Synthèse Multicritère",
               fluidPage(
                 wellPanel(
                   fluidRow(
                     column(4,
                            # SelectInput avec multiple=TRUE pour choisir plusieurs catégories DPE
                            selectInput("dpe_filter", "Filtrer par Étiquettes DPE :",
                                        choices = LETTERS[1:7],
                                        selected = LETTERS[1:7], 
                                        multiple = TRUE)
                     ),
                     column(4,
                            sliderInput("conso_limit", "Limiter la Conso EP (kWh/m²/an) :",
                                        min = 0, max = 800, value = c(0, 800), step = 50)
                     )
                   )
                 ),
                 fluidRow(
                   column(6, wellPanel(h4("Top 10 Consommation moyenne par commune"), plotOutput("bar_conso_by_commune"), downloadButton("export_commune_png", "Exporter PNG"))),
                   column(6, wellPanel(h4("Distribution Consommation selon Étiquette GES"), plotOutput("boxplot_conso_by_ges"), downloadButton("export_ges_boxplot_png", "Exporter PNG")))
                 )
               )
      )
    ) # Fin navbarPage
  ) # Fin div main_app
) # Fin fluidPage (Termine l'objet UI)


# --- Serveur (Logique de l'application) ---
server <- function(input, output, session) {
  
  # --- 1. Gestion de la connexion ---
  login_status <- reactiveVal(FALSE)
  
  observeEvent(input$loginButton, {
    if (input$userName == "admin" && input$password == "admin") {
      login_status(TRUE)
      shinyjs::hide("login_page")
      shinyjs::show("main_app")
    } else {
      output$loginMessage <- renderText({"Nom d'utilisateur ou mot de passe incorrect."})
    }
  })
  
  # --- 2. Gestion du rafraîchissement et des données brutes ---
  
  last_update_time <- reactiveVal(Sys.time())
  data_trigger <- reactiveVal(0)
  
  observeEvent(input$refresh_data, {
    data_trigger(data_trigger() + 1)
  })
  
  output$last_update <- renderText({
    paste("Dernière mise à jour de l'API :", format(last_update_time(), "%Y-%m-%d %H:%M:%S"))
  })
  
  # Jeu 1: Données pour tous les départements potentiellement sélectionnés
  data_brute_all_deps <- eventReactive(list(data_trigger()), {
    # Charge 23, 19, 87 par défaut, si un input sélectionne "all", on utilise ces données
    deps_to_load <- c("23", "19", "87")
    
    df <- get_ademe_data_multi(departements = deps_to_load, size = 500, dataset = "dpe03existant")
    req(!is.null(df))
    if (nrow(df) > 0) {
      df$Departement <- paste("Département", df$code_departement_ban)
    }
    last_update_time(Sys.time())
    df
  }, ignoreNULL = FALSE)
  
  # Jeu 2: Données multi-départements (pour la comparaison régionale)
  data_brute_multi <- eventReactive(list(data_trigger(), input$dep_compare), {
    deps <- unlist(strsplit(input$dep_compare, ";"))
    df <- get_ademe_data_multi(departements = deps, size = 500, dataset = "dpe03existant")
    req(!is.null(df))
    df <- df %>% mutate(
      Departement = paste("Département", code_departement_ban)
    )
    df
  }, ignoreNULL = FALSE)
  
  # --- 3. Nettoyage et Filtrage des Données ---
  
  # Nettoyage de base (Géopoint, type conversion)
  data_clean_base <- reactive({
    df_raw <- data_brute_all_deps()
    req(nrow(df_raw) > 0)
    
    # Prétraitement de la géolocalisation
    df <- df_raw %>%
      mutate(
        latitude = as.numeric(sapply(strsplit(`_geopoint`, ","), `[`, 1)),
        longitude = as.numeric(sapply(strsplit(`_geopoint`, ","), `[`, 2))
      )
    
    df_clean <- df %>% 
      mutate(
        across(c(emission_ges_chauffage, surface_habitable_logement, conso_5_usages_par_m2_ep, annee_construction), as.numeric),
        etiquette_dpe = factor(etiquette_dpe, levels = LETTERS[1:7]),
        etiquette_ges = factor(etiquette_ges, levels = LETTERS[1:7])
      ) %>%
      filter(!is.na(etiquette_dpe))
    
    return(df_clean)
  })
  
  # Filtrage pour l'ANALYSE UNIDIMENSIONNELLE (Onglet II)
  filter_dep_analysis <- reactive({
    df <- data_clean_base()
    
    deps <- if (input$dep_select_1 == "all") c("23", "19", "87") else input$dep_select_1
    df <- df %>% filter(code_departement_ban %in% deps)
    
    if (input$type_bat_1 != "Tous") {
      df <- df %>% filter(type_batiment == input$type_bat_1)
    }
    return(df)
  })
  
  # Nettoyage du jeu multi-dép (pour la comparaison régionale)
  data_clean_multi <- reactive({
    df <- data_brute_multi()
    req(nrow(df) > 0)
    
    # Prétraitement de la géolocalisation
    df <- df %>%
      mutate(
        latitude = as.numeric(sapply(strsplit(`_geopoint`, ","), `[`, 1)),
        longitude = as.numeric(sapply(strsplit(`_geopoint`, ","), `[`, 2))
      )
    
    df %>% 
      mutate(
        across(c(emission_ges_chauffage, surface_habitable_logement, conso_5_usages_par_m2_ep, annee_construction), as.numeric),
        etiquette_dpe = factor(etiquette_dpe, levels = LETTERS[1:7]),
        etiquette_ges = factor(etiquette_ges, levels = LETTERS[1:7])
      ) %>%
      filter(!is.na(etiquette_dpe))
  })
  
  # --- 4. Fonctions de Génération des Graphiques ---
  
  dpe_colors <- c(A="#009933", B="#33cc33", C="#ccff33", D="#ffcc00", E="#ff9900", F="#ff6600", G="#ff0000")
  ges_colors <- dpe_colors
  
  # 4.1. Répartition DPE
  generate_histogram_dpe <- function(df) {
    title_dep <- if (input$dep_select_1 == "all") "Tous Départements" else paste("Département", unique(df$code_departement_ban))
    ggplot(df, aes(x = etiquette_dpe, fill = etiquette_dpe)) +
      geom_bar(color = "black") +
      scale_fill_manual(values = dpe_colors, drop = FALSE) +
      labs(title = paste("Répartition des étiquettes DPE (", title_dep, ")"), 
           x = "Étiquette DPE", y = "Nombre d’observations") +
      theme_minimal() +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size=14))
  }
  output$histogram_dpe <- renderPlot({ generate_histogram_dpe(filter_dep_analysis()) })
  output$export_dpe_png <- downloadHandler(
    filename = function() { paste("repartition_dpe-", Sys.Date(), ".png", sep="") },
    content = function(file) { ggsave(file, plot = generate_histogram_dpe(filter_dep_analysis()), device = "png", width = 8, height = 6) }
  )
  
  # 4.2. Distribution GES (Histogramme de fréquences)
  output$histogram_ges_freq <- renderPlot({ 
    df <- filter_dep_analysis() %>% filter(emission_ges_chauffage > 0 & emission_ges_chauffage < 500)
    title_dep <- if (input$dep_select_1 == "all") "Tous Départements" else paste("Département", unique(df$code_departement_ban))
    ggplot(df, aes(x = emission_ges_chauffage)) +
      geom_histogram(binwidth = 10, fill = "#17a2b8", color = "white", alpha = 0.8) +
      labs(title = paste("Distribution des émissions GES (", title_dep, ")"),
           x = "Émission GES (kg CO₂e/m²/an)",
           y = "Nombre d’observations") +
      scale_x_continuous(limits = c(0, 300)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size=14))
  })
  output$export_ges_png <- downloadHandler(
    filename = function() { paste("histogramme_ges-", Sys.Date(), ".png", sep="") },
    content = function(file) { ggsave(file, plot = output$histogram_ges_freq, device = "png", width = 8, height = 6) }
  )
  
  # 4.3. Densité Conso par Type
  generate_density_conso_by_type <- function(df) {
    df2 <- df %>% filter(type_batiment %in% c("maison", "appartement"),
                         conso_5_usages_par_m2_ep < 800)
    title_dep <- if (input$dep_select_1 == "all") "Tous Départements" else paste("Département", unique(df$code_departement_ban))
    
    ggplot(df2, aes(x = conso_5_usages_par_m2_ep, fill = type_batiment)) +
      geom_density(alpha = 0.6, adjust=1.5) +
      scale_fill_manual(values = c("maison"="#e76f51", "appartement"="#2a9d8f")) +
      labs(title = paste("Distribution de la consommation : Maison vs Appartement (", title_dep, ")"),
           x = "Consommation énergie primaire (kWh/m²/an)",
           y = "Densité",
           fill = "Type de bâtiment") +
      scale_x_continuous(limits = c(0,500)) +
      theme_minimal() +
      theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face="bold", size=14))
  }
  output$density_conso_by_type <- renderPlot({ generate_density_conso_by_type(filter_dep_analysis()) })
  output$export_conso_type_png <- downloadHandler(
    filename = function() { paste("density_conso_type-", Sys.Date(), ".png", sep="") },
    content = function(file) { ggsave(file, plot = generate_density_conso_by_type(filter_dep_analysis()), device = "png", width = 8, height = 6) }
  )
  
  # 4.4. Conso par Année (Boxplot / Barres)
  generate_boxplot_dpe_annee <- function(df, type_plot) {
    df2 <- df %>% filter(!is.na(annee_construction), annee_construction >= 1900)
    df2 <- df2 %>%
      mutate(
        periode_construction = cut(annee_construction,
                                   breaks = c(1900, 1948, 1975, 1989, 2000, 2012, 2024),
                                   labels = c("Avant 1948","1948‑1974","1975‑1988","1989‑1999","2000‑2011","Après 2012"),
                                   right = FALSE,
                                   include.lowest = TRUE)
      ) %>%
      filter(!is.na(periode_construction), conso_5_usages_par_m2_ep < 1000)
    
    if (type_plot == "box") {
      p <- ggplot(df2, aes(x = periode_construction, y = conso_5_usages_par_m2_ep, fill = periode_construction)) +
        geom_boxplot(outlier.alpha = 0.1) +
        labs(y = "Conso EP (kWh/m²/an)")
    } else {
      df_mean <- df2 %>% group_by(periode_construction) %>% 
        summarise(conso_moy = mean(conso_5_usages_par_m2_ep, na.rm=TRUE), .groups="drop")
      p <- ggplot(df_mean, aes(x = periode_construction, y = conso_moy, fill = periode_construction)) +
        geom_col(color="black") +
        labs(y = "Conso EP Moyenne (kWh/m²/an)")
    }
    
    p + labs(title = "Consommation EP selon période de construction", x = "Période de construction") +
      scale_y_continuous(limits = c(0,700)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle=45, hjust=1), legend.position = "none",
            plot.title = element_text(hjust=0.5, face="bold", size=14))
  }
  
  output$boxplot_dpe_annee <- renderPlot({ generate_boxplot_dpe_annee(filter_dep_analysis(), input$periode_type) })
  output$export_annee_png <- downloadHandler(
    filename = function() { paste("conso_annee-", Sys.Date(), ".png", sep="") },
    content = function(file) { ggsave(file, plot = generate_boxplot_dpe_annee(filter_dep_analysis(), input$periode_type), device = "png", width = 10, height = 7) }
  )
  
  # 4.5. Bar Conso par Commune
  generate_bar_conso_by_commune <- function(df) {
    df2 <- df %>% filter(!is.na(conso_5_usages_par_m2_ep), conso_5_usages_par_m2_ep < 800)
    
    # Applique le filtre DPE et Conso de l'onglet IV (Multicritère)
    df2 <- df2 %>% 
      filter(etiquette_dpe %in% input$dpe_filter) %>%
      filter(conso_5_usages_par_m2_ep >= input$conso_limit[1] & conso_5_usages_par_m2_ep <= input$conso_limit[2])
    
    df_comm <- df2 %>% 
      group_by(nom_commune_brut) %>%
      summarise(conso_moy = mean(conso_5_usages_par_m2_ep, na.rm=TRUE), .groups="drop") %>%
      arrange(desc(conso_moy)) %>%
      slice_head(n=10)
    
    ggplot(df_comm, aes(x = reorder(nom_commune_brut, conso_moy), y = conso_moy, fill = conso_moy)) +
      geom_col() +
      scale_fill_gradient(low="#b2e067", high="#e76f51", name="Conso Moy.") +
      labs(title="Top 10 communes les plus énergivores", x="Commune", y="Conso EP moyenne (kWh/m²/an)") +
      coord_flip() +
      theme_minimal() +
      theme(legend.position="none", plot.title = element_text(hjust=0.5, face="bold", size=14))
  }
  output$bar_conso_by_commune <- renderPlot({ generate_bar_conso_by_commune(filter_dep_analysis()) })
  output$export_commune_png <- downloadHandler(
    filename = function() { paste("bar_conso_commune-", Sys.Date(), ".png", sep="") },
    content = function(file) { ggsave(file, plot = generate_bar_conso_by_commune(filter_dep_analysis()), device = "png", width = 8, height = 6) }
  )
  
  # 4.6. Boxplot Conso par GES (Retrait des outliers)
  generate_boxplot_conso_by_ges <- function(df) {
    df2 <- df %>% filter(!is.na(etiquette_ges), conso_5_usages_par_m2_ep < 800)
    
    # Applique le filtre DPE et Conso de l'onglet IV (Multicritère)
    df2 <- df2 %>% 
      filter(etiquette_dpe %in% input$dpe_filter) %>%
      filter(conso_5_usages_par_m2_ep >= input$conso_limit[1] & conso_5_usages_par_m2_ep <= input$conso_limit[2])
    
    ggplot(df2, aes(x = etiquette_ges, y = conso_5_usages_par_m2_ep, fill = etiquette_ges)) +
      geom_boxplot(outlier.shape = NA) + # Retire les outliers
      scale_fill_manual(values = ges_colors, drop = FALSE) +
      labs(title="Consommation EP selon Étiquette GES", x="Étiquette GES", y="Conso EP (kWh/m²/an)") +
      scale_y_continuous(limits = c(0,700)) +
      theme_minimal() +
      theme(legend.position="none", plot.title = element_text(hjust=0.5, face="bold", size=14))
  }
  output$boxplot_conso_by_ges <- renderPlot({ generate_boxplot_conso_by_ges(filter_dep_analysis()) })
  output$export_ges_boxplot_png <- downloadHandler(
    filename = function() { paste("boxplot_conso_ges-", Sys.Date(), ".png", sep="") },
    content = function(file) { ggsave(file, plot = generate_boxplot_conso_by_ges(filter_dep_analysis()), device = "png", width = 8, height = 6) }
  )
  
  # 4.7. Comparaison Régionale
  generate_comparison_dpe_regions <- function(df) {
    ggplot(df, aes(x = etiquette_dpe, fill = etiquette_dpe)) +
      geom_bar(position="dodge", color="black") +
      facet_wrap(~ Departement, scales="free_y") +
      scale_fill_manual(values = dpe_colors, drop = FALSE) +
      labs(title=paste("Répartition des étiquettes DPE : Comparaison", input$dep_compare),
           x="Étiquette DPE", y="Nombre d’observations") +
      theme_minimal() +
      theme(legend.position="none", plot.title = element_text(hjust=0.5, face = "bold", size=14))
  }
  output$comparison_dpe_regions <- renderPlot({ generate_comparison_dpe_regions(data_clean_multi()) })
  output$export_regions_png <- downloadHandler(
    filename = function() { paste("comparison_dpe_regions-", Sys.Date(), ".png", sep="") },
    content = function(file) { ggsave(file, plot = generate_comparison_dpe_regions(data_clean_multi()), device = "png", width = 10, height = 6) }
  )
  
  # --- 5. Corrélation et Régression (Correction de l'erreur "unused argument") ---
  
  correlation_plot_react <- eventReactive(input$calc_correlation, {
    # Filtre sur le(s) département(s) sélectionné(s) dans l'onglet III
    deps <- if (input$cor_dep_select == "all") c("23", "19", "87") else input$cor_dep_select
    df <- data_clean_base() %>% filter(code_departement_ban %in% deps)
    
    req(nrow(df) > 0)
    
    x_var <- sym(input$cor_x)
    y_var <- sym(input$cor_y)
    
    df_plot <- df %>% 
      select(!!x_var, !!y_var) %>% 
      filter(!is.na(!!x_var), !is.na(!!y_var))
    
    cor_value <- cor(df_plot[[input$cor_x]], df_plot[[input$cor_y]], use = "complete.obs")
    lm_model <- lm(paste(input$cor_y, "~", input$cor_x), data = df_plot)
    r_squared <- summary(lm_model)$r.squared
    
    output$correlation_text <- renderText({
      paste0("Coefficient de Corrélation de Pearson (R) : ", round(cor_value, 3), 
             " | R-carré de la Régression : ", round(r_squared, 3),
             " (Département(s) : ", paste(unique(deps), collapse = ", "), ")")
    })
    
    p <- ggplot(df_plot, aes(x = !!x_var, y = !!y_var)) +
      geom_point(alpha = 0.6, color = "#007bff") +
      geom_smooth(method = "lm", se = TRUE, color = "#dc3545", fill = "#dc3545", alpha = 0.2) +
      labs(title = paste("Corrélation :", input$cor_y, "vs", input$cor_x),
           x = input$cor_x,
           y = input$cor_y) +
      theme_light() +
      theme(plot.title = element_text(hjust=0.5, face="bold", size=14))
    
    return(p)
  })
  
  output$correlation_plot <- renderPlot({ correlation_plot_react() })
  output$export_correlation_png <- downloadHandler(
    filename = function() { paste("correlation_plot-", Sys.Date(), ".png", sep="") },
    content = function(file) {
      p <- correlation_plot_react()
      ggsave(file, plot = p, device = "png", width = 10, height = 7)
    }
  )
  
  # --- 6. Cartographie Interactive (Leaflet) ---
  
  output$dpe_map <- renderLeaflet({
    
    deps <- if (input$dep_map_select == "all") c("23", "19", "87") else input$dep_map_select
    
    df <- data_clean_base() %>% filter(code_departement_ban %in% deps)
    req(nrow(df) > 0)
    
    df_map <- df %>% 
      filter(!is.na(latitude), !is.na(longitude), latitude > 0, longitude > -10)
    
    req(nrow(df_map) > 0)
    
    # Calcul du centre de vue englobant
    center_lng = mean(df_map$longitude, na.rm=TRUE)
    center_lat = mean(df_map$latitude, na.rm=TRUE)
    zoom_level = if (length(deps) > 1) 7 else 8
    
    pal <- colorFactor(palette = dpe_colors, domain = LETTERS[1:7], ordered = TRUE)
    
    leaflet(df_map) %>% addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = center_lng, lat = center_lat, zoom = zoom_level) %>% 
      addCircleMarkers(
        lng = ~longitude, 
        lat = ~latitude,
        radius = 4, 
        color = ~pal(etiquette_dpe),
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.8,
        popup = ~paste(
          "<b>DPE :</b> ", etiquette_dpe, "<br>",
          "<b>Dépt :</b> ", code_departement_ban, "<br>",
          "<b>Commune :</b> ", nom_commune_brut, "<br>",
          "<b>Conso EP :</b> ", round(conso_5_usages_par_m2_ep, 1), " kWh/m²/an"
        )
      ) %>%
      addLegend(pal = pal, values = LETTERS[1:7], opacity = 1, title = "Étiquette DPE")
  })
  
  # --- 7. Tableaux de Données Brutes et Exports CSV (Fusionnés) ---
  
  data_raw_display <- reactive({
    if (input$raw_data_selector == "1dep") {
      data_clean_base() %>% filter(code_departement_ban == input$dep_select_1) %>% select(-latitude, -longitude)
    } else {
      data_clean_multi() %>% select(-latitude, -longitude)
    }
  })
  
  output$table_ademe_raw <- renderDT({
    datatable(data_raw_display(), 
              options=list(pageLength=10, scrollX=TRUE), rownames=FALSE,
              caption=paste("Source : ADEME DPE —", input$raw_data_selector))
  })
  
  output$export_raw_csv <- downloadHandler(
    filename = function() { paste("ademe_dpe_export-", input$raw_data_selector, "-", Sys.Date(), ".csv", sep="") },
    content = function(file) { write.csv(data_raw_display(), file, row.names = FALSE) }
  )
}

# --- LANCER L’APP ---
shinyApp(ui=ui, server=server)