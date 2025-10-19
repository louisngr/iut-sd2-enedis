library(shiny)
library(httr)
library(jsonlite)
library(DT)
library(ggplot2)
library(dplyr)

# --- FONCTION DE RÉCUPÉRATION DES DONNÉES ---
get_ademe_data_multi <- function(departements = c("23", "19"),
                                 size = 500,
                                 dataset = "dpe03existant") {
  # dataset peut être "dpe03existant" (logements existants) ou un autre identifiant
  base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets"
  full_url <- paste0(base_url, "/", dataset, "/lines")
  
  # Force le type caractère
  departements <- as.character(departements)
  
  # Construire filtre qs : code_departement_ban:("23" OR "19")
  quoted <- paste0('"', departements, '"')
  qs_filter <- paste0("code_departement_ban:(", paste(quoted, collapse = " OR "), ")")
  
  message("DEBUG — qs_filter = ", qs_filter)
  
  params <- list(
    page = 1,
    size = size * length(departements),
    select = "etiquette_dpe,emission_ges_chauffage,nom_commune_brut,code_departement_ban,surface_habitable_logement,conso_5_usages_par_m2_ep,type_batiment,annee_construction,etiquette_ges",
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
  
  if (is.null(response)) {
    warning("Response is NULL")
    return(NULL)
  }
  if (response$status_code != 200) {
    warning("Échec requête API, status = ", response$status_code)
    cont <- try(rawToChar(response$content), silent = TRUE)
    message("DEBUG — contenu réponse = ", cont)
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
  
  if (is.null(parsed)) {
    warning("parsed JSON est NULL")
    return(NULL)
  }
  
  if (!"results" %in% names(parsed)) {
    warning("Champ 'results' manquant dans le JSON")
    return(NULL)
  }
  
  return(parsed$results)
}

# --- UI ---
ui <- navbarPage(
  title = "Diagnostic de Performance Énergétique (DPE) – Départements sélectionnés",
  
  tags$head(
    tags$style(HTML("
      body { font-family: 'Arial', sans-serif; background-color: #f8f9fa; }
      .panel-title { background-color: #007bff; color: white; padding: 15px; margin-bottom: 20px; border-radius: 5px; font-weight: bold; font-size: 24px; }
      .well { background-color: white; border: 1px solid #dee2e6; border-radius: 5px; padding: 20px; box-shadow: 0 0 10px rgba(0,0,0,0.05); }
      h4 { color: #004085; font-weight: bold; font-size: 1.35em; border-bottom: 2px solid #dee2e6; padding-bottom: 5px; margin-bottom: 15px; }
    "))
  ),
  
  tabPanel("Analyse DPE – Département unique",
           fluidPage(
             fluidRow(
               column(6, wellPanel(h4("Répartition étiquettes DPE"), plotOutput("histogram_dpe"))),
               column(6, wellPanel(h4("Émissions GES chauffage"), plotOutput("histogram_ges")))
             ),
             fluidRow(
               column(6, wellPanel(h4("Conso EP vs Surface"), plotOutput("scatter_conso_surface"))),
               column(6, wellPanel(h4("Conso : Maison vs Appartement"), plotOutput("density_conso_by_type")))
             )
           )
  ),
  tabPanel("Analyse Temporelle",
           fluidPage(
             fluidRow(
               column(12, wellPanel(h4("Conso selon Période de Construction"), plotOutput("boxplot_dpe_annee")))
             )
           )
  ),
  tabPanel("Analyse Multicritère",
           fluidPage(
             fluidRow(
               column(6, wellPanel(h4("Distribution DPE par type de bâtiment"), plotOutput("bar_dpe_by_type"))),
               column(6, wellPanel(h4("Conso moyenne par commune"), plotOutput("bar_conso_by_commune")))
             ),
             fluidRow(
               column(12, wellPanel(h4("Conso selon Étiquette GES"), plotOutput("boxplot_conso_by_ges")))
             )
           )
  ),
  tabPanel("Comparaison Régionale",
           fluidPage(
             fluidRow(
               column(12, wellPanel(h4("Comparaison DPE : plusieurs départements"), plotOutput("comparison_dpe_regions")))
             )
           )
  ),
  tabPanel("Données Brutes",
           fluidPage(
             fluidRow(
               column(12, wellPanel(h4("Tableau – Département unique"), DTOutput("table_ademe_1dep"))),
               column(12, wellPanel(h4("Tableau – Multi‑Départements"), DTOutput("table_ademe_multi")))
             )
           )
  )
)

# --- Serveur ---
server <- function(input, output, session) {
  
  # Données pour un seul département (ex : 23)
  data_brute_1dep <- reactive({
    df <- get_ademe_data_multi(departements = c("23"), size = 500, dataset = "dpe03existant")
    req(!is.null(df))
    if (nrow(df) > 0) {
      df$Departement <- "Département 23"
    }
    df
  })
  
  data_clean_1dep <- reactive({
    df <- data_brute_1dep()
    req(nrow(df) > 0)
    df %>% 
      mutate(
        emission_ges_chauffage = as.numeric(emission_ges_chauffage),
        surface_habitable_logement = as.numeric(surface_habitable_logement),
        conso_5_usages_par_m2_ep = as.numeric(conso_5_usages_par_m2_ep),
        annee_construction = as.numeric(annee_construction),
        etiquette_dpe = factor(etiquette_dpe, levels = LETTERS[1:7]),
        etiquette_ges = factor(etiquette_ges, levels = LETTERS[1:7])
      ) %>%
      filter(!is.na(etiquette_dpe))
  })
  
  # Données multi‐départements (ex : 23, 19)
  data_brute_multi <- reactive({
    df <- get_ademe_data_multi(departements = c("23", "19"), size = 500, dataset = "dpe03existant")
    req(!is.null(df))
    df <- df %>% mutate(
      Departement = ifelse(code_departement_ban == "23", "Département 23", "Département 19")
    )
    df
  })
  
  data_clean_multi <- reactive({
    df <- data_brute_multi()
    req(nrow(df) > 0)
    df %>% 
      mutate(
        emission_ges_chauffage = as.numeric(emission_ges_chauffage),
        surface_habitable_logement = as.numeric(surface_habitable_logement),
        conso_5_usages_par_m2_ep = as.numeric(conso_5_usages_par_m2_ep),
        annee_construction = as.numeric(annee_construction),
        etiquette_dpe = factor(etiquette_dpe, levels = LETTERS[1:7]),
        etiquette_ges = factor(etiquette_ges, levels = LETTERS[1:7])
      ) %>%
      filter(!is.na(etiquette_dpe))
  })
  
  # --- Graphiques / Tableaux ---
  
  output$histogram_ges <- renderPlot({
    df <- data_clean_1dep()
    req(nrow(df) > 0)
    df2 <- df %>% filter(emission_ges_chauffage > 0 & emission_ges_chauffage < 500)
    ggplot(df2, aes(x = emission_ges_chauffage)) +
      geom_histogram(binwidth = 5, fill = "#17a2b8", color = "white", alpha = 0.8) +
      labs(title = "Distribution des émissions GES Chauffage",
           x = "Émission GES (kg CO₂e/m²/an)",
           y = "Nombre d’observations") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size=16))
  })
  
  output$histogram_dpe <- renderPlot({
    df <- data_clean_1dep()
    req(nrow(df) > 0)
    dpe_colors <- c(A="#009933", B="#33cc33", C="#ccff33", D="#ffcc00", E="#ff9900", F="#ff6600", G="#ff0000")
    ggplot(df, aes(x = etiquette_dpe, fill = etiquette_dpe)) +
      geom_bar(color = "black") +
      scale_fill_manual(values = dpe_colors, drop = FALSE) +
      labs(title = "Répartition des étiquettes DPE", x = "Étiquette DPE", y = "Nombre") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold", size=16))
  })
  
  output$scatter_conso_surface <- renderPlot({
    df <- data_clean_1dep()
    req(nrow(df) > 0)
    df2 <- df %>% filter(!is.na(surface_habitable_logement),
                         !is.na(conso_5_usages_par_m2_ep),
                         conso_5_usages_par_m2_ep < 800)
    ggplot(df2, aes(x = surface_habitable_logement,
                    y = conso_5_usages_par_m2_ep,
                    color = etiquette_dpe)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = "black", linetype="dashed") +
      scale_color_manual(values = c(A="#009933", B="#33cc33", C="#ccff33", D="#ffcc00", E="#ff9900", F="#ff6600", G="#ff0000"),
                         name = "Étiquette DPE") +
      labs(title = "Consommation EP vs Surface",
           x = "Surface habitable (m²)",
           y = "Consommation énergie primaire (kWh/m²/an)") +
      theme_light() +
      theme(plot.title = element_text(hjust=0.5, face="bold", size=16))
  })
  
  output$density_conso_by_type <- renderPlot({
    df <- data_clean_1dep()
    req(nrow(df) > 0)
    df2 <- df %>% filter(type_batiment %in% c("maison", "appartement"),
                         conso_5_usages_par_m2_ep < 800)
    ggplot(df2, aes(x = conso_5_usages_par_m2_ep, fill = type_batiment)) +
      geom_density(alpha = 0.6, adjust=1.5) +
      scale_fill_manual(values = c("maison"="#e76f51", "appartement"="#2a9d8f")) +
      labs(title = "Distribution de la consommation : Maison vs Appartement",
           x = "Consommation énergie primaire (kWh/m²/an)",
           y = "Densité",
           fill = "Type de bâtiment") +
      scale_x_continuous(limits = c(0,500)) +
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust=0.5, face="bold", size=16))
  })
  
  output$boxplot_dpe_annee <- renderPlot({
    df <- data_clean_1dep()
    req(nrow(df) > 0)
    df2 <- df %>% filter(!is.na(annee_construction), annee_construction >= 1900)
    df2 <- df2 %>%
      mutate(
        periode_construction = cut(annee_construction,
                                   breaks = c(1900, 1948, 1975, 1989, 2000, 2012, 2024),
                                   labels = c("Avant 1948","1948‑1974","1975‑1988","1989‑1999","2000‑2011","Après 2012"),
                                   right = FALSE,
                                   include.lowest = TRUE)
      ) %>%
      filter(!is.na(periode_construction),
             conso_5_usages_par_m2_ep < 1000)
    ggplot(df2, aes(x = periode_construction, y = conso_5_usages_par_m2_ep, fill = periode_construction)) +
      geom_boxplot(outlier.alpha = 0.1) +
      labs(title = "Consommation EP selon période de construction",
           x = "Période de construction",
           y = "Consommation énergie primaire (kWh/m²/an)") +
      scale_y_continuous(limits = c(0,700)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            legend.position = "none",
            plot.title = element_text(hjust=0.5, face="bold", size=16))
  })
  
  output$bar_dpe_by_type <- renderPlot({
    df <- data_clean_1dep()
    req(nrow(df) > 0)
    df2 <- df %>% filter(type_batiment %in% c("maison", "appartement"))
    dpe_colors <- c(A="#009933", B="#33cc33", C="#ccff33", D="#ffcc00", E="#ff9900", F="#ff6600", G="#ff0000")
    ggplot(df2, aes(x = etiquette_dpe, fill = etiquette_dpe)) +
      geom_bar(position = "fill", color="black") +
      facet_wrap(~ type_batiment) +
      scale_fill_manual(values = dpe_colors, drop = FALSE) +
      labs(title = "Distribution DPE par type de bâtiment", x = "Étiquette DPE", y = "Proportion") +
      theme_minimal() +
      theme(plot.title = element_text(hjust=0.5, face="bold", size=16))
  })
  
  output$bar_conso_by_commune <- renderPlot({
    df <- data_clean_1dep()
    req(nrow(df) > 0)
    df2 <- df %>% filter(!is.na(conso_5_usages_par_m2_ep), conso_5_usages_par_m2_ep < 800)
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
      theme(legend.position="none", plot.title = element_text(hjust=0.5, face="bold", size=16))
  })
  
  output$boxplot_conso_by_ges <- renderPlot({
    df <- data_clean_1dep()
    req(nrow(df) > 0)
    df2 <- df %>% filter(!is.na(etiquette_ges), conso_5_usages_par_m2_ep < 800)
    ges_colors <- c(A="#009933", B="#33cc33", C="#ccff33", D="#ffcc00", E="#ff9900", F="#ff6600", G="#ff0000")
    ggplot(df2, aes(x = etiquette_ges, y = conso_5_usages_par_m2_ep, fill = etiquette_ges)) +
      geom_boxplot(outlier.alpha = 0.1) +
      scale_fill_manual(values = ges_colors, drop = FALSE) +
      labs(title="Consommation EP selon Étiquette GES", x="Étiquette GES", y="Conso EP (kWh/m²/an)") +
      scale_y_continuous(limits = c(0,700)) +
      theme_minimal() +
      theme(legend.position="none", plot.title = element_text(hjust=0.5, face="bold", size=16))
  })
  
  output$comparison_dpe_regions <- renderPlot({
    df <- data_clean_multi()
    req(nrow(df) > 0)
    dpe_colors <- c(A="#009933", B="#33cc33", C="#ccff33", D="#ffcc00", E="#ff9900", F="#ff6600", G="#ff0000")
    ggplot(df, aes(x = etiquette_dpe, fill = etiquette_dpe)) +
      geom_bar(position="dodge", color="black") +
      facet_wrap(~ Departement, scales="free_y") +
      scale_fill_manual(values = dpe_colors, drop = FALSE) +
      labs(title="Répartition des étiquettes DPE : comparaison départements",
           x="Étiquette DPE", y="Nombre d’observations") +
      theme_minimal() +
      theme(legend.position="none", plot.title = element_text(hjust=0.5, face="bold", size=16))
  })
  
  output$table_ademe_1dep <- renderDT({
    df <- data_brute_1dep()
    req(nrow(df) > 0)
    datatable(df, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE,
              caption="Source : ADEME DPE – Dépt seul")
  })
  
  output$table_ademe_multi <- renderDT({
    df <- data_brute_multi()
    req(nrow(df) > 0)
    datatable(df, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE,
              caption="Source : ADEME DPE – Multi dép.")
  })
}

# --- Lancer l’app ---
shinyApp(ui=ui, server=server)
