# Documentation Technique – Application DPE ADEME (Shiny)

------------------------------------------------------------------------

## I. Architecture Technique et Flux de Données

### 1. Schéma d'Architecture Fonctionnelle

Cette application utilise l'architecture réactive de **Shiny** pour gérer le flux de données, de l'acquisition API jusqu'à la restitution graphique, en intégrant une gestion du thème et des exports.

| Composant | Technologie(s) | Rôle dans le Flux de Données |
|----|----|----|
| **Source Externe** | ADEME API / HTTP GET | Fourniture des données DPE brutes (`dpe03existant`) via l'API Data Fair. |
| **Acquisition** | httr, jsonlite | Exécution de la requête HTTP et conversion des résultats JSON en DataFrame R. |
| **Serveur** | shiny, reactive | Gestion de la réactivité, de l'authentification, du filtrage et des calculs statistiques (corrélation, régression). |
| **Traitement/Nettoyage** | dplyr, rlang | Filtrage des valeurs manquantes, conversion de types (numérique, facteur), prétraitement des coordonnées géographiques. |
| **Visualisation** | ggplot2, leaflet, DT | Rendu des graphiques statistiques, de la carte interactive (Leaflet) et du tableau de données (DT). |
| **Interface Utilisateur (UI)** | shiny, shinyjs, shinythemes | Affichage, navigation (navbarPage), gestion du thème (CSS/JS) et des éléments de contrôle (sliders, boutons). |

------------------------------------------------------------------------

### 2. Flux de Données Détaillé

Le cycle de vie des données suit une chaîne réactive :

1.  **Déclenchement :**
    Le chargement initial ou l'action sur le bouton *"Rafraîchir les Données"* (`input$refresh_data`) active l'appel API.

2.  **Acquisition API :**
    La fonction `get_ademe_data_multi` utilise `httr::GET` pour interroger l'API de l’ADEME, ciblant les départements **19**, **23** et **87**.
    Le paramètre `qs` (query string) est construit pour filtrer les départements directement à la source, optimisant la charge.

3.  **Nettoyage (`data_clean_base`) :**

    -   Conversion des colonnes clés (GES, consommation, surface, année de construction) en **type numeric**.
    -   Extraction et transformation des coordonnées géographiques à partir du champ `_geopoint` (chaîne `"lat,lng"`) vers deux colonnes numériques `latitude` et `longitude`.
    -   Les étiquettes **DPE** et **GES** sont factorisées avec un ordre alphabétique.

3.  **Filtrage**
    Les données sont ensuite filtrées dynamiquement selon les sélections utilisateur (département, type de bâtiment, limites de consommation).

5.  **Génération des Résultats :**

    -   **Graphiques :** `ggplot2` utilise le thème adapté (`get_custom_theme`) pour garantir la cohérence visuelle en mode clair ou sombre.
    -   **Cartographie :** `leaflet` est alimenté par les coordonnées nettoyées et utilise `colorFactor` pour attribuer des couleurs cohérentes (A à G) aux marqueurs circulaires.
        Un filtre CSS (`filter: invert(90%) hue-rotate(180deg)`) est appliqué sur la carte en mode sombre pour en améliorer la lisibilité.
    -   **Corrélation :** Le calcul de *R* et *R2* est exécuté sur les données filtrées pour les variables *X* et *Y*, mais le nuage de points limite l'affichage aux **80% des valeurs centrales** pour réduire l’effet des valeurs extrêmes (choix de notre groupe pour une meilleure lisibilitée).

------------------------------------------------------------------------

## II. Installation et Dépendances

### 1. Prérequis et Installation

| Prérequis | Détail |
|----|----|
| **Logiciel R** | Dernière version de préférence |
| **IDE** | RStudio (recommandé). |
| **Code Source** | Le script intégral doit être enregistré dans un répertoire de travail. |

#### Étapes d'installation locale :

1.  Ouvrir **RStudio**.
2.  Installer les packages nécessaires via la commande :

\`\`\`r install.packages(c( "shiny", "httr", "jsonlite", "DT", "ggplot2", "dplyr", "shinyjs", "shinythemes", "leaflet", "shinyWidgets", "rlang" ))
