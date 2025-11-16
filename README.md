# I. Présentation de l'Application

## 1. Objectif Principal

-   **Titre :** DPE – Analyse (Diagnostic de Performance Énergétique ADEME)
-   **But :** L'application est un outil d'analyse interne développé en R/Shiny pour l'exploration et la visualisation des performances énergétiques (DPE, émissions GES, Consommation).
-   **Périmètre :** Les données proviennent de l’API ADEME et se concentrent sur les départements de la Creuse (23), de la Corrèze (19) et de la Haute-Vienne (87).

# II. Accès et Configuration

## 1. Prérequis Techniques

L'application est développée en R et nécessite les packages suivants :

`shiny`, `DT`, `ggplot2`, `leaflet`, `dplyr`, `shinyjs`, `shinythemes`, `httr`, `jsonlite`, `rlang`.

## 2. Démarrage

1.  Enregistrer le code fourni dans un fichier nommé `app.R`.
2.  Lancer l'application dans RStudio.

## 3. Authentification

| Champ             | Valeur |
|:------------------|:-------|
| Nom d'utilisateur | admin  |
| Mot de passe      | admin  |

# III. Structure et Fonctionnalités

## 1. Vue d’ensemble

| Page | Nom de l’Onglet | Contenu de l’Analyse |
|:--------------|:------------------------------------|:--------------------|
| 1 | Contexte et Données Brutes | Contexte du projet, affichage et export (CSV) des données sources (filtrable par département). |
| 2 | Analyse Unidimensionnelle et Temporelle | Distributions des émissions GES, comparaisons Maison vs Appartement, impact de l’année de construction. |
| 3 | Analyse Bi-variée et Géographique | Corrélation/Régression entre variables et cartographie Leaflet des DPE par géolocalisation. |
| 4 | Synthèse Multicritère | Top 10 des communes les plus énergivores et distribution de la consommation par étiquette GES. |

## 2. Fonctionnalités Transversales

-   Thème dynamique (clair/sombre via bouton).
-   Rafraîchissement des données via API ADEME.
-   Export CSV (onglet 1) et export PNG pour les graphiques.

## 3. Informations supplémentaires

-   Le fichier `app.R` contient des commentaires générés par IA, mais le développement a été fait au fur et à mesure. L’objectif était d'avoir une meilleure compréhension et une incrémentation propre.
-   Le dépôt GitHub comporte quatre branches :
    - Branche 'main' dans laquelle vous êtes qui décrit le projet.
    - Branche 'Application' intégrant le code `app.R` et comment le deployer sur ShinyApps.io grâce au fichier 'Deploy.R'
    - Branche 'Documentation' avec les fichiers markdown et le Rmarkdown.
    - Une branche par élève du groupe (trois branches supplémentaires) pour que chacun puisse avancer de son coté selon notre organisation.
-   Un déploiement initial avait été réalisé sur shinyapps.io mais n’a pas été réactualisé en raison de limitations de taille et de librairies. Il donne toutefois une base visuelle du début du projet :\
    <https://diagnosticenedis.shinyapps.io/projet_enedis/>
- Le lien de la vidéo de demonstration se trouve ici : <youtube.com>
