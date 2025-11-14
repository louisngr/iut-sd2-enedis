# I. Présentation de l'Application

## 1. Objectif Principal

-   **Titre :** DPE – Analyse (Diagnostic de Performance Énergétique ADEME)
-   **But :** L'application est un outil d'analyse interne développé en **R/Shiny** pour l'exploration et la visualisation des performances énergétiques (DPE, émissions GES, Consommation).
-   **Périmètre :** Les données proviennent de l'**API ADEME** et se concentrent sur les départements de la **Creuse (23)**, de la **Corrèze (19)** et de la **Haute-Vienne (87)**.

------------------------------------------------------------------------

# II. Accès et Configuration

## 1. Prérequis Techniques

L'application est développée en **R** et nécessite les packages suivants :

`shiny`, `DT`, `ggplot2`, `leaflet`, `dplyr`, `shinyjs`, `shinythemes`, `httr`, `jsonlite`, `rlang`.

## 2. Démarrage

1.  Enregistrer le code fourni dans un fichier nommé **`app.R`**.
2.  Lancer l'application dans RStudio

## 3. Authentification

L'accès à l'interface principale est protégé :

| Champ             | Valeur  |
|:------------------|:--------|
| Nom d'utilisateur | admin |
| Mot de passe      | admin |

------------------------------------------------------------------------

# III. Structure et Fonctionnalités

## 1. Vue d'ensemble

L'interface est organisée autour d'un **barre de navigation** comprenant quatre onglets d'analyse :

| Page | Nom de l'Onglet | Contenu de l'Analyse |
|:----------|:-----------------------|:------------------------------------|
| **1** | Contexte et Données Brutes | Contexte du projet, affichage et export (CSV) des données sources (filtrable par département). |
| **2** | Analyse Unidimensionnelle et Temporelle | Distributions des émissions GES, comparaisons Maison vs. Appartement, impact de l'année de construction sur la consommation. |
| **3** | Analyse Bi-variée et Géographique | Calcul de Corrélation/Régression entre variables et Cartographie interactive (Leaflet) des DPE par géolocalisation. |
| **4** | Synthèse Multicritère | Classement Top 10 des communes les plus énergivores et analyse de la distribution de la consommation par étiquette GES. |

## 2. Fonctionnalités Transversales

-   **Thème Dynamique :** Bascule aisée entre le Mode Clair et le Mode Sombre via le bouton *Thème Blanc/Noir*.
-   **Mise à Jour :** Le bouton *Rafraîchir les Données* permet de recharger un échantillon récent de l'API ADEME.
-   **Exports :** Possibilité d'exporter les données de l'onglet 1 en **CSV** et les graphiques en **PNG**.
