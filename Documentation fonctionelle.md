# Documentation Fonctionnelle – Application DPE ADEME

## 1. Objectif de l’application

L’application a pour but d’analyser et de visualiser les **performances énergétiques** des bâtiments à partir des données publiques de l’**ADEME**.\
Elle permet aux utilisateurs (étudiants, chercheurs, collectivités) de : - Explorer les données DPE locales,\
- Visualiser les consommations et émissions,\
- Comparer les départements et communes,\
- Identifier les tendances selon le type et l’ancienneté du logement.

------------------------------------------------------------------------

## 2. Authentification

L’accès est restreint :\
**Nom d’utilisateur :** `admin`\
**Mot de passe :** `admin`

> Une fois connecté, l’utilisateur accède à la navigation principale via une barre d’onglets (`navbarPage`).

------------------------------------------------------------------------

## 3. Structure fonctionnelle de l’application

### Page 1 — Contexte et Données Brutes

-   Présente le **contexte du projet** et la **source ADEME**.\
-   Permet d’afficher et d’exporter les **données brutes** :
    -   Tous les départements ou un seul département.\
    -   Tableau interactif (filtrable et triable).\
    -   Export possible au format **CSV**.

------------------------------------------------------------------------

### Page 2 — Analyse Unidimensionnelle et Temporelle

-   Visualisation de la **distribution des émissions GES** (histogramme).\
-   Comparaison **Maisons vs Appartements** (densités).\
-   Étude de la **consommation énergétique moyenne selon l’année de construction** :
    -   Barres (moyenne) ou boxplots (dispersion).\
-   Export des graphiques au format **PNG**.

------------------------------------------------------------------------

### Page 3 — Analyse Bi-variée et Géographique

-   Analyse de **corrélation et régression linéaire** entre deux variables numériques.\
-   Affichage du **coefficient de corrélation (R)** et du **R²**.\
-   Carte **Leaflet interactive** des DPE :
    -   Points colorés selon l’étiquette DPE (A → G).\
    -   Pop-up : département, commune, émission GES.\
-   Comparaison visuelle **entre deux départements** (histogrammes DPE).

------------------------------------------------------------------------

### Page 4 — Synthèse Multicritère

-   Classement **Top 10 des communes** les plus énergivores.\
-   Analyse de la **consommation par étiquette GES** (boxplot).\
-   Possibilité de **filtrer les valeurs extrêmes** via un slider interactif.

------------------------------------------------------------------------

## 4. Fonctionnalités transversales

| Fonctionnalité | Description |
|----|----|
| **Mode clair/sombre** | Bouton *Thème Blanc/Noir* (géré par `shinyjs`) |
| **Rafraîchissement API** | Bouton pour recharger les données ADEME en direct |
| **Exports** | PNG pour les graphiques, CSV pour les tableaux |
| **Thèmes graphiques adaptatifs** | `ggplot2` et CSS ajustés selon le thème actif |
| **Messages de debug et de sécurité** | Gestion propre des erreurs API et des données manquantes |

------------------------------------------------------------------------

## 5. Intérêt et valeur ajoutée

-   Application **clé en main** pour explorer les performances énergétiques à l’échelle locale.\
-   Permet de **croiser indicateurs énergétiques et territoriaux**.\
-   Interface ergonomique, **interactive** et **multithématique** (GES, consommation, géographie, typologie).\
-   Utilisable en **enseignement**, **diagnostic territorial** ou **projet de data science**.

------------------------------------------------------------------------

## 6. Données et sources

-   **Source principale :** ADEME – Dataset *dpe03existant*\
      <https://data.ademe.fr/>\
-   Données dynamiques, rafraîchies à chaque lancement ou sur demande utilisateur.

------------------------------------------------------------------------

## 7. Résumé des pages

| Page | Nom | Fonction principale |
|----|----|----|
| 1 | Contexte et Données | Chargement, affichage et export CSV des données |
| 2 | Analyse Unidimensionnelle | Histogrammes et densités GES / consommation |
| 3 | Analyse Bi-variée & Carte | Corrélations, régressions et cartographie interactive |
| 4 | Synthèse Multicritère | Classements, comparaisons et visualisations avancées |
