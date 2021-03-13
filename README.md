
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ACC : Accidents Corporels de la Circulation routière

<!-- badges: start -->

[![R build
status](https://github.com/ACCCertDS/ACC/workflows/R-CMD-check/badge.svg)](https://github.com/ACCCertDS/ACC/actions)
<!-- badges: end -->

Ce package permet de récupérer des données d’accidents de la circulation
routière disponibles sur le site
[data.gouv.fr](https://www.data.gouv.fr/fr/datasets/bases-de-donnees-annuelles-des-accidents-corporels-de-la-circulation-routiere-annees-de-2005-a-2019/).

Les vignettes qui l’accompagnent sont le reflet de l’étude qui a été
menée dans le cadre de ce projet.

## Description des données incluses dans le package

``` r
library(ACC)
data_list <-  list(caracteristiques = caracteristiques,
       usagers = usagers,
       lieux = lieux,
       vehicules = vehicules)
DataExplorer::plot_str(
 data_list
    )
```

![alt text](man/figures/README-intro.png)

## Résultats de notre étude

-   Une première visualisation des données dans la
    `vignette("exploration-donnees-brutes")` qui a permis une
    compréhension des données
-   Une analyse uni et bi-variée des données dans
    `vignette("Stats_Descriptives")` à la suite de laquelle nous avons
    recodé des modalités de certaines variables, et sélectionné d’autres
-   Une classification par ACM, mélanges de modèles et K-modes dans
    `vignette("ACM_classif")`
-   Une modélisation de la gravité des accidents
-   Une visusalisation shiny

## Installation

Vous pouvez installer la dernière version du package sur
[GitHub](https://github.com/) :

``` r
# install.packages("devtools")
devtools::install_github("ACCCertDS/ACC")
```

## Exemple d’utilisation

Un exemple d’utilisation est la récupération des données (à l’aide du
package
[BARIS](https://cran.r-project.org/web/packages/BARIS/index.html)):

``` r
library(ACC)

# Jeux de données disponibles :
ACC_ressources()

# Caracteristiques des accidents pour une année donnée
carac <- dl_caracteristiques(2015L)

# Recodage des modalités
carac <- recod_caracteristiques(carac)

head(carac)
```
