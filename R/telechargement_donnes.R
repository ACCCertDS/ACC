#' Ensemble des ressources du jeu de données Accident
#'
#' Le package `BARIS` permet de récupérer les ressources disponibles sur le site
#' [data.gouv.fr](https://data.gouv.fr/) avec la fonction [BARIS::BARIS_resources]. Seules sont renvoyées
#' les ressources au format `csv`.
#'
#'
#' @seealso [BARIS::BARIS_resources]
#' @return Une table de toutes les ressources incluant l'url pour les télécharger.
#' @export
#'
#' @examples
#' \dontrun{
#' ressources <- ACC_ressources()
#' }
ACC_ressources <- function(){

  #Identifiant data.gouv.fr du jeu de données
  id_donnees <- "53698f4ca3a729239d2036df"

  BARIS::BARIS_resources(id_donnees) %>%
    # csv pour retirer les pdf explicatifs
    # certains fichiers sont agrégés sur certaines années seulement et sont nommés "Base de données..."
    #   On ne souhaite pas garder ces fichiers
    dplyr::filter(format=="csv", ! stringr::str_detect(title,"^Base de donn")) %>%
    # L'année est contenue dans le nom du fichier parfois fichier-annee parfois fichier_annee
    # L'ajout de la variable annee permettra de filtrer
    tidyr::extract(title, c('type_fichier','annee'), "(.*)[[:punct:]]([0-9]{4}).csv", remove = FALSE)
}
