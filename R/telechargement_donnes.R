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

#' Téléchargement des données
#'
#'
#' Ces fonctions permettent de télécharger pour les 4 différentes ressources,
#' les données pour différentes années.
#' Les ressources sont :
#'
#' + CARACTERISTIQUES
#' + LIEUX
#' + VEHICULES
#' + USAGERS
#'
#' @return Un `tibble`
#'
#' @examples
#' # Caractéristiques des accidents en 2019
#'
#' \dontrun{
#' caracteristiques <- dl_caracteristiques(2019)
#' usagers <- dl_usagers(2019)
#' vehicules <- dl_vehicules(2015:1019)
#' lieux <- dl_lieux(2019)
#' }
#'
#' @param .annee Un entier ou un vecteur d'entiers pour l'année souhaite
#' @name dl_data
NULL

#' @describeIn dl_data Téléchargement des données
#' @export
dl_caracteristiques <- function(.annee = 2008:2019){
  carac_col_types <-
    readr::cols(
      Num_Acc = readr::col_character(),
      jour = readr::col_integer(),
      mois = readr::col_integer(),
      an = readr::col_integer(),
      hrmn = readr::col_character(),
      lum = readr::col_integer(),
      dep = readr::col_character(),
      com = readr::col_character(),
      agg = readr::col_integer(),
      int = readr::col_integer(),
      atm = readr::col_integer(),
      col = readr::col_integer(),
      adr = readr::col_character(),
      lat = readr::col_character(),
      long = readr::col_character()
    )


  ACC_ressources() %>%
    dplyr::filter(type_fichier=='caracteristiques', annee %in% .annee) %>%
    dplyr::rowwise() %>%
    dplyr::group_map(~ read_acc(.x, carac_col_types)) %>%
    dplyr::bind_rows()
}



#' @describeIn dl_data Téléchargement des données
#' @export
dl_lieux <- function(.annee = 2008:2019){
  carac_col_types <-
    readr::cols(
      Num_Acc = readr::col_character(),
      catr = readr::col_integer(),
      voie = readr::col_character(),
      v1 = readr::col_character(),
      v2 = readr::col_character(),
      circ = readr::col_integer(),
      nbv = readr::col_integer(),
      vosp = readr::col_integer(),
      prof = readr::col_integer(),
      pr = readr::col_character(),
      pr1 = readr::col_character(),
      plan = readr::col_double(),
      lartpc = readr::col_number(),
      larrout = readr::col_number(),
      surf = readr::col_double(),
      infra = readr::col_integer(),
      situ = readr::col_integer(),
      vma = readr::col_number()
    )


  ACC_ressources() %>%
    dplyr::filter(type_fichier=='lieux', annee %in% .annee) %>%
    dplyr::rowwise() %>%
    dplyr::group_map(~ read_acc(.x, carac_col_types)) %>%
    dplyr::bind_rows()
}



#' @describeIn dl_data Téléchargement des données
#' @export
dl_usagers <- function(.annee = 2008:2019){
  carac_col_types <-
    readr::cols(
      Num_Acc = readr::col_character(),
      id_vehicule = readr::col_character(),
      num_veh = readr::col_character(),
      place = readr::col_integer(),
      catu = readr::col_integer(),
      grav = readr::col_integer(),
      sexe = readr::col_integer(),
      an_nais = readr::col_integer(),
      trajet = readr::col_integer(),
      secu = readr::col_character(),
      secu1 = readr::col_character(),
      secu2 = readr::col_integer(),
      secu3 = readr::col_integer(),
      locp = readr::col_integer(),
      actp = readr::col_character(),
      etatp = readr::col_integer()
    )


  ACC_ressources() %>%
    dplyr::filter(type_fichier=='usagers', annee %in% .annee) %>%
    dplyr::rowwise() %>%
    dplyr::group_map(~ read_acc(.x, carac_col_types)) %>%
    purrr::map(~ dplyr::rename_all(.x, recode, secu = 'secu1')) %>%  # https://stackoverflow.com/a/53842689
    dplyr::bind_rows()
}


#' @describeIn dl_data Téléchargement des données
#' @export
dl_vehicules <- function(.annee = 2008:2019){
  carac_col_types <-
    readr::cols(
      Num_Acc = readr::col_character(),
      id_vehicule = readr::col_character(),
      num_veh = readr::col_character(),
      senc = readr::col_integer(),
      catv = readr::col_integer(),
      obs = readr::col_integer(),
      obsm = readr::col_integer(),
      choc = readr::col_integer(),
      manv = readr::col_integer(),
      motor = readr::col_integer(),
      occutc = readr::col_integer()
    )


  ACC_ressources() %>%
    dplyr::filter(type_fichier=='vehicules', annee %in% .annee) %>%
    dplyr::rowwise() %>%
    dplyr::group_map(~ read_acc(.x, carac_col_types)) %>%
    dplyr::bind_rows()
}


read_acc <- function(df, carac_col_types){
  usethis::ui_info("Chargement de {usethis::ui_field(df$title)}")

  Sys.sleep(5) # 5 secondes entre chaque téléchargement pour être poli
  tmp_file <- tempfile()
  curl::curl_download(df$url, tmp_file)
  #from https://stackoverflow.com/a/33417611 détecter le type de séparateur csv
  L <- readLines(tmp_file, n = 1L)


  # Certains fichiers sont même séparés par des tabulations
  if(count.fields(textConnection(L), sep = ";") > 1L)
    data <- readr::read_delim(tmp_file, delim = ";",
                              col_types = carac_col_types,
                              locale = readr::locale("fr", decimal_mark = ".", grouping_mark = ""),
                              na = c('','NA','-','-1', ' -1'))

  if(count.fields(textConnection(L), sep = ",") > 1L)
    data <- readr::read_delim(tmp_file, delim = ",",
                              col_types = carac_col_types,
                              locale = readr::locale("fr", decimal_mark = ".", grouping_mark = ""),
                              na = c('','NA','-','-1', ' -1'))

  if(count.fields(textConnection(L), sep = "\t") > 1L )
    data <- readr::read_delim(tmp_file, delim = "\t",
                              col_types = carac_col_types,
                              locale = readr::locale("fr", decimal_mark = ".", grouping_mark = ""),
                              na = c('','NA','-','-1', ' -1'))

  unlink(tmp_file) # le fichier temporaire n'est plus utile
  data

}
