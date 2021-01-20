#' Title
#'
#' @param caracteristiques Table des caractéristiques "brute"
#'
#' @return Un `data.frame` des caractéristiques avec une sélection de variables
#'   et un recodage de certaines variables avec les libellés correspondants
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  caracteristiques <- dl_caracteristiques()
#'  caracteristiques <- recod_caracteristiques(caracteristiques)
#' }
recod_caracteristiques <- function(caracteristiques){

  caracteristiques <- caracteristiques %>%
    select(-gps) %>%
    mutate(lum=case_when(
      lum==1 ~ "Plein jour",
      lum==2 ~ "Crépuscule ou aube",
      lum %in% c(3,4) ~ "Nuit sans éclairage",
      TRUE ~ "Nuit avec éclairage public")
    ) %>%
    # Ajout des libellés relatifs à l'agglomération
    mutate(agg=ifelse(agg==1,"Hors agglomération","En agglomération")) %>%
    # Ajout des libellés relatifs aux types d'intersection avec regroupement des variables 2, 3, 4, 5 et 9 en "Intersection"
    mutate(intersection=case_when(
      int==1 ~ "Hors intersection",
      int %in% c(2:5,9) ~ "Intersection",
      int==6 ~ "Giratoire",
      int==7 ~ "Place",
      TRUE ~ "Passage à niveau")
    ) %>% select(-`int`) %>%
    # Ajout des libellés relatifs aux conditions atmosphériques
    mutate(atm=case_when(
      atm==1 ~ "Normale",
      atm==2 ~ "Pluie légère",
      atm==3 ~ "Pluie forte",
      atm==4 ~ "Neige - grêle",
      atm==5 ~ "Brouillard - fumée",
      atm==6 ~ "Vent fort - tempête",
      atm==7 ~ "Temps éblouissant",
      atm==8 ~ "Temps couvert",
      TRUE ~ "Autre")
    ) %>%
    # Ajout des libellés relatifs au nombre de véhicules impliqués dans la collision avec regroupement des modalités 4 et 5
    mutate(col=case_when(
      col=='-1' ~ "Non Renseigné",
      col==1 ~ "2 véhicules - frontale",
      col==2 ~ "2 véhicules - par l'arrière",
      col==3 ~ "2 véhicules - par le côté",
      col %in% c(4,5) ~ "3 véhicules et plus",
      col==6 ~ "Autre collision",
      TRUE~ "Sans collision")
    ) %>%
    # Date de l'accident
    mutate(
      annee = ifelse(an<2000L, an+2000, an),
      date_acc = lubridate::ymd(paste(annee, mois, jour, sep = '-'))
    ) %>% select(-an, -annee, -jour, -mois)

  coord_gps_code_commune <- BARIS::BARIS_resources("545b55e1c751df52de9b6045") %>%
    dplyr::filter(stringr::str_detect(title,"Base off")) %>%
    dplyr::pull(id) %>%
    BARIS::BARIS_extract(format = "csv") %>%
    dplyr::filter(coordonnees_gps!="") %>%
    dplyr::distinct(code_commune_insee, coordonnees_gps) %>%
    tidyr::separate(coordonnees_gps, sep = ",", into = c('lat','long'))

  caracteristiques_avec_gps <- filter(caracteristiques, !is.na(lat))
  caracteristiques_sans_gps <- filter(caracteristiques, is.na(lat)) %>% select(-lat,-lon) %>%
    left_join(coord_gps_code_commune, by = c("com"="code_commune_insee"))
  caracteristiques <- bind_rows(caracteristiques_avec_gps, caracteristiques_sans_gps)

  caracteristiques
}
