#' Recodage et sélection de la table caracteristiques
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
    dplyr::select(-gps) %>%
    dplyr::mutate(lum = dplyr::case_when(
      lum==1 ~ "Plein jour",
      lum==2 ~ "Crépuscule ou aube",
      lum %in% c(3,4) ~ "Nuit sans éclairage",
      TRUE ~ "Nuit avec éclairage public")
    ) %>%
    # Ajout des libellés relatifs à l'agglomération
    dplyr::mutate(agg = ifelse(agg==1,"Hors agglomération","En agglomération")) %>%
    # Ajout des libellés relatifs aux types d'intersection avec regroupement des variables 2, 3, 4, 5 et 9 en "Intersection"
    dplyr::mutate(intersection = dplyr::case_when(
      int==1 ~ "Hors intersection",
      int %in% c(2:5,9) ~ "Intersection",
      int==6 ~ "Giratoire",
      int==7 ~ "Place",
      TRUE ~ "Passage à niveau")
    ) %>% dplyr::select(-`int`) %>%
    # Ajout des libellés relatifs aux conditions atmosphériques
    dplyr::mutate(atm = dplyr::case_when(
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
    dplyr::mutate(col = dplyr::case_when(
      col=='-1' ~ "Non Renseigné",
      col==1 ~ "2 véhicules - frontale",
      col==2 ~ "2 véhicules - par l'arrière",
      col==3 ~ "2 véhicules - par le côté",
      col %in% c(4,5) ~ "3 véhicules et plus",
      col==6 ~ "Autre collision",
      TRUE~ "Sans collision")
    ) %>%
    # Date de l'accident
    dplyr::mutate(
      annee = ifelse(an<2000L, an+2000, an),
      date_acc = lubridate::ymd(paste(annee, mois, jour, sep = '-'))
    ) %>% dplyr::select(-an, -annee, -jour, -mois)

  coord_gps_code_commune <- BARIS::BARIS_resources("545b55e1c751df52de9b6045") %>%
    dplyr::filter(stringr::str_detect(title,"Base off")) %>%
    dplyr::pull(id) %>%
    BARIS::BARIS_extract(format = "csv") %>%
    dplyr::filter(coordonnees_gps!="") %>%
    dplyr::distinct(code_commune_insee, coordonnees_gps) %>%
    tidyr::separate(coordonnees_gps, sep = ",", into = c('lat','long'))

  caracteristiques_avec_gps <- dplyr::filter(caracteristiques, !is.na(lat))
  caracteristiques_sans_gps <- dplyr::filter(caracteristiques, is.na(lat)) %>%
    dplyr::select(-lat,-long) %>%
    dplyr::left_join(coord_gps_code_commune, by = c("com"="code_commune_insee"))

  caracteristiques <- dplyr::bind_rows(caracteristiques_avec_gps, caracteristiques_sans_gps)

  caracteristiques
}



#' Recodage et sélection de la table lieux
#'
#' @param lieux Table des lieux d'accidents "brute"
#'
#' @return Un `data.frame` des lieux avec une sélection de variables
#'   et un recodage de certaines variables avec les libellés correspondants
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  lieux <- dl_lieux()
#'  lieux <- recod_lieux(lieux)
#' }

recod_lieux <- function(lieux){
  lieux <- lieux %>%
    #On retire la variable larrout, lartpc, env1 (non ducmentée)
    dplyr::select(-larrout, -lartpc, -env1, -v1, -v2, -vosp, -pr, -pr1) %>%
    dplyr::mutate(catr = dplyr::case_when(
      catr==1 ~ "Autoroute",
      catr==2 ~ "Route Nationale",
      catr==3 ~ "Route Départementale",
      catr==4 ~ "Voie Communale",
      TRUE  ~  "Autre")
    ) %>%
    # Ajout des libellés relatifs au régime de circulation avec regroupement
    # des modalités -1 et 4 en "Autre/Non Renseigné"
    dplyr::mutate(circ = dplyr::case_when(
      circ==1 ~ "A sens unique",
      circ==2 ~ "Bidirectionnelle",
      circ==3 ~ "A chaussées séparées",
      TRUE ~ "Autre/Non Renseigné")
    ) %>%
    # Ajout des libellés relatifs au nombre de voies de circulation avec
    # regroupement des modalités 3 et plus en une seule modalité et regroupement
    # des modalités -1 et 0 en "Non Renseigné"
    dplyr::mutate(nbv = ifelse(
      nbv %in% c(1,2),nbv,
      ifelse(
        nbv >=3, "3 et plus","Non Renseigné"
      )
    )
    ) %>%
    # Ajout des libellés relatifs au profil de la route avec regroupement des
    # modalités -1, 3 et 4 en "Autre/Non Renseigné"
    dplyr::mutate(prof = dplyr::case_when(
      prof==1 ~ "Plat",
      prof==2 ~ "Pente",
      TRUE ~ "Autre/Non Renseigné")
    ) %>%
    # Ajout des libellés relatifs au tracé de la route dans une variable binaire
    #  "Rectiligne/Non rectiligne"
    dplyr::mutate(plan = dplyr::case_when(
      plan==1 ~ "Rectiligne",
      plan %in% c(2,3,4) ~ "Non Rectiligne",
      TRUE ~ "Non Renseigné")
    ) %>%
    # Ajout des libellés relatifs à la surface en faisant 2 modalités :
    # Normale et Anormale.
    dplyr::mutate( surf = dplyr::case_when(
      surf==1 ~ "Normale",
      surf %in% c(2:9) ~ "Anormale",
      TRUE ~ "Non Renseigné")
    ) %>%
    # Ajout des libellés relatifs à la présence d'un aménagement ou d'une
    # infrastructure, regroupement en 3 modalités "Oui/Non/Non Renseigné"
    dplyr::mutate(infra = dplyr::case_when(
      infra==0 ~ "Non",
      infra %in% c(1:9) ~ "Oui",
      TRUE ~ "Non Renseigné")
    ) %>%
    # Ajout des libellés relatifs à la situation de l'accident en regroupant
    # les modalités -1, 6 et 8 en "Autre/Non Renseigné"
    dplyr::mutate(situ = dplyr::case_when(
      situ==0 ~ "Aucun",
      situ==1 ~ "Sur chaussée",
      situ==2 ~ "Sur bande d'arrêt d'urgence",
      situ==3 ~ "Sur accotement",
      situ==4 ~ "Sur trottoir",
      situ==5 ~ "Sur piste cyclable",
      TRUE  ~  "Autre/Non Renseigné")
    )
  lieux
}

#' Recodage et sélection de la table vehicules
#'
#' @param vehicules Table des vehicules d'accidents "brute"
#'
#' @return Un `data.frame` des vehicules avec une sélection de variables
#'   et un recodage de certaines variables avec les libellés correspondants
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  vehicules <- dl_vehicules()
#'  vehicules <- recod_vehicules(vehicules)
#' }
recod_vehicules <- function(vehicules){
  vehicules <- vehicules %>%
    dplyr::select(-c(senc,manv,occutc)) %>% # On retire les variables
    inexploitables
    dplyr::mutate(catv = dplyr::case_when(
      catv==1 ~ "Bicyclette",
      catv %in% c(2,30:34) ~ "2 roues motorisé",
      catv==7 ~ "VL seul",
      catv==10 ~ "VU seul",
      catv %in% c(13:15) ~ "Poids lourd",
      catv %in% c(16:17,20:21) ~ "Engin agricole",
      catv %in% c(37,38) ~ "Bus",
      catv==39 ~ "Train",
      catv==40 ~ "Tramway",
      catv %in% c(35,36) ~ "Quad",
      TRUE ~ "Autre")
    ) %>%
    # Ajout d'une variable binaire "Oui/Non" si un obstacle fixe a été heurté
    dplyr::mutate(obs = dplyr::case_when(
      obs==0 ~ "Non",
      obs %in% c(1:17) ~ "Oui",
      TRUE ~ "Non Renseigné")
    ) %>%
    # Ajout des libellés relatifs à l'obstale mobile heurté avec regroupement en
    #  Aucun/Piéton/Véhicule/Animal/Autre
    dplyr::mutate(obsm = dplyr::case_when(
      obsm==0 ~ "Aucun",
      obsm==1 ~ "Piéton",
      obsm %in% c(2,4) ~ "Véhicule",
      obsm %in% c(5,6) ~ "Animal",
      TRUE ~ "Autre")
    ) %>%
    # Ajout des libellés relatifs au point de choc initial avec regroupement
    #  Avant/Arrière/Côté/Aucun
    dplyr::mutate(choc = dplyr::case_when(
      choc==0 ~ "Aucun",
      choc %in% c(1:3) ~ "Avant",
      choc %in% c(4:6) ~ "Arrière",
      choc %in% c(7,8) ~ "Côté",
      choc==9 ~ "Chocs multiples (tonneaux)",
      TRUE ~ "Non Renseigné")
    )
  vehicules
}


#' Recodage et sélection de la table usagers
#'
#' @param usagers Table des usagers d'accidents "brute"
#'
#' @return Un `data.frame` des usagers avec une sélection de variables
#'   et un recodage de certaines variables avec les libellés correspondants
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  usagers <- dl_usagers()
#'  usagers <- recod_usagers(usagers)
#' }
recod_usagers <- function(usagers){
  usagers <- usagers %>%
    # On retire les variables inexploitables
    dplyr::select(-c(place,trajet,locp,actp,etatp)) %>%
    dplyr::mutate(catu = dplyr::case_when(
      catu==1 ~ "Conducteur",
      catu==2 ~ "Passager",
      catu==3 ~ "Piéton")
    ) %>%
    # Ajout des libellés relatifs à la gravité de blessure de l'usager
    dplyr::mutate(grav = dplyr::case_when(
      grav==1 ~ "Indemne",
      grav==2 ~ "Tué",
      grav==3 ~ "Blessé hospitalisé",
      grav==4 ~ "Blessé léger")
    ) %>%
    # Ajout des libellés relatifs au sexe de l'usager
    dplyr::mutate(sexe = dplyr::case_when(
      sexe==1 ~ "Masculin",
      sexe==2 ~ "Féminin")
    )
  usagers
}
