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
#' caracteristiques <- dl_caracteristiques()
#' caracteristiques <- recod_caracteristiques(caracteristiques)
#' }
recod_caracteristiques <- function(caracteristiques) {
  caracteristiques <- caracteristiques %>%
    dplyr::select(-tidyselect::any_of("gps")) %>%
    dplyr::mutate(lum = dplyr::case_when(
      lum == 1 ~ "Plein jour",
      lum == 2 ~ "Crépuscule ou aube",
      lum %in% c(3, 4) ~ "Nuit sans éclairage",
      TRUE ~ "Nuit avec éclairage public"
    )) %>%
    # Ajout des libellés relatifs à l'agglomération
    dplyr::mutate(agg = ifelse(agg == 1, "Hors agglomération", "En agglomération")) %>%
    # Ajout des libellés relatifs aux types d'intersection avec regroupement des variables 2, 3, 4, 5 et 9 en "Intersection"
    dplyr::mutate(intersection = dplyr::case_when(
      int == 1 ~ "Hors intersection",
      int %in% c(2:5, 9) ~ "Intersection",
      int == 6 ~ "Giratoire",
      int == 7 ~ "Place",
      TRUE ~ "Passage à niveau"
    )) %>%
    dplyr::select(-tidyselect::any_of("int")) %>%
    # Ajout des libellés relatifs aux conditions atmosphériques
    dplyr::mutate(atm = dplyr::case_when(
      atm == 1 ~ "Normale",
      atm == 2 ~ "Pluie légère",
      atm == 3 ~ "Pluie forte",
      atm == 4 ~ "Neige - grêle",
      atm == 5 ~ "Brouillard - fumée",
      atm == 6 ~ "Vent fort - tempête",
      atm == 7 ~ "Temps éblouissant",
      atm == 8 ~ "Temps couvert",
      TRUE ~ "Autre"
    )) %>%
    # Ajout des libellés relatifs au nombre de véhicules impliqués dans la collision avec regroupement des modalités 4 et 5
    dplyr::mutate(col = dplyr::case_when(
      col == "-1" ~ "Non Renseigné",
      col == 1 ~ "2 véhicules - frontale",
      col == 2 ~ "2 véhicules - par l'arrière",
      col == 3 ~ "2 véhicules - par le côté",
      col %in% c(4, 5) ~ "3 véhicules et plus",
      col == 6 ~ "Autre collision",
      TRUE ~ "Sans collision"
    )) %>%
    # Date de l'accident
    dplyr::mutate(
      annee = ifelse(an < 2000L, an + 2000, an),
      date_acc = lubridate::ymd(paste(annee, mois, jour, sep = "-")),

      mois_acc =  lubridate::month(date_acc, label=TRUE, abbr = FALSE),
      jour_acc = lubridate::wday(date_acc, label=TRUE, abbr = FALSE)
    ) %>%
    # L'heure est parfois 12, 1230, 12:30, il faut tout mettre en HH:MM
    dplyr::mutate(
      hrmn = dplyr::case_when(
        stringr::str_length(hrmn) == 3L ~ paste0('0',hrmn), #632 en 0632
        stringr::str_length(hrmn) == 2L ~ paste0(hrmn,'00'), #12 en 1200
        stringr::str_length(hrmn) == 1L ~ paste0('0',hrmn,'00'), #5 en 0500
        TRUE ~ hrmn
      ),
      hrmn = dplyr::case_when(
        # On rajoute le : entre les heures et les minutes
        stringr::str_length(hrmn) <= 4 ~ stringr::str_replace(hrmn,"(\\d{2})(\\d{2})", "\\1:\\2"),
        TRUE ~ hrmn
      )
    ) %>%
    # Date et heure en secondes depuis le 01/01/2000
    dplyr::mutate(
      dtm_num = lubridate::make_datetime(annee, mois, jour, as.integer(substr(hrmn,1,2)), as.integer(substr(hrmn,4,5)), tz="Europe/Paris"),
      heure = as.factor(lubridate::hour(dtm_num)),
      dtm_num = as.numeric(lubridate::as.duration(dtm_num - lubridate::ymd_hm("20000101 00:00")))
    ) %>%
    dplyr::select(-tidyselect::any_of(c("an", "annee", "jour", "mois"))) %>%
    # Jusqu'en 2018 : Département : Code INSEE du département suivi d'un 0
    # (201 Corse-du-Sud - 202 Haute-Corse)
    dplyr::mutate(
      dep = ifelse(date_acc < '2019-01-01',
                   substr(dep,1L,2L),
                   dep)
    ) %>%
    dplyr::mutate(dep = as.character(dep) ,
                  dep = dplyr::case_when(
                    dep == "1" ~ "01",
                    dep == "2" ~ "02",
                    dep == "3" ~ "03",
                    dep == "4" ~ "04",
                    dep == "5" ~ "05",
                    dep == "6" ~ "06",
                    dep == "7" ~ "07",
                    dep == "8" ~ "08",
                    dep == "9" ~ "09",
                    dep %in% c("971", "972", "973", "974", "975", "976", "977", "978", "986", "987","988")  ~ "97",
                    TRUE ~ dep
                  )) %>%
    # Certaines `com` ne sont pas préfixées du code département, on l'ajoute
    dplyr::mutate(
      com = ifelse(stringr::str_count(com)<5,
                   paste0(dep,com),
                   com)
    )

  coord_gps_code_commune <- BARIS::BARIS_resources("545b55e1c751df52de9b6045") %>%
    dplyr::filter(stringr::str_detect(title, "Base off")) %>%
    dplyr::pull(id) %>%
    BARIS::BARIS_extract(format = "csv") %>%
    dplyr::filter(coordonnees_gps != "") %>%
    dplyr::distinct(code_commune_insee, coordonnees_gps) %>%
    tidyr::separate(coordonnees_gps, sep = ",", into = c("lat", "long"))

  caracteristiques_avec_gps <- dplyr::filter(caracteristiques, !is.na(lat))
  caracteristiques_sans_gps <- dplyr::filter(caracteristiques, is.na(lat)) %>%
    dplyr::select(-lat, -long) %>%
    dplyr::left_join(coord_gps_code_commune, by = c("com" = "code_commune_insee"))

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
#' lieux <- dl_lieux()
#' lieux <- recod_lieux(lieux)
#' }
#'
recod_lieux <- function(lieux) {
  lieux <- lieux %>%
    # On retire la variable larrout, lartpc, env1 (non ducmentée)
    dplyr::select(-tidyselect::any_of(c("larrout", "lartpc", "env1", "v1", "v2", "vosp", "pr", "pr1"))) %>%
    dplyr::mutate(catr = dplyr::case_when(
      catr == 1 ~ "Autoroute",
      catr == 2 ~ "Route Nationale",
      catr == 3 ~ "Route Départementale",
      catr == 4 ~ "Voie Communale",
      TRUE ~ "Autre"
    )) %>%
    # Ajout des libellés relatifs au régime de circulation avec regroupement
    # des modalités -1 et 4 en "Autre/Non Renseigné"
    dplyr::mutate(circ = dplyr::case_when(
      circ == 1 ~ "A sens unique",
      circ == 2 ~ "Bidirectionnelle",
      circ == 3 ~ "A chaussées séparées",
      TRUE ~ "Autre/Non Renseigné"
    )) %>%
    # Ajout des libellés relatifs au nombre de voies de circulation avec
    # regroupement des modalités 3 et plus en une seule modalité et regroupement
    # des modalités -1 et 0 en "Non Renseigné"
    dplyr::mutate(nbv = ifelse(
      nbv %in% c(1, 2), nbv,
      ifelse(
        nbv >= 3, "3 et plus", "Non Renseigné"
      )
    )) %>%
    # Ajout des libellés relatifs au profil de la route avec regroupement des
    # modalités -1, 3 et 4 en "Autre/Non Renseigné"
    dplyr::mutate(prof = dplyr::case_when(
      prof == 1 ~ "Plat",
      prof == 2 ~ "Pente",
      TRUE ~ "Autre/Non Renseigné"
    )) %>%
    # Ajout des libellés relatifs au tracé de la route dans une variable binaire
    #  "Rectiligne/Non rectiligne"
    dplyr::mutate(plan = dplyr::case_when(
      plan == 1 ~ "Rectiligne",
      plan %in% c(2, 3, 4) ~ "Non Rectiligne",
      TRUE ~ "Non Renseigné"
    )) %>%
    # Ajout des libellés relatifs à la surface en faisant 2 modalités :
    # Normale et Anormale.
    dplyr::mutate(surf = dplyr::case_when(
      surf == 1 ~ "Normale",
      surf %in% c(2:9) ~ "Anormale",
      TRUE ~ "Non Renseigné"
    )) %>%
    # Ajout des libellés relatifs à la présence d'un aménagement ou d'une
    # infrastructure, regroupement en 3 modalités "Oui/Non/Non Renseigné"
    dplyr::mutate(infra = dplyr::case_when(
      infra == 0 ~ "Non",
      infra %in% c(1:9) ~ "Oui",
      TRUE ~ "Non Renseigné"
    )) %>%
    # Ajout des libellés relatifs à la situation de l'accident en regroupant
    # les modalités -1, 6 et 8 en "Autre/Non Renseigné"
    dplyr::mutate(situ = dplyr::case_when(
      situ == 0 ~ "Aucun",
      situ == 1 ~ "Sur chaussée",
      situ == 2 ~ "Sur bande d'arrêt d'urgence",
      situ == 3 ~ "Sur accotement",
      situ == 4 ~ "Sur trottoir",
      situ == 5 ~ "Sur piste cyclable",
      TRUE ~ "Autre/Non Renseigné"
    ))
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
#' vehicules <- dl_vehicules()
#' vehicules <- recod_vehicules(vehicules)
#' }
recod_vehicules <- function(vehicules) {
  vehicules <- vehicules %>%
    dplyr::select(-c(senc, manv, occutc)) %>% # On retire les variables inexploitables
  dplyr::mutate(catv = dplyr::case_when(
    catv == 1 ~ "Bicyclette",
    catv %in% c(2, 30:34) ~ "2 roues motorisé",
    catv == 7 ~ "VL seul",
    catv == 10 ~ "VU seul",
    catv %in% c(13:15) ~ "Poids lourd",
    catv %in% c(16:17, 20:21) ~ "Engin agricole",
    catv %in% c(37, 38) ~ "Bus",
    catv == 39 ~ "Train",
    catv == 40 ~ "Tramway",
    catv %in% c(35, 36) ~ "Quad",
    TRUE ~ "Autre"
  )) %>%
    # Ajout d'une variable binaire "Oui/Non" si un obstacle fixe a été heurté
    dplyr::mutate(obs = dplyr::case_when(
      obs == 0 ~ "Non",
      obs %in% c(1:17) ~ "Oui",
      TRUE ~ "Non Renseigné"
    )) %>%
    # Ajout des libellés relatifs à l'obstale mobile heurté avec regroupement en
    #  Aucun/Piéton/Véhicule/Animal/Autre
    dplyr::mutate(obsm = dplyr::case_when(
      obsm == 0 ~ "Aucun",
      obsm == 1 ~ "Piéton",
      obsm %in% c(2, 4) ~ "Véhicule",
      obsm %in% c(5, 6) ~ "Animal",
      TRUE ~ "Autre"
    )) %>%
    # Ajout des libellés relatifs au point de choc initial avec regroupement
    #  Avant/Arrière/Côté/Aucun
    dplyr::mutate(choc = dplyr::case_when(
      choc == 0 ~ "Aucun",
      choc %in% c(1:3) ~ "Avant",
      choc %in% c(4:6) ~ "Arrière",
      choc %in% c(7, 8) ~ "Côté",
      choc == 9 ~ "Chocs multiples (tonneaux)",
      TRUE ~ "Non Renseigné"
    ))
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
#' usagers <- dl_usagers()
#' usagers <- recod_usagers(usagers)
#' }
recod_usagers <- function(usagers) {
  usagers <- usagers %>%
    # On retire les variables inexploitables
    dplyr::select(-c(place, trajet, locp, actp, etatp)) %>%
    dplyr::mutate(catu = dplyr::case_when(
      catu == 1 ~ "Conducteur",
      catu == 2 ~ "Passager",
      catu == 3 ~ "Piéton"
    )) %>%
    # Ajout des libellés relatifs à la gravité de blessure de l'usager
    dplyr::mutate(grav = dplyr::case_when(
      grav == 1 ~ "Indemne",
      grav == 2 ~ "Tué",
      grav == 3 ~ "Blessé hospitalisé",
      grav == 4 ~ "Blessé léger"
    )) %>%
    # Ajout des libellés relatifs au sexe de l'usager
    dplyr::mutate(sexe = dplyr::case_when(
      sexe == 1 ~ "Masculin",
      sexe == 2 ~ "Féminin"
    )) %>%
    #Ajout libellés relatifs aux équipements de sécurité
    dplyr::mutate(
      secu1 = dplyr::case_when(
        is.na(secu1) ~ 'Non déterminable',
        secu1 %in% c(0, 12, 22, 32, 42,92) ~ 'Aucun équipement',
        secu1 %in% c(1,11) ~ 'Ceinture',
        secu1 %in% c(2,21) ~ 'Casque',
        secu1 %in% c(3,31) ~ 'Dispositif enfants',
        secu1 %in% c(4,41) ~ 'Gilet réfléchissant',
        secu1 == 5 ~ 'Airbag (2RM/3RM)',
        secu1 == 6 ~ 'Gants (2RM/3RM)',
        secu1 == 7 ~ 'Gants + Airbag (2RM/3RM)',
        secu1 %in% c(-1,8,13,23,33,43,93) ~ 'Non déterminable',
        secu1 %in% c(4,41) ~ 'Autre'
      )
    )
  usagers
}
