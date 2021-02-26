#' Modification des variables
#'
#' Cette fonction permet d'appliquer des modifications *finales* à la table
#' accidents comme l'ajout d'une variable supplémentaire sur la taille du
#' département, le regroupement de certaines modalités trop détaillées sur les
#' équipements de sécurité, le regroupement de l'âge en classes d'âges et
#' le regroupement de l'heure de l'accident en heure de pointe (matin et soir) et
#' les autres heures.
#'
#' Ces regroupements ont pu être mis en évidence suite à l'exploration des données,
#' et nous ont parus intéressants.
#'
#' @param accidents table accidents obtenue via [create_table_accidents()]
#' @param population table population obtenue via `data("population", package = "ACC")`
#'
#' @return
#' @export
#'
mutate_pour_modele <- function(accidents, population){
  # calcul de la population moyenne par département
  pop_dep <- population %>%
    dplyr::group_by(dep) %>%
    dplyr::summarise (pop_moy =  mean(population), .groups = 'drop') %>%
    dplyr::arrange(dplyr::desc(pop_moy)) %>%
    dplyr::mutate(gr_dep =
                    dplyr::case_when(
                      0 <= pop_moy & pop_moy < 500000 ~ "moins 500 000 hab",
                      500000 <= pop_moy & pop_moy < 1000000 ~ "500 000 - 1 000 000 hab",
                      100000 <= pop_moy & pop_moy < 2000000 ~ "1 000 000 - 2 000 000 hab",
                      2000000 <= pop_moy  ~ "plus de 2 M hab"             )
    ) %>%
    dplyr::select(-pop_moy)

  # re-codage de la variable secu1 dans la table accidents
  accidents <- accidents %>%
    dplyr::mutate(
      secu1 =
        dplyr::case_when(
          secu1 %in% c("Airbag (2RM/3RM)","Casque", " Gants (2RM/3RM)", "Gants + Airbag (2RM/3RM)", "Gilet réfléchissant", "Gants (2RM/3RM)") ~"Deux roues",
          secu1 %in% c("Ceinture", "Dispositif enfants") ~ "Voiture",
          secu1 %in% c('91','Autre','Non déterminable') ~ "Autre",
          TRUE ~ secu1 ),
      secu1 = (ifelse(catu %in% "Piéton", "Piéton",secu1))
    )

  accidents <- accidents %>%
    dplyr::left_join(pop_dep)

  # passage de l'âge en classe d'âge
  accidents <- accidents  %>%
    dplyr::mutate(classe_age =
                    dplyr::case_when(
                      0 <= age & age < 15 ~ "0-14 ans",
                      15 <= age & age < 24 ~ "15-24 ans",
                      25 <= age & age < 34 ~ "25-34 ans",
                      35 <= age & age < 44 ~ "35-44 ans",
                      45 <= age & age < 54 ~ "45-54 ans",
                      55 <= age & age < 64 ~ "55-64 ans",
                      65 <= age & age < 74 ~ "65-74 ans",
                      75 <= age  ~ "75 ans et +"         )
    ) %>%
    dplyr::select(-age)

  # passage des heures en classes pour heure de pointe/autre
  accidents <- accidents %>%
    dplyr::mutate(cl_heure =
             dplyr::case_when(
               heure %in% c(20,21,22,23,24,0,1,2,3,4,5,6) ~"20h-06h",
               heure %in% c(7,8,9,10) ~ "7h-10h",
               heure %in% c(11,12,13,14,15) ~ "10h-15h",
               heure %in% c(16,17,18,19) ~ "16h-19h"       )
    )%>%
    dplyr::select(-heure)

  accidents <- accidents %>%
    dplyr::mutate(
      lat = as.numeric(stringr::str_replace(lat,",",".")),
      long =  as.numeric(stringr::str_replace(long,",","."))
    )

  accidents %>%
    na.omit() %>%
    dplyr::select(-tidyselect::any_of(c("num_veh","dep","date_acc","dep","age", "lat", "long","dtm_num")))
}
