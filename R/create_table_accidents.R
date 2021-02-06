
#' Table unique des accidents
#'
#' @param caracteristiques
#' @param usagers
#' @param lieux
#' @param vehicules
#'
#' @return Un tibble des différentes tables jointes
#' @export

create_table_accidents <- function(caracteristiques, usagers, lieux, vehicules){
  usagers %>%
    dplyr::left_join(vehicules %>% dplyr::distinct(Num_Acc, num_veh, catv),
              by = c("Num_Acc", "num_veh")) %>%
    dplyr::left_join(caracteristiques, by = c("Num_Acc")) %>%
    dplyr::left_join(lieux,by = c("Num_Acc")) %>%
    dplyr::select(-secu2, -secu3, # Variables crées en 2019, trop de NA avant
           -adr, -com, # utilisation de lat, long et dep
           -voie, -vma, nbv
           )
}
