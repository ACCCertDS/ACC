
#' Table unique des accidents
#'
#' @param caracteristiques data.frame obtenu par [dl_caracteristiques()] `%>%` [recod_caracteristiques()]
#' @param usagers data.frame obtenu par [dl_usagers()] `%>%` [recod_usagers()]
#' @param lieux data.frame obtenu par [dl_lieux()] `%>%` [recod_lieux()]
#' @param vehicules data.frame obtenu par [dl_vehicules()] `%>%` [recod_vehicules()]
#'
#' @return Un tibble des différentes tables jointes
#' @export

create_table_accidents <- function(caracteristiques, usagers, lieux, vehicules){
  usagers %>%
    dplyr::select(-secu2, -secu3, -id_vehicule) %>%# Variables crées en 2019, trop de NA avant
    dplyr::left_join(vehicules %>% dplyr::distinct(Num_Acc, num_veh, catv),
              by = c("Num_Acc", "num_veh")) %>%
    dplyr::left_join(caracteristiques, by = c("Num_Acc")) %>%
    dplyr::left_join(lieux,by = c("Num_Acc")) %>%
    dplyr::mutate(age = lubridate::year(date_acc)-an_nais) %>%
    dplyr::select(
           -adr, -com, # utilisation de lat, long et dep
           -voie, -vma, -nbv,
           -hrmn, -an_nais
           )
}
