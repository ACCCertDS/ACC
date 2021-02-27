## code to prepare `dl_data` dataset goes here
library(ACC)

caracteristiques <- dl_caracteristiques(2015:2018) %>%
  recod_caracteristiques()

usagers <- dl_usagers(2015:2018) %>%
  recod_usagers()

vehicules <- dl_vehicules(2015:2018) %>%
  recod_vehicules()

lieux <- dl_lieux(2015:2018) %>%
  recod_lieux()

accidents <- create_table_accidents(caracteristiques, usagers, lieux, vehicules)
# usagers %>%
#   left_join(vehicules %>% distinct(Num_Acc, num_veh, catv),
#             by = c("Num_Acc", "num_veh")) %>%
#   left_join(caracteristiques, by = c("Num_Acc")) %>%
#   left_join(lieux,by = c("Num_Acc"))

usethis::use_data(caracteristiques, overwrite = TRUE, compress = "xz")
usethis::use_data(usagers, overwrite = TRUE, compress = "xz")
usethis::use_data(vehicules, overwrite = TRUE, compress = "xz")
usethis::use_data(lieux, overwrite = TRUE, compress = "xz")
usethis::use_data(accidents, overwrite = TRUE, compress = "xz")
