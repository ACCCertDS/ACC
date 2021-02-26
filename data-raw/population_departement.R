## code to prepare `population_departement` dataset goes here

# import des données démographiques - source Insee
population <- readxl::read_excel("data-raw/population.xlsx")

usethis::use_data(population, overwrite = TRUE)
