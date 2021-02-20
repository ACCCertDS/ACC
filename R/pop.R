library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)


# import des données démographiques - source Insee
population <- read_excel("C:/Users/cecil/Downloads/population.xlsx")


# calcul de la population moyenne par departement
pop_dep <- population %>%
  group_by(dep) %>%
  summarise (pop_moy =  mean(population), .groups = 'drop') %>%
  arrange(desc(pop_moy))

summary(pop_dep)

# représentation graphique de la population moyenne par département
ggplot(pop_dep) +
  aes(x = dep, y = pop_moy) +
  geom_histogram(stat = "identity", fill = "midnightblue") +
  labs(x = "Département", y = "Nb habitants (en milliers)") +
  theme_light()

# recodage de la variable dep dans la table accidents
accidents <- accidents %>%
  mutate(accidents, dep =as.character(dep) , dep = case_when(
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
  ))

# recodage de la variable secu1 dans la table accidents
accidents <- accidents %>%
  mutate(
    secu1 =
      case_when(
        secu1 %in% c("Airbag (2RM/3RM)","Casque", " Gants (2RM/3RM)", "Gants + Airbag (2RM/3RM)", "Gilet réfléchissant", "Gants (2RM/3RM)") ~"Deux roues",
        secu1 %in% c("Ceinture", "Dispositif enfants") ~ "Voiture",
        secu1 %in% c('91','Autre','Non déterminable') ~ "Autre",
        TRUE ~ secu1 ),
    secu1 = (ifelse(catu %in% "Piéton", "Piéton",secu1))
  )


# ajout de la population moyenne par département dans la base accidents
accidents2 <- accidents %>%
  left_join(pop_dep)

# calcul du nb d'accidents par départements
gr_acc_dep <- accidents2 %>%
  mutate(annee = year(date_acc)) %>%
  group_by(annee, dep)  %>%
  summarise(nb_acc_hab = n_distinct(Num_Acc)/pop_moy*1000) %>%
  arrange(desc(nb_acc_hab))

gr_acc_dep <- gr_acc_dep %>% na.omit()

ggplot(gr_acc_dep) +
  aes(x = dep, y = nb_acc_hab) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  labs(x = "Département", y = "nb accidents" ,title = "Nb accidents pour 1 000 habitants") +
  theme_light()+
  facet_wrap(vars(annee))


# passage des départements en classes selon la population moyenne
accidents2 <- accidents2  %>%
  mutate(gr_dep =
           case_when(
             0 <= pop_moy & pop_moy < 500000 ~ "moins 500 000 hab",
             500000 <= pop_moy & pop_moy < 1000000 ~ "500 000 - 1 000 000 hab",
             100000 <= pop_moy & pop_moy < 2000000 ~ "1 000 000 - 2 000 000 hab",
             2000000 <= pop_moy  ~ "plus de 2 M hab"             )
  )


# passage de l'âge en classe d'âge
accidents2 <- accidents2  %>%
  mutate(classe_age =
           case_when(
             0 <= age & age < 15 ~ "0-14 ans",
             15 <= age & age < 24 ~ "15-24 ans",
             25 <= age & age < 34 ~ "25-34 ans",
             35 <= age & age < 44 ~ "35-44 ans",
             45 <= age & age < 54 ~ "45-54 ans",
             55 <= age & age < 64 ~ "55-64 ans",
             65 <= age & age < 74 ~ "65-74 ans",
             75 <= age  ~ "75 ans et +"         )
  )

# passage des heures en classes
accidents2 <- accidents2 %>%
  mutate(cl_heure =
           case_when(
             heure %in% c(20,21,22,23,24,0,1,2,3,4,5,6) ~"20h-06h",
             heure %in% c(7,8,9,10) ~ "7h-10h",
             heure %in% c(11,12,13,14,15) ~ "10h-15h",
             heure %in% c(16,17,18,19) ~ "16h-19h"       )
  )
