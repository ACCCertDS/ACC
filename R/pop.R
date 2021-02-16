
# import des données demographique - source Insee
library(readxl)
population <- read_excel("C:/Users/cecil/Downloads/population.xlsx")

# calcul de la population moyenne par departement
pop_dep <- population %>%
  group_by(dep) %>%
  summarise (pop_moy =  mean(population), .groups = 'drop') %>%
  arrange(desc(pop_moy))

summary(pop_dep)

ggplot(pop_dep) +
  aes(x = dep, y = pop_moy) +
  geom_histogram(stat = "identity", fill = "midnightblue") +
  labs(x = "Département", y = "Nb habitants (en milliers)") +
  theme_light()


# recodage de la variable dep dans la table accidents
library(tidyverse)
library(dplyr)
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
