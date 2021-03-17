library(shiny)
library(leaflet)
library(lubridate)
library(ACC)
library(plotly)
library(tidyverse)
library(glmnet)
library(caret)
library(GADMTools)
library(classInt)

data("modele_fin", package="ACC")

data(population, package = "ACC")
data(accidents, package = "ACC")

accidents2 <- mutate_pour_modele(accidents, population) %>%
  mutate(Y = factor(ifelse(
    grav == "Indemne","Indemne",
    "Blessures"))) %>%
  select(-Num_Acc,-grav) %>%
  select(Y, everything()) %>%
  mutate_if(is.character, factor)

accidents$lat <- type.convert(accidents$lat, dec=".")
accidents$long <- type.convert(accidents$long, dec=".")
accidents$lat <- as.numeric(accidents$lat)
accidents$long <- as.numeric(accidents$long)

accidents_gps <- accidents %>%
    mutate(lat = case_when(
        abs(lat) > 100 ~ lat/100000,
        TRUE ~ lat),
        long = case_when(
            abs(long) > 100 ~ long/100000,
            TRUE ~ long))

accidents_bis <- accidents_gps %>%
    dplyr::select(Num_Acc, grav, lat, long, date_acc) %>%
    mutate(ye = year(date_acc))

factpal <- colorFactor(c("orange","forestgreen","gold","firebrick"), accidents_bis$grav)

Fr<-gadm_sf_loadCountries("FRA",level=2)
listNames(Fr,level=2)

# Import d'une table de correspondance Département/Région
regions <- read_csv("departements-region.csv")

# Jointure entre table accidents et regions pour ajouter les départements
accidents$dep <- as.numeric(accidents$dep)
regions$num_dep <- as.numeric(regions$num_dep)
accidents_region <- accidents %>%
  dplyr::left_join(regions, by = c("dep"="num_dep"))

# Regroupement des accidents par département
accidents_par_dept <- accidents_region %>%
  dplyr::select(Num_Acc,dep,dep_name) %>%
  group_by(dep, dep_name) %>%
  summarise(nbre_acc=n())

#####################################################
################ Application Shiny ##################
#####################################################

ui <- fluidPage(
  titlePanel("ACC - Accidents Corporels de la Circulation"),
  sidebarLayout(
    sidebarPanel(
      h3("Filtres"),
      checkboxGroupInput("gravite", "Gravité",
                         choices = c("Indemne",
                                     "Blessé léger",
                                     "Blessé hospitalisé",
                                     "Tué"),
                         selected = c("Blessé hospitalisé","Tué")),
      radioButtons("annee", "Année",choices = c("2018",
                                                "2017",
                                                "2016",
                                                "2015"),
                   selected = ("2018"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Graphes",
                 h3("Quelques statistiques descriptives"),
                 fluidRow(
                   column(8,plotlyOutput("graph1", height = 400)),
                   fluidRow(
                     #column(4,tableOutput("chiffresacc")),
                     column(4,tableOutput("chiffrestues"))
                   )
                   #),
                   #fluidRow(
                   # column(8,plotOutput("gravacc")),
                   #column(4,tableOutput("chiffresgrav")))),
                 )
        ),
        tabPanel("France",
                 h3("Répartition géographique des accidents par année en fonction de leur gravité"),
                 leafletOutput("mymap", height=650, width=605),
                 plotOutput("cartecoloree", height=650, width=605)),
        tabPanel("Application",
                 fluidRow(
                   column(6,
                          h3("Renseigner les données des menus déroulants ci-dessous pour prédiction de la gravité de l'accident"),
                          fluidRow(
                            column(8,
                                   dateInput("date",
                                             label = h3("Date de l'accident"),
                                             value = Sys.Date()),
                                   selectInput(
                                     inputId = "heure",
                                     label = "Heure de l'accident :",
                                     choices = unique(accidents2$cl_heure),

                                     selectize = FALSE
                                   ),
                                   selectInput(
                                     inputId = "dep",
                                     label = "Département lieu de l'accident :",
                                     choices = unique(accidents$dep),
                                     selected = "75",
                                     selectize = FALSE
                                   ),
                                   selectInput(
                                     inputId = "catus",
                                     label = "Catégorie d'usager :",
                                     choices = c("Conducteur", "Piéton", "Passager"),
                                     selected = "Conducteur",
                                     selectize = FALSE
                                   ),
                                   selectInput(
                                     inputId = "sex",
                                     label = "Sexe :",
                                     choices = unique(accidents$sexe),
                                     selected = "Masculin",
                                     selectize = FALSE
                                   ),
                                   selectInput(
                                     inputId = "secu",
                                     label = "Equipement de sécurité :",
                                     choices = na.omit(unique(accidents$secu1)),
                                     selected = "Ceinture",
                                     selectize = FALSE
                                   ),
                                   selectInput(
                                     inputId = "catev",
                                     label = "Catégorie de véhicule :",
                                     choices = unique(accidents$catv),
                                     selected = "VL seul",
                                     selectize = FALSE
                                   ),
                                   selectInput(
                                     inputId = "lumi",
                                     label = "Luminosité :",
                                     choices = unique(accidents$lum),
                                     selected = "Plein jour",
                                     selectize = FALSE
                                   ),
                                   selectInput(
                                     inputId = "agglo",
                                     label = "En agglomération ou non ?",
                                     choices = unique(accidents$agg),
                                     selected = "En agglomération",
                                     selectize = FALSE
                                   ),
                                   selectInput(
                                     inputId = "atmo",
                                     label = "Conditions atmosphériques :",
                                     choices = unique(accidents$atm),
                                     selected = "Normale",
                                     selectize = FALSE
                                   ),
                                   selectInput(
                                     inputId = "coli",
                                     label = "Collision :",
                                     choices = unique(accidents$col),
                                     selected = "Sans collision",
                                     selectize = FALSE
                                   ),
                                   selectInput(
                                     inputId = "inter",
                                     label = "Intersection ?",
                                     choices = unique(accidents$intersection),
                                     selected = "Hors intersection",
                                     selectize = FALSE
                                   ),
                                   selectInput(
                                     inputId = "catroute",
                                     label = "Catégorie de route :",
                                     choices = unique(accidents$catr),
                                     selected = "Voie communale",
                                     selectize = FALSE
                                   ),
                                   selectInput(
                                     inputId = "circu",
                                     label = "Mode de circulation :",
                                     choices = unique(accidents$circ),
                                     selected = "A sens unique",
                                     selectize = FALSE
                                   ),
                                   selectInput(
                                     inputId = "profil",
                                     label = "Profil de la route :",
                                     choices = unique(accidents$prof),
                                     selected = "Plat",
                                     selectize = FALSE
                                   ),
                                   selectInput(
                                     inputId = "plan",
                                     label = "Profil de la route (bis) :",
                                     choices = unique(accidents$plan),
                                     selected = "Rectiligne",
                                     selectize = FALSE
                                   ),
                                   selectInput(
                                     inputId = "surface",
                                     label = "Surface de la route :",
                                     choices = unique(accidents$surf),
                                     selected = "Normale",
                                     selectize = FALSE
                                   ),
                                   selectInput(
                                     inputId = "infr",
                                     label = "Infrastructure :",
                                     choices = unique(accidents$infra),
                                     selected = "Non",
                                     selectize = FALSE
                                   ),
                                   selectInput(
                                     inputId = "situation",
                                     label = "Situation de l'accident :",
                                     choices = unique(accidents$situ),
                                     selected = "Sur chaussée",
                                     selectize = FALSE
                                   ),
                                   selectInput(
                                     inputId = "age",
                                     label = "Age de l'accidenté :",
                                     choices = unique(accidents2$classe_age),
                                   ),
                                   actionButton("submit","Submit")
                            )
                          )
                   ),
                   column(4,
                          h3("Prediction"),
                          tableOutput("prediction"))
                 )
        )
      )
    )
  )
)

server <- function(input, output) {
    reactive_anneegrav <- reactive({
        accidents_bis %>%
            dplyr::filter((ye %in% input$annee) & (grav %in% input$gravite))
    })

    reactive_accident_par_dept <- reactive({
      # Regroupement des accidents par département
      accidents_region %>%
        filter(lubridate::year(date_acc) %in% input$annee) %>%
        dplyr::select(Num_Acc,dep,dep_name) %>%
        group_by(dep, dep_name) %>%
        summarise(nbre_acc=n_distinct(Num_Acc))
    })

    reactive_donnees_ajoutees <- eventReactive(input$submit, {

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

      donnees_inp <- data.frame("catu" = input$catus,
                                "sexe" = input$sex,
                                "secu1" = input$secu,
                                "catv" = input$catev,
                                "lum" = input$lumi,
                                "agg" = input$agglo,
                                "atm" = input$atmo,
                                "col" = input$coli,
                                "intersection" = input$inter,
                                "catr" = input$catroute,
                                "circ" = input$circu,
                                "prof" = input$profil,
                                "plan" = input$plan,
                                "surf" = input$surface,
                                "infra" = input$infr,
                                "situ" = input$situation,
                                "classe_age" = input$age,
                                "jour_acc" = lubridate::wday(lubridate::ymd(input$date), label=TRUE, abbr = FALSE),
                                "mois_acc" = lubridate::month(lubridate::ymd(input$date), label=TRUE, abbr = FALSE),
                                "cl_heure" = input$heure,
                                "dep" = input$dep
      ) %>%
        left_join(pop_dep) %>%
        select(-dep)
      print(donnees_inp)
      donnees_inp
    })

    output$mymap = renderLeaflet({
        leaflet(data = reactive_anneegrav()) %>%
            setView(lng=1.7, lat=47, zoom=6) %>%
            addTiles() %>%
            addCircleMarkers(~reactive_anneegrav()$long,
                             ~reactive_anneegrav()$lat,
                             radius = 0.05,
                             color = ~factpal(grav),
                             #color = "#FA2E02",
                             stroke = TRUE,
                             fillOpacity = 0.5)
    })

    output$cartecoloree = renderPlot({
        choropleth(Fr,
                   data = as.data.frame(reactive_accident_par_dept()),
                   step = 4,
                   value = "nbre_acc",
                   adm.join = "dep_name",
                   breaks = "quantile",
                   palette = "Reds",
                   legend = "Nombre d'accidents",
                   title = paste("Nombre d'accidents par département en ", input$annee))
    })

    output$graph1 = renderPlotly({

        monggplot <- ggplot(accidents %>%
                   mutate(annee = year(date_acc)) %>%
                   group_by(annee, grav) %>%
                   summarise(nb_accidents = n_distinct(Num_Acc))) +
            aes(x = annee, y = nb_accidents, fill = grav) +
            geom_bar(position="stack", stat="identity") +
            # geom_text(nudge_y = 1, label = "") +
            labs(x = "Année", y = "Nombre d'accidents", fill="Gravité", title = "Nombre de personnes accidentées par année") +
            theme_light()

            ggplotly(monggplot)
    })

    #output$chiffresacc = renderTable({
        #accidents %>%
            #mutate(annee = year(date_acc)) %>%
            #group_by(annee) %>%
            #summarise(nb_accidents = n_distinct(Num_Acc)) %>%
            #rename("Année" = annee, "Nombre d'accidents" = nb_accidents)
    #})

    output$chiffrestues = renderTable({
        accidents %>%
            filter(grav == "Tué") %>%
            mutate(annee = year(date_acc)) %>%
            group_by(annee,grav) %>%
            summarise(nb_accidents_tues = n_distinct(Num_Acc)) %>%
            dplyr::select(-grav) %>%
            rename("Année" = annee, "Nombre de décès"=nb_accidents_tues)
    })

    #output$gravacc = renderPlot({
        #ggplot(accidents %>%
                   #mutate(annee=year(date_acc)) %>%
                   #group_by(annee,grav) %>%
                   #summarise(nb_accidents_gravite = n_distinct(Num_Acc))) +
            #aes(x = grav, y = nb_accidents_gravite, fill = grav) +
            #geom_bar(stat = "identity") +
            #scale_fill_viridis_d(option = "inferno") +
            #labs(x = "Gravité", y = "Nombre d'accidents", fill="Gravité", title = "Gravité des accidents ayant eu lieu de 2015 à 2019") +
            #theme_light() +
            #facet_wrap(vars(annee)) +
            #theme(axis.text.x = element_text(angle = 90))
    #})

    #output$chiffresgrav = renderTable({
        #accidents %>%
            #mutate(annee=year(date_acc)) %>%
            #group_by(annee,grav) %>%
            #summarise(nb_accidents_gravite = n_distinct(Num_Acc)) %>%
            #mutate(nb_tot = sum(nb_accidents_gravite)) %>%
            #mutate(prop = (nb_accidents_gravite / nb_tot)*100)
    #})

    output$prediction = renderTable({
        predict(modele_fin, newdata = reactive_donnees_ajoutees(), type="prob")
      # predict(modele_fin, newdata = accidents2[1,], type = "prob")
    })
}

shinyApp(ui=ui, server=server)

