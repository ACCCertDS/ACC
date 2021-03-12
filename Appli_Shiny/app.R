#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(shiny)
library(leaflet)
library(lubridate)
library(ggplot2)
library(ACC)
library(plotly)

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
                                 column(4,tableOutput("chiffresacc")),
                                 column(4,tableOutput("chiffrestues"))
                             )
                         ),
                         fluidRow(
                             column(8,plotOutput("gravacc")),
                             column(4,tableOutput("chiffresgrav")))),
                tabPanel("France",
                         h3("Répartition géographique des accidents par année en fonction de leur gravité"),
                         leafletOutput("mymap", height=650, width=605)),
                tabPanel("Application",
                         h3("Renseigner les données des menus déroulants ci-dessous pour prédiction de la gravité de l'accident"),
                         dateInput("date",
                                   label = h3("Date de l'accident"),
                                   value = Sys.Date()),
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
                             inputId = "heure",
                             label = "Heure de l'accident :",
                             choices = unique(accidents$heure),
                             selected = "17",
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
                         numericInput(
                             inputId = "age",
                             label = "Age de l'accidenté (Entrer un âge entier) :",
                             value = 20
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

    output$graph1 = renderPlotly({

        monggplot <- ggplot(accidents %>%
                   mutate(annee = year(date_acc)) %>%
                   group_by(annee, grav) %>%
                   summarise(nb_accidents = n_distinct(Num_Acc))) +
            aes(x = annee, y = nb_accidents, fill = grav) +
            geom_bar(position="stack", stat="identity") +
            # geom_text(nudge_y = 1, label = "") +
            labs(x = "Année", y = "Nombre d'accidents",title = "Nombre de personnes accidentées par année") +
            theme_light()

            ggplotly(monggplot)
    })

    output$chiffresacc = renderTable({
        accidents %>%
            mutate(annee = year(date_acc)) %>%
            group_by(annee) %>%
            summarise(nb_accidents = n_distinct(Num_Acc)) %>%
            rename("Année" = annee, "Nombre d'accidents" = nb_accidents)
    })

    output$chiffrestues = renderTable({
        accidents %>%
            filter(grav == "Tué") %>%
            mutate(annee = year(date_acc)) %>%
            group_by(annee,grav) %>%
            summarise(nb_accidents_tues = n_distinct(Num_Acc)) %>%
            dplyr::select(-grav) %>%
            rename("Année" = annee, "Nombre de décès"=nb_accidents_tues)
    })

    output$gravacc = renderPlot({
        ggplot(accidents %>%
                   mutate(annee=year(date_acc)) %>%
                   group_by(annee,grav) %>%
                   summarise(nb_accidents_gravite = n_distinct(Num_Acc))) +
            aes(x = grav, y = nb_accidents_gravite, fill = grav) +
            geom_bar(stat = "identity") +
            scale_fill_viridis_d(option = "inferno") +
            labs(x = "Gravité", y = "Nombre d'accidents", fill="Gravité", title = "Gravité des accidents ayant eu lieu de 2015 à 2019") +
            theme_light() +
            facet_wrap(vars(annee)) +
            theme(axis.text.x = element_text(angle = 90))
    })

    output$chiffresgrav = renderTable({
        accidents %>%
            mutate(annee=year(date_acc)) %>%
            group_by(annee,grav) %>%
            summarise(nb_accidents_gravite = n_distinct(Num_Acc)) %>%
            mutate(nb_tot = sum(nb_accidents_gravite)) %>%
            mutate(prop = (nb_accidents_gravite / nb_tot)*100)
    })
}

shinyApp(ui=ui, server=server)

