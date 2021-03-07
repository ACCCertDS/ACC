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
            radioButtons("annee", "Année",
                         choices = c("2019",
                                     "2018",
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
                             column(8,plotOutput("graph1", height = 400)),
                             fluidRow(
                                 column(4,tableOutput("chiffresacc")),
                                 column(4,tableOutput("chiffrestues"))
                                 )
                             ),
                             #column(4,tableOutput("chiffrestues"))),
                         fluidRow(
                             column(8,plotOutput("gravacc")),
                             column(4,tableOutput("chiffresgrav")))),
                tabPanel("France",
                         h3("Répartition géographique des accidents par année en fonction de leur gravité"),
                         leafletOutput("mymap", height=650, width=605)),
                tabPanel("Départements")
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
    output$graph1 = renderPlot({
        ggplot(accidents %>%
                   mutate(annee = year(date_acc)) %>%
                   group_by(annee) %>%
                   summarise(nb_accidents = n_distinct(Num_Acc))) +
            aes(x = annee, y = nb_accidents) +
            geom_histogram(stat = "identity", fill = "#440154") +
            labs(x = "Année", y = "Nombre d'accidents",title = "Nombre d'accidents par année") +
            theme_light()
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

