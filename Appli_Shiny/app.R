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

accidents_bis <- accidents_gps %>%
    dplyr::select(Num_Acc, grav, lat, long, date_acc) %>%
    mutate(ye = year(date_acc))

factpal <- colorFactor(c("firebrick","orange","gold","forestgreen"), accidents_bis$grav)

ui <- fluidPage(
    titlePanel("Répartition géographique des accidents de la route"),
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("gravite", "Gravité :",
                               c("Indemne" = "indemne",
                                 "Blessé léger" = "blesseleger",
                                 "Blessé hospitalisé" = "blessehosp",
                                 "Tué" = "tue")),
            radioButtons("annee", "Année :",
                         choices = c("2019",
                                     "2018",
                                     "2017",
                                     "2016",
                                     "2015"))
        ),
        mainPanel(
            h1("Notre application Shiny"),
            leafletOutput("mymap", height=650, width=605)
        )
    )
)

server <- function(input, output) {
    reactive_anneegrav <- reactive({
        accidents_bis %>%
        dplyr::filter(ye %in% input$annee && grav %in% input$gravite)
                            })

    output$mymap = rebderLeaflet({
            leaflet(data=reactive_anneegrav()) %>%
            setView(lng=1.7, lat=47, zoom=5) %>%
            addTiles() %>%
            addCircleMarkers(~reactive_anneegrav()$long,
                             ~reactive_anneegrav()$lat,
                             radius = 0.1,
                             color = ~factpal(grav),
                             stroke = FALSE,
                             fillOpacity = 0.5)
    })
}

shinyApp(ui=ui, server=server)

