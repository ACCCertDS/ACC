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
                               choices = c("Indemne",
                                        "Blessé léger",
                                        "Blessé hospitalisé",
                                        "Tué")),
            radioButtons("annee", "Année :",
                         choices = c("2019",
                                     "2018",
                                     "2017",
                                     "2016",
                                     "2015"))
        ),
        mainPanel(
            leafletOutput("mymap", height=650, width=605)
                )
    )
)

server <- function(input, output) {
    reactive_anneegrav <- reactive({
        accidents_bis %>%
        dplyr::filter((ye %in% input$annee) & (grav %in% input$gravite))
                            })

    reactive_color <- reactive({
        df_color <- data.frame(grav = c("Tué", "Blessé hospitalisé", "Blessé léger", "Tué"),
                               color = NULL)
        if(input$gravite == "Tué"){color = "#FA2E02"}
        if(input$gravite == "Blessé hospitalisé"){color = "#FC6413"}
        if(input$gravite == "Blessé léger"){color = "#F8FC13"}
        if(input$gravite == "Indemne"){color = "#17FC13"}
    })

    output$mymap = renderLeaflet({
            leaflet(data = reactive_anneegrav()) %>%
            setView(lng=1.7, lat=47, zoom=5) %>%
            addTiles() %>%
            addCircleMarkers(~reactive_anneegrav()$long,
                             ~reactive_anneegrav()$lat,
                             radius = 0.1,
                             color = reactive_color(),
                             #color = "#FA2E02",
                             stroke = TRUE,
                             fillOpacity = 0.5)
                                })
                            }

shinyApp(ui=ui, server=server)

