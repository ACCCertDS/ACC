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
                           c("2019" = "a2019",
                             "2018" = "a2018",
                             "2017" = "a2017",
                             "2016" = "a2016",
                             "2015" = "a2015"))
                    ),
        mainPanel(
          h1("Notre application Shiny"),
          leafletOutput("carte")
                  )
              )
          )

server <- function(input, output) {
    france = leaflet(data=reactive({accidents_gps_group %>%
        filter(year(date_acc) == input$annee)})) %>%
                    addTiles() %>%
                    setView(lng=1.7, lat=47, zoom=5) %>%
                    addCircles(~long, ~lat, weight = 0.25)
    output$mymap = renderLeaflet(france)
}

shinyApp(ui=ui, server=server)

