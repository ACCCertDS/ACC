#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(lubridate)

server <- function(input, output) {
    france = leaflet(data=(accidents_gps_group %>%
                               filter(year(date_acc) == input$annee)) %>%
                         addTiles() %>%
                         setView(lng=1.7, lat=47, zoom=5) %>%
                         addCircles(~long, ~lat, weight = 0.25))
    output$mymap = renderLeaflet(france)
}
