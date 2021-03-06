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

accidents_gps_group <- accidents_gps_group %>%
                        mutate(ye=year(date_acc))

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
    #accidents_reactive <- reactive(input$annee,{
    #if(input$annee==a2015){
        #accidents_annee <- accidents_gps_group %>%
        #filter(year(date_acc) == 2015)
    #}
    #if(input$annee=="a2016"){
        #accidents_annee <- accidents_gps_group %>%
        #filter(year(date_acc) == 2016)
    #}
    #if(input$annee=="a2017"){
        #accidents_annee <- accidents_gps_group %>%
        #filter(year(date_acc) == 2017)
    #}
    #if(input$annee=="a2018"){
        #accidents_annee <- accidents_gps_group %>%
        #filter(year(date_acc) == 2018)
    #}
    #if(input$annee=="a2019"){
        #accidents_annee <- accidents_gps_group %>%
        #filter(year(date_acc) == 2019)
      #}
  #})
    reactive_annee <- reactive({accidents_gps_group %>%
                                 dplyr::filter(ye %in% input$annee)})

    output$mymap = renderLeaflet({
        leaflet(data = reactive_annee()) %>%
        addTiles() %>%
        setView(lng=1.7, lat=47, zoom=5) %>%
        addCircles(~reactive_annee()$long, ~reactive_annee()$lat, weight = 0.25)
                            #})
    #output$mymap <- renderLeaflet({
      #leaflet(data=accidents_2018) %>%
      #setView(lng=1.7, lat=47, zoom=5) %>%
      #addTiles() %>%
      #addCircles(~long, ~lat, weight = 0.25)
    })
}

shinyApp(ui=ui, server=server)

