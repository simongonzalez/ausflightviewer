#author: Simon Gonzalez
#email: simon.gonzalez@anu.edu.au
#date: 31 October 2019

library(shiny)
library(shinydashboard)
library(jsonify)
library(mapdeck)
library(tidyverse)
library(highcharter)
library(streamgraph)
library(viridis)
library(shinyjqui)
library(shinyalert)
library(shinyBS)
library(shinyWidgets)

ui <- dashboardPage(
  dashboardHeader(title = "AU Flight Viewer",
                  dropdownMenu(badgeStatus = NULL,icon = icon('info'), headerText = 'App creator', type = 'messages',
                               notificationItem(
                                 text = "Simon Gonzalez",
                                 icon("user")
                               ),
                               notificationItem(
                                 text = "www.visualcv.com/simongonzalez/",
                                 icon("link"),
                                 status = "success",
                                 href = 'https://www.visualcv.com/simongonzalez'
                               )
                  )),
  dashboardSidebar(
    bsButton("info1", label = "What's this app?", icon = icon("globe"), style = 'info'),
    selectInput('plotVar', label = 'What do you want to plot?', choices = c('Passengers', 'Mail', 'Freight'), selected = 'Passengers'),
    uiOutput('ausportUI'),
    radioButtons("plotBy", label = 'Plot by Destination or by Country',
                 choices = list("ForeignPort" = "ForeignPort", "Country" = "Country"), 
                 selected = "ForeignPort"),
    uiOutput('foreignPortUI'),
    uiOutput('yearUI'),
    checkboxInput("proportionalSize", label = "Proportional Size", value = TRUE)
  ),
  dashboardBody(
    bsButton("btn1", label = "Map not loading?", icon = icon("exclamation-circle"), style = 'danger'),
    jqui_resizable(mapdeckOutput(
      outputId = 'myMap'
    )),
    streamgraphOutput(outputId = 'yearPlot')
  )
)

server <- function(input, output, session) {
  
  df <- reactive({
    df <- read.csv('airlines.csv')
    
    df$Passengers <- df$Passengers / 5000
    df$Freight <- df$Freight / 1000
    df$Mail <- df$Mail / 1000
    
    return(df)
    
  })
  
  output$ausportUI <- renderUI({
    if(is.null(df()))
      return()
    
    df <- df()
    
    tmpList <- sort(unique(df$AustralianPort))
    
    selectInput('ausport', 'Select Australian Port Location', choices = tmpList, selected = tmpList, multiple = T)
  })
  
  output$foreignPortUI <- renderUI({
    if(is.null(df()))
      return()
    
    if(is.null(input$ausport))
      return()
    
    df <- df()
    
    df <- df[df$AustralianPort %in% input$ausport,]
    
    tmpList <- sort(unique(df[[input$plotBy]]))
    
    selectInput('foreignPort', label = NULL, choices = tmpList, selected = tmpList[1:10], multiple = T)
  })
  
  output$yearUI <- renderUI({
    if(is.null(df()))
      return()
    
    if(is.null(input$ausport))
      return()
    
    if(is.null(input$foreignPort))
      return()
    
    df <- df()
    
    df <- df[df$AustralianPort %in% input$ausport,]
    
    df <- df[df[[input$plotBy]] %in% input$foreignPort,]
    
    tmpList <- sort(unique(df$Year))
    
    sliderInput('year', 'Year of the data collected', min = min(tmpList), max = max(tmpList), step = 1, value = c(min(tmpList), max(tmpList)), animate = T)
  })
  
  output$myMap <- renderMapdeck({
    mapdeck(style = mapdeck_style('dark'), pitch = 35 ) 
  })
  
  observe({
    if(is.null(df()))
      return()
    
    if(is.null(input$ausport))
      return()
    
    if(is.null(input$foreignPort))
      return()
    
    if(is.null(input$year))
      return()
    
    df <- df()
    
    df <- df[df$AustralianPort %in% input$ausport,]
    
    df <- df[df[[input$plotBy]] %in% input$foreignPort,]
    
    df <- df[df$Year >= input$year[1] & df$Year <= input$year[length(input$year)],]
    
    df <- df[df[[input$plotBy]] != 0,]
    
    if(nrow(df) == 0)
      return()
    
    #df <- df %>% group_by()
    
    set_token('pk.eyJ1Ijoic2ltb25nb256YWxleiIsImEiOiJjazJjc3Y4dGUyMXR3M21vNnp6b29xdGQ0In0.9jrdfph8jioWuTA3XXY1UQ')
    
    key <- 'pk.eyJ1Ijoic2ltb25nb256YWxleiIsImEiOiJjazJjc3Y4dGUyMXR3M21vNnp6b29xdGQ0In0.9jrdfph8jioWuTA3XXY1UQ'
    
    if(input$proportionalSize){
      mapdeck_update(map_id = 'myMap') %>%
        add_arc(
          data = df
          , layer_id = "arc_layer"
          , origin = c("aus_lon", "aus_lat")
          , destination = c("for_lon", "for_lat")
          , stroke_from = 'AusCol'
          , stroke_to = 'ForCol'
          , stroke_width = input$plotVar
          , tooltip = 'pair'
          , auto_highlight = T
        )%>%
        add_scatterplot(
          data = df
          , lon = "for_lon"
          , lat = "for_lat"
          , radius = 100000
          , fill_colour = 'ForCol'
          , layer_id = "scatter"
          , fill_opacity = 1
        )
    }else{
      mapdeck_update(map_id = 'myMap') %>%
        add_arc(
          data = df
          , layer_id = "arc_layer"
          , origin = c("aus_lon", "aus_lat")
          , destination = c("for_lon", "for_lat")
          , stroke_from = 'AusCol'
          , stroke_to = input$plotVar
          , tooltip = 'pair'
          , auto_highlight = T
        )%>%
        add_scatterplot(
          data = df
          , lon = "for_lon"
          , lat = "for_lat"
          , radius = 100000
          , fill_colour = 'ForCol'
          , layer_id = "scatter"
          , fill_opacity = 1
        )
    }
    
    
  })
  
  observeEvent(input$info1, {
    sendSweetAlert(
      session = session,
      title = "Australian Flight Viewer",
      text = HTML('This app allows users to see the fligh history from Australian ports to international ones. The data is publically available at https://www.bitre.gov.au/publications/ongoing/airport_traffic_data.aspx'),
      closeOnClickOutside = T, type = 'info'
    )
  })
  
  observeEvent(input$btn1, {
    sendSweetAlert(
      session = session,
      title = "Reload App",
      text = 'If lines are shown but not the map, please reload the app.',
      closeOnClickOutside = T, type = 'warning'
    )
  })
  
}

shinyApp(ui, server)