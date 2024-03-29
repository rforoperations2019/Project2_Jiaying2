library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(DT)
library(stringr)
library(tools)
library(rlist)
library(scales)
library(data.table)
library(rgdal)
library(httr)
library(jsonlite)
library(leaflet.extras)

# Generate URL
url <- URLencode('https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT * from "79ddcc74-33d2-4735-9b95-4169c7d0413d"', repeated = TRUE)
# Send request
get <- GET(url)
# Retrieve results
intersection <- fromJSON(content(get, "text"))$result$records
# Clean data by omit NA values
intersection <- na.omit(intersection)
# load map data
district <- readOGR("PGH_CityCouncilOD.geojson")
# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "City of Pittsburgh Signalized Intersections", titleWidth = 400)

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(width = 250,
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Signalized Intersection Map", icon = icon("map-pin"), tabName = "map"),
    menuItem("Neighborhood Info Chart", icon = icon("bar-chart"), tabName = "neighborhood"),
    menuItem("Operation Type Info Chart", icon = icon("bar-chart"), tabName = "type"),
    menuItem("Table", icon = icon("th"), tabName = "table", badgeLabel = "new", badgeColor = "green"),
    
    # Inputs: choose to add markers by operation type to the map ----------------------------------------------------
    checkboxInput("marker", "Markers by Operation Type", TRUE),
    
    # Inputs: choose to add markers by neighborhood to the map ----------------------------------------------------
    checkboxInput("marker2", "Markers by Neighborhood", FALSE),
    
    # Inputs: select council district to view ----------------------------------------------
    selectInput(inputId = "district",
                label = "Select Council District to View",
                choices = sort(unique(intersection$council_district)),
                selected = "1"),
    
    # Inputs: select operation type to view ----------------------------------------------
    checkboxGroupInput(inputId = "type",
                       label = "Select Operation Type to View",
                       choices = sort(unique(intersection$operation_type)),
                       selected = c("Actuated","Actuated/PED","Fixed","Fixed / Ped Actuated","Fully Actuated", "Semi Actuated","Master")),
    
    # Download Button--------------------------------------------------------
    downloadButton("downloadData", "Download Data for Your Seclection")
  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Map page ----------------------------------------------
  tabItem("map",
          #  ----------------------------------------------
          fluidRow(
            tabBox(width = 12,
                   leafletOutput("intersection_map")
          ))),
  
  # Graph page ----------------------------------------------
  tabItem("neighborhood",
            # Plot ----------------------------------------------
            fluidRow(
              tabBox(width = 12,
                     tabPanel("Number of Signalized Intersections by Neighborhood", plotlyOutput("plot_neighborhood")))
            )),

  # Graph page ----------------------------------------------
  tabItem("type",
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(width = 12,
                   tabPanel("Number of Signalized Intersections by Operation Type", plotlyOutput("plot_type")))
          )),
  
  # Data Table Page ----------------------------------------------
  tabItem("table",
          fluidPage(
            box(title = "Datatable for Your Selection", DT::dataTableOutput("table"), width = 12)))
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "yellow")

# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Reactive data function -------------------------------------------
  intersection.subset <- reactive({
      
    #District Filter
    intersection <- subset(intersection, council_district %in% input$district)
    
    #Type Filter
    intersection <- subset(intersection, operation_type %in% input$type)
    
    # Return dataframe ----------------------------------------------
    return(intersection)
  })
  
  # Basic Map with a layer of district polygons
  output$intersection_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap.HOT", group = "HOT") %>%
      addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
      addPolygons(data = district, color = "navy",group = "Show Council Districts",weight = 2 ) %>%
      setView(-79.978, 40.439, 11) %>%
      addLayersControl(
        baseGroups = c("HOT", "Toner Lite"),
        overlayGroups = "Show Council Districts",
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  # Add circle markers based on operation type
  observe({
    if(input$marker){
      inter = intersection.subset()
      type <- unique(inter$operation_type)
      pal1 <- colorFactor(palette = "RdYlBu", domain = type)

      leafletProxy("intersection_map",data = inter) %>%
        clearGroup("inter") %>%
        removeControl("legend")%>%
        addCircleMarkers(lng = ~longitude ,
                         lat = ~latitude,
                         group ="inter",
                         popup = paste("longitude:",inter$longitude,"latitue:",inter$latitude,"type:",inter$operarion_type),
                         color = ~pal1(type),
                         radius = 1) %>%
        addLegend(position = "topright" ,
                  pal = pal1,
                  values = inter$operation_type,
                  title = "Operation Type",
                  layerId = "legend")
    }
    else{leafletProxy("intersection_map",data = intersection.subset()) %>%
        clearGroup("inter") %>%
        removeControl("legend")}
  })

  # Add circle markers based on neighborhood
  observe({
    if(input$marker2){
      inter = intersection.subset()
      neighborhood <-unique(inter$neighborhood)
      pal2 <- colorFactor(palette = "Paired", domain = neighborhood)

      leafletProxy("intersection_map",data = inter) %>%
        clearGroup("inter2") %>%
        removeControl("legend2")%>%
        addCircleMarkers(lng = ~longitude ,
                         lat = ~latitude,
                         group ="inter2",
                         popup = paste("longitude:",inter$longitude,"latitue:",inter$latitude,"neighborhood:",inter$neighborhood),
                         color = ~pal2(neighborhood),
                         radius = 1)%>%
        addLegend(position = "topright" ,
                  pal = pal2,
                  values = inter$neighborhood,
                  title = "Neighborhood",
                  layerId = "legend2")
    }
    else{leafletProxy("intersection_map",data = intersection.subset()) %>%
        clearGroup("inter2") %>%
        removeControl("legend2")}
  })

  # A plot showing the intersection count by operation type -----------------------------------
  output$plot_type <- renderPlotly({
    ggplot(data = intersection.subset(),
           aes(x = operation_type, fill = operation_type)) +
      geom_bar(stat = 'count') +
      labs (y = "Number of Signalized Intersections", x = "Operation Type", fill = "Operation Type") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # A plot showing the intersection count by neighborhood -----------------------------------
  output$plot_neighborhood <- renderPlotly({
      ggplot(intersection.subset(), 
             aes(x = neighborhood, fill = neighborhood)) +
      geom_bar(stat = 'count') +
      labs (y = "Number of Signalized Intersections", x = "Neighborhood", fill = "Neighborhood") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Data table of Signalized Intersections ----------------------------------------------
  output$table <- DT::renderDataTable({
    ad <-subset(intersection.subset(),select = c(name, neighborhood, description, operation_type, council_district, ward, police_zone, fire_zone, flash_yellow, flash_time, pli_division, public_works_division, id))
    DT::datatable(ad, options = list(scrollX = TRUE))
  })
  
  #Download function
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Pittsburgh Signalized Intersections", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(intersection.subset(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)