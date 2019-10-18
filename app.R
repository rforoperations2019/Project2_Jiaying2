library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(utils)
library(httr)

# Generate URL
url <- URLencode('https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT * from "79ddcc74-33d2-4735-9b95-4169c7d0413d"', repeated = TRUE)
# Send request
get <- GET(url)
# Retrieve results
intersection <- fromJSON(content(get, "text"))$result$records
# Clean data by omit NA values
intersection <-na.omit(intersection)
View(intersection)
# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "City of Pittsburgh Signalized Intersections", titleWidth = 400)

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(width = 250,
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Map", icon = icon("bar-chart"), tabName = "map"),
    menuItem("Chart", icon = icon("bar-chart"), tabName = "counts"),
    menuItem("Table", icon = icon("th"), tabName = "table", badgeLabel = "new", badgeColor = "green"),
    
    # Inputs: select council district to plot ----------------------------------------------
    selectInput(inputId = "district",
                label = "Select Council District",
                choices = sort(unique(intersection$council_district)),
                selected = "1"),
    
    # Inputs: select operation type to plot ----------------------------------------------
    checkboxGroupInput(inputId = "type",
                       label = "Select Operation Type",
                       choices = sort(unique(intersection$operation_type)),
                       selected = c("Actuated","Actuated/PED","Fixed","Fixed / Ped Actuated","Fully Actuated", "Semi Actuated","Master")),
    
    #   # Select sample size ----------------------------------------------------
    numericInput(inputId = "n_samp", 
                 label = "Select number of signalized intercestions to view:", 
                 min = 1, max = nrow(intersection), 
                 value = 508),
    
    # Download Button--------------------------------------------------------
    downloadButton("downloadData", "Download Raw Data")
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
  
  # Chart page ----------------------------------------------
  tabItem("chart",
            # Plot ----------------------------------------------
            fluidRow(
              tabBox(width = 8,
                     tabPanel("Overall Distribution", plotlyOutput("plot_count")))
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

  
  # Data table of Signalized Intersections ----------------------------------------------
  output$table <- DT::renderDataTable({
    ad <-subset(adultInput(), select = c(Reporting.Year, Reporting.Quarter, Reporting.Organization, Certified.Type, County, Certified.Capacity, 
                                         End.Census, Male.Census, Female.Census))
    DT::datatable(ad, options = list(scrollX = TRUE))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)