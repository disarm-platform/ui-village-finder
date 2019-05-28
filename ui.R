library(shiny)
library(raster)
library(leaflet)

# Get country codes data
ccodes <- ccodes()

ui <- bootstrapPage(
  fluidRow(column(8, offset = 2, align = "center",
                  h1("The Village Finder App"))),
  fluidRow(column(7, offset = 1,
                  p("This app suggests GPS coordinates of populated locations based on population density layers. It complements any eventual list of geo-referenced villages provided by the user."),
                  hr(),
                  p("The user needs to specify characteristics of the type of populated locations looked for:"),
                  tags$ul(
                    tags$li("a maximum area size (km",tags$sup("2"),"),", tags$strong("Area_Max"), "above which a region cannot be considered as a unique location"),
                    tags$li("a population threshold", tags$strong("Population_Max"), "above which a location should be counted as a unique location"),
                    tags$li("a population threshold", tags$strong("Population_Min"), "below which a region smaller than Area_Max should not be counted as a populated location.")),
                  hr(),
                  fluidRow(column(9,
                                  p("The user can upload a file with geo-referenced villages in the region of interest."),
                                  tags$small(em("The file needs to be in", tags$strong(".csv"), "format and should contain only 3 columns:", tags$strong("Village_Name"), ",", tags$strong("Longitude"), "and", tags$strong("Latitude"), ". GPS coordinates should be in decimal units."))),
                           column(3,
                                  fileInput(inputId = "User_Input_Village",
                                            label = "Upload CSV File",
                                            accept = c(
                                              "text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv"))))),
           column(3,
                  inputPanel(selectInput(inputId = "Country_selected",
                                         label = "Country",
                                         choices = c("Côte d'Ivoire", "Ghana", "Haiti", "India", "Liberia", "Malawi", 
                                                     "Nigeria", "Philippines", "Swaziland")),
                             numericInput(inputId = "Maximum_Area_Size_Of_Cluster_Km2",
                                          label = "Area_Max",
                                          value = 100),
                             numericInput(inputId = "Max_Number_Of_People_In_Cluster",
                                          label = "Population_Max",
                                          value = 5000),
                             numericInput(inputId = "Min_Number_Of_People_In_Cluster",
                                          label = "Population_Min",
                                          value = 5000),
                             actionButton(inputId = "go",
                                          label = "Update"),
                             conditionalPanel(condition = "input.go > 0",
                                              br(h4("Download results")),  
                                              downloadButton("downloadGeoData", "Download geojson"))))),

  
  absolutePanel(style="opacity: 1; padding: 4px; border-bottom: 1px solid #CCC; background-color: rgba(0,0,0,0.7);",
                imageOutput("logo", height=2, width=3),
                top = 370, right = 164,
                width = 120, height = 40),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  fluidRow(column(10, offset = 1,
                  leafletOutput("myMap", height = 700)))
  
)