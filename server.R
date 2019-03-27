#Load packages
library(raster)
library(velox)
library(rgeos)
library(leaflet)
library(tidyverse)
library(shiny)
library(downloader)
library(httr)
library(geojsonio)
library(sf)

# Get links to datasets
data_urls <- read.csv("https://www.dropbox.com/s/amp4qeh2z8cs2hs/worldpop_country_urls.csv?dl=1",
                      header= T)

# Get map
map <- leaflet() %>% addProviderTiles("CartoDB.Positron")

### User input
server <- function(input, output){
  
  # Get country
  ccodes <- ccodes()
  
  gen_request_data <- eventReactive(input$go, {
    
      ISO3 <- ccodes[ccodes$NAME==input$Country_selected, "ISO3"]
    
      filename_raster_pop <- list(filename = paste0(ISO3, "_worldpop_2015_ppp.RData"),
                                  url = as.character(data_urls$URL[data_urls$ISO3==ISO3]))
      
      # List of existing villages
      # User_Input_Villages_DF <- Champasak_Villages <- read_csv("Data/Champasak_Villages_Coordinates_Shinny.csv", col_types = "cdd")
      User_Input_Villages_DF <- Champasak_Villages <- NULL
      if (!is.null(input$User_Input_Village)){
        User_Input_Villages_DF <- Champasak_Villages <- read_csv(User_Input_Village_Table$datapath, col_types = "cdd")
      }
      
      
      return(list(Max_Number_Of_People_In_Cluster = input$Max_Number_Of_People_In_Cluster, # Max number of people defining a populated cluster
                  Min_Number_Of_People_In_Cluster = input$Min_Number_Of_People_In_Cluster, # Minimun number of people defining a populated cluster
                  Maximum_Area_Size_Of_Cluster_Km2 = input$Maximum_Area_Size_Of_Cluster_Km2,
                  filename_raster_pop = filename_raster_pop,
                  User_Input_Villages_DF = User_Input_Villages_DF))
      
      
      
  }, ignoreNULL = FALSE)
  
  
  # Define map
  output$myMap = renderLeaflet({
    
    if(input$go[1]==0){
      return(map %>% setView(0,0,zoom=2))
    }
    
    request_data <- gen_request_data()

    # Send request data to algo
    response <-    
      httr::POST(
        url = "https://faas.srv.disarm.io/function/fn-village-finder",
        body = as.json(request_data),
        content_type_json(),
        timeout(90)
      )
browser()
    # Get content
    response_content <- content(response)
    
    # catch output
    Coordinates_Of_Suggested_Villages <- st_read(as.json(response_content$result$Coordinates_Of_Suggested_Villages))
    Populated_Cluster_PP <- st_read(as.json(response_content$result$Populated_Cluster_PP))
    # map
    
    
    

    
    ############################
    ### Output visualization ###
    ############################
    factpal <- colorFactor(topo.colors(length(unique(Populated_Cluster_PP$Area_Of_Populated_Cluster_km2))), Populated_Cluster_PP$Area_Of_Populated_Cluster_km2)
    
    Populated_Cluster_PP$Population_Of_Populated_Cluster_Label <- paste0(as.character(round(Populated_Cluster_PP$Population_Of_Populated_Cluster, digits = 0)), " habitants")
    
    map <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addPolygons(data = Populated_Cluster_PP,
                  color = ~ factpal(Area_Of_Populated_Cluster_km2),
                  label = ~ Population_Of_Populated_Cluster_Label) %>%
      addLegend(pal = factpal,
                values = Populated_Cluster_PP$Area_Of_Populated_Cluster_km2,
                title = "Area",
                labFormat = labelFormat(suffix = " km2")) 

    if (!is.null(input$User_Input_Villages_DF)){
      map <- map %>%
        addCircleMarkers(data = User_Input_Villages_DF,
                         lng = ~ Longitude, lat = ~ Latitude,
                         stroke = F,
                         radius = 2,
                         color = "orange",
                         fillOpacity = 1,
                         label = ~ Village_Name) %>%
        addLegend(colors = "orange; width:7px; height:7px; border:1px solid orange; border-radius:50%; margin-top: 5px", labels = c("Imported Villages"), opacity = 1)
      }
    
    map <- map %>%
        addCircleMarkers(data = Coordinates_Of_Suggested_Villages,
                         stroke = F,
                         radius = 3,
                         color = "black",
                         fillOpacity = 1,
                         label = "Suggested village") %>%
      addLegend(colors = "black; width:7px; height:7px; border:1px solid black; border-radius:50%; margin-top: 5px", labels = c("Suggested Villages"), opacity = 1)
    
    # TODO : Francois to check
    # if (nrow(User_Input_Villages_Max_Population_In_Populated_Cluster_DF) > 0){
    #   map <- map %>%
    #     addCircleMarkers(data = User_Input_Villages_Max_Population_In_Populated_Cluster_DF,
    #                      lng = ~ Longitude, lat = ~ Latitude,
    #                      stroke = F,
    #                      radius = 3,
    #                      color = "black",
    #                      fillOpacity = 1,
    #                      label = ~ Village_Name) 
    # }
    
    
    map
      
  })
  
  output$logo <- renderImage({
    list(src = "logo_transparent.png")
  }, deleteFile = FALSE)
  
}
