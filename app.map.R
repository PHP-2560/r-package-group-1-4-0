#install.packages("shiny")
#install.packages("leaflet")
#install.packages('ipeds', repos=c('http://R-Forge.R-project.org', 'http://lib.stat.cmu.edu/R/CRAN'), dep=TRUE)
#install.packages("leaflet.extras")
#install.packages("shinydashboard")

library(shiny)
library(ggplot2)
library(dplyr)
library(tools)
library(shinythemes)
library(leaflet)
library(ipeds)
library(maps)

# Set value for the minZoom and maxZoom settings.
leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))


hd2017 = read.csv("hd2017.csv", header = T)
#joining datasets with dplyr 
hd2017 = hd2017 %>%
  mutate(location = paste(CITY,",", sep = "")) %>%
  mutate(locationfinal = paste(location, STABBR, sep = " "))  %>%
  select(INSTNM, LATITUDE, LONGITUD, locationfinal)

hd2017 = as.tbl(hd2017)
str(hd2017)

unique(hd2017$locationfinal)


df.latlon1 = left_join(df, hd2017, by = c("University" = "INSTNM"))

df.latlon1 = as.tbl(df.latlon1)




names(df.latlon1)[which(names(df.latlon1) == "LONGITUD")] = "lat"
names(df.latlon1)[which(names(df.latlon1) == "LATITUDE")] = "lng"



# Load the htmltools package
library(htmltools)

# Create data frame called public with only public colleges
public <- filter(df.latlon1, School_Type == "Public")  


# Create data frame called private with only private colleges
private <- filter(df.latlon1, School_Type == "Private")  






library(shiny)
library(leaflet)
library(shinydashboard)
library(leaflet.extras)

ui <- fluidPage(
  
             leafletOutput("ShinyAppMap", width="100%", height="500")
                 )
                    

  
server <- function(input, output, session) {

  
  output$ShinyAppMap <- renderLeaflet({
    leaflet() %>% 
      addTiles(group = "OSM") %>% 
      addProviderTiles("CartoDB", group = "Carto") %>% 
      addProviderTiles("Esri", group = "Esri") %>% 
      addCircleMarkers(data = public, radius = 3, label = ~htmlEscape(University),
                       color = ~pal(School_Type), group = "Public") %>%
      addCircleMarkers(data = private, radius = 2, label = ~htmlEscape(University),
                       color = ~pal(School_Type), group = "Private", clusterOptions = markerClusterOptions()) %>% 
      addLayersControl(baseGroups = c("OSM", "Carto", "Esri"), 
                       overlayGroups = c("Public", "Private")) %>% 
      setView(lat = 40.8282, lng = -98.5795, zoom = 4) 
    
  })
  
  
  
}

shinyApp(ui, server)
