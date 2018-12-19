#install.packages("shiny")
#install.packages("leaflet")
#install.packages('ipeds', repos=c('http://R-Forge.R-project.org', 'http://lib.stat.cmu.edu/R/CRAN'), dep=TRUE)

library(shiny)
library(ggplot2)
library(dplyr)
library(tools)
library(shinythemes)
library(leaflet)
library(ipeds)
library(shiny)
library(leaflet)
library(shinydashboard)



# Set value for the minZoom and maxZoom settings.
leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))

library(maps)
mapStates = map("state", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)


###



# Make a color palette called pal for the values of `sector_label` using `colorFactor()`  
# Colors should be: "red", "blue", and "#9b4a11" for "Public", "Private", and "For-Profit" colleges, respectively
pal <- colorFactor(palette = c("red", "blue"), 
                   levels = c("Public", "Private"))

# Add circle markers that color colleges using pal() and the values of sector_label
map2 <- 
  map %>% 
  addCircleMarkers(data = RI, radius = 2, 
                   color = ~pal(sector_label), 
                   label = ~paste0(name, " (", sector_label, ")"))

# Print map2
map2

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

names(hd2017)

str(df.latlon1)



library(maps)



library(leaflet.extras)




# Load the htmltools package
library(htmltools)

# Create data frame called public with only public colleges
public <- filter(df.latlon1, School_Type == "Public")  


#---------------


which(names(df.latlon1) == "LONGITUD")
names(df.latlon1)[which(names(df.latlon1) == "LONGITUD")] = "lng"
names(df.latlon1)[which(names(df.latlon1) == "LATITUDE")] = "lat"

names(df.latlon1)
# Load the htmltools package
library(htmltools)
library(RColorBrewer)

# Create data frame called public with only public colleges
public1 <- filter(df.latlon1, School_Type == "Public")  

# Create a leaflet map of public colleges called m3 
m4 <- leaflet() %>% 
  addProviderTiles("CartoDB") %>% 
  addCircleMarkers(data = public1, radius = 2, label = ~htmlEscape(University),
                   color = ~pal(School_Type), group = "Public")

m4


# Create data frame called private with only private colleges
private <- filter(df.latlon1, School_Type == "Private")  

# Add private colleges to `m3` as a new layer
m5 <- m4 %>% 
  addCircleMarkers(data = private, radius = 2, label = ~htmlEscape(University),
                   color = ~pal(School_Type), group = "Private") %>% 
  addLayersControl(overlayGroups = c("Public", "Private"))

m5


m6 <- leaflet() %>% 
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addProviderTiles("Esri", group = "Esri") %>% 
  addCircleMarkers(data = public1, radius = 2, label = ~htmlEscape(University),
                   color = ~pal(School_Type), group = "Public") %>%
  addCircleMarkers(data = private, radius = 2, label = ~htmlEscape(University),
                   color = ~pal(School_Type), group = "Private", clusterOptions = markerClusterOptions()) %>% 
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri"), 
                   overlayGroups = c("Public", "Private")) %>% 
  setView(lat = 39.8282, lng = -98.5795, zoom = 4) 

m6

install.packages("leaflet.extras")
install.packages("shinydashboard")
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
      addCircleMarkers(data = public1, radius = 2, label = ~htmlEscape(University),
                       color = ~pal(School_Type), group = "Public") %>%
      addCircleMarkers(data = private, radius = 2, label = ~htmlEscape(University),
                       color = ~pal(School_Type), group = "Private", clusterOptions = markerClusterOptions()) %>% 
      addLayersControl(baseGroups = c("OSM", "Carto", "Esri"), 
                       overlayGroups = c("Public", "Private")) %>% 
      setView(lat = 39.8282, lng = -98.5795, zoom = 4) 
    
    
  })
}

shinyApp(ui, server)
