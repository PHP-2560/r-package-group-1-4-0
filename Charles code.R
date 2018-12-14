
#install.packages("maps")
#install.packages("usmap")
#install.packages("ggmap")
#install.packages("gridExtra")
library(usmap)
library(maps)
library(ggplot2)
library(dplyr)
library(ggmap)
library(gridExtra)



# --------

# Creating column for state that the school is located in
df.copy <- df.final
df.copy$location <- as.character(df.copy$location)
df.copy$state <- substr(df.copy$location, nchar(df.copy$location)-1, nchar(df.copy$location)) 


#' mapPlot
#'
#' Graphs the an outline of the state that the inputted school is located in for the top 200 nationally ranked universities or colleges in the United States.
#'
#' @param SchoolName The name of the college or university being plotted for the respective state.
#' @return A map of a single state in the US with an abbreviated label of the state pasted in the interior. 
#' @examples
#' mapPlot(schoolName = "Brown University")
#' mapPlot(schoolName = "Princeton University")
#' @import tidyverse, usmap, ggplot2

mapPlot <- function(SchoolName) {
  state <- df.copy %>%
    filter(universities == SchoolName) %>%
    select(state)
  plot_usmap(include = c(state), labels = T) +
    labs(title = "US State Close-up")
  
}


# --------


#Parsing location and making a separate column for city
for (i in 1:length(df.copy$location)) {
df.copy$city[i] = strsplit(df.copy$location, split = ",")[[i]][1] 
}

# reading in a csv file with thousands of cities in the US and their respective geographical coordinates
cityData <- as.tbl(read.csv("USCities.csv")) %>%
  select(city, state_id, lat, lng) %>%
  rename(latitude = lat, longitude = lng) %>%
  mutate(location = paste(city,", ", state_id, sep = "")) 

# Getting the coordinates of each school by joining the dataframe with the csv file
df.copy1 <- left_join(df.copy, cityData, by = "location")

states <- map_data("state")

#' PlotCity
#'
#' Graphs a map of the United States and places a black dot where the inputted school is geographically located. 
#'
#' @param schoolName The name of the college or university that is being plotted.
#' @return A map of the United States with a black dot of the college or university. A few summary statistics are given underneath the graph, such as whether the school is public or private, the school rank, location, and annual tuition.
#' @examples
#' PlotCity(schoolName = "Brown University")
#' PlotCity(schoolName = "Princeton University")
#' @import tidyverse, ggmap, ggplot2, gridExtra

PlotCity <- function(schoolName) {
coordinates <- df.copy1 %>%
    filter(universities == schoolName) %>%
    select(longitude, latitude)
coordinates <- unlist(coordinates)
# this grabs the correct coordinates for the user-inputted school


Stats <- df.copy1 %>%
  filter(universities == schoolName)
  
schoolPlot <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE) + geom_point(aes(x = coordinates[1], y = coordinates[2]), size = 3) +
  labs(title = paste("US School Location of", schoolName)) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

SummaryTable <- data.frame(Stats$universities, Stats$school_type, Stats$ranking, Stats$location, Stats$tuition)
names(SummaryTable) <- c("School Name", "School Type", "Ranking", "Location", "Tuition")

tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
tbl <- tableGrob(SummaryTable, rows=NULL, theme=tt)
# Plot chart and table into one object
grid.arrange(schoolPlot, tbl,
             nrow=2,
             as.table=TRUE,
             heights=c(3,1))

}






