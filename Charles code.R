#tryCatch, put expression, when there is an error it can print something



# right before the function, comment all the packages that are used
# make function as general as possible
# call the name of the column rather than the index



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



# Creating column for state that the school is located in
df.copy <- df.final
df.copy$location <- as.character(df.copy$location)
df.copy$state <- substr(df.copy$location, nchar(df.copy$location)-1, nchar(df.copy$location)) 



# -------

# This block of code graphs a map of the United States. 

# BY RANK:Each state is colored based on the highest ranking school in each respective state. Higher ranked states show darker colors while lower ranked states show lighter colors. 

# make into function

df.grouped <- df.copy %>%
  group_by(state) %>%
  mutate(rankLow = min(ranking), schoolCount = n()) %>%
  select(state, rankLow, schoolCount)

df.grouped <- unique(df.grouped)

plot_usmap(data = df.grouped, values = "rankLow", lines = "red") + 
  scale_fill_continuous(name = "Best School Rank", low = "blue", high = "white", label = scales::comma) + 
  theme(legend.position = "right")



# --------
# mapPlot graphs the state that the school is located in

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

cityData <- as.tbl(read.csv("USCities.csv")) %>%
  select(city, state_id, lat, lng) %>%
  rename(latitude = lat, longitude = lng) %>%
  mutate(location = paste(city,", ", state_id, sep = "")) 

df.copy1 <- left_join(df.copy, cityData, by = "location")



states <- map_data("state")

# PlotCity graphs a US and places a point where the inputted school is
PlotCity <- function(schoolName) {
coordinates <- df.copy1 %>%
    filter(universities == schoolName) %>%
    select(longitude, latitude)
coordinates <- unlist(coordinates)

Stats <- df.copy1 %>%
  filter(universities == schoolName)
  
  
schoolPlot <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE) + geom_point(aes(x = coordinates[1], y = coordinates[2])) +
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


# --------









