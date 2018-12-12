#tryCatch, put expression, when there is an error it can print something



# right before the function, comment all the packages that are used
# make function as general as possible
# call the name of the column rather than the index

# given a salary, how many years would it take to pay tuition of university if interest rate is x%, how much you're giving 

# distance between home location and university


#install.packages("maps")
#install.packages("usmap")
#install.packages("ggmap")
library(usmap)
library(maps)
library(ggplot2)
library(dplyr)
library(ggmap)

# rownames(df) = c("University", 
#                  "Year_Founded", 
#                  "Religion", 
#                  "Endowment", 
#                  "School_Type", 
#                  "Median_Start_Sal", 
#                  "Acc_Rate", 
#                  "Stu_Fac_Ratio", 
#                  "Graduation_Rate",
#                  "Score",
#                  "Location",
#                  "Tuition",
#                  "Room_Board",
#                  "Enrollment"
# )


df.copy <- df.final
df.copy$location <- as.character(df.copy$location)
df.copy$state <- substr(df.copy$location, nchar(df.copy$location)-1, nchar(df.copy$location)) 
# Creating column for state that the school is located in


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
  plot_usmap(include = c(state)) +
    labs(title = "US State Visualization", subtitle = paste("This is the state of", state))
  
}

# --------

################################################################################################################

#Parsing location and making a separate column for city
for (i in 1:length(df.copy$location)) {
df.copy$city[i] = strsplit(df.copy$location, split = ",")[[i]][1] 
}

# IMPORTANT: this finds the lat and lon of each city, only run ONCE
# df.copy <- cbind(df.copy, geocode(as.character(df.copy$city), source = "dsk"))

states <- map_data("state")



PlotCity <- function(schoolName) {
coordinates <- df.copy %>%
    filter(universities == schoolName) %>%
    select(lon, lat)
coordinates <- unlist(coordinates)

  
ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE) + geom_point(aes(x = coordinates[1], y = coordinates[2]))

}


#########################################################################################################










