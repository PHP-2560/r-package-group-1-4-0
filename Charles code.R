# plotting a map showing the location of a specficic university
# -> package called map, given name of university, it goes to df and then plots it on a map


#install.packages("maps")
#install.packages("usmap")
library(usmap)
library(maps)
library(ggplot2)
library(dplyr)

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

### EXAMPLES VVV

plot_usmap(regions = "states") + 
  labs(title = "US Counties", subtitle = "This is a blank map of the counties of the United States.") + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"))

plot_usmap(include = c("CA", "ID", "NV", "OR", "WA", "AZ")) +
  labs(title = "Western US States", subtitle = "These are the states in the Pacific Timezone.")

# Color maps with data
plot_usmap(data = statepop, values = "pop_2015")

# Include labels on map (e.g. state abbreviations)
plot_usmap(data = statepop, values = "pop_2015", labels = TRUE)
# Choose color for labels
plot_usmap(data = statepop, values = "pop_2015", labels = TRUE, label_color = "white")



# EXAMPLES ^^^




df.copy <- df.final
df.copy$location <- as.character(df.copy$location)
df.copy$state <- substr(df.copy$location, nchar(df.copy$location)-1, nchar(df.copy$location)) 

# -------
#put dot where school is on map

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

mapPlot <- function(SchoolName) {
  state <- df.copy %>%
    filter(universities == SchoolName) %>%
    select(state)
  plot_usmap(include = c(state)) +
    labs(title = "US State", subtitle = paste("This is the state of", state))
  
}

# --------

#tryCatch, put expression, when there is an error it can print something



# right before the function, comment all the packages that are used
# make function as general as possible
# call the name of the column rather than the index

# given a salary, how many years would it take to pay tuition of university if interest rate is x%, how much you're giving 

# distance between home location and university



