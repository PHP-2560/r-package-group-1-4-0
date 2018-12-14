

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






LA <- map_data("usa")

salesCalls <- data.frame(State=rep("louisiana",5), 
                         City=c("Baton Rouge", "New Orleans", "Shreveport", 
                                "Lafayette", "Mandeville"),
                         Calls=c(10,5,8,13,2))

salesCalls <- cbind(geocode(as.character(salesCalls$City), source = "dsk"), salesCalls)

salesCalls
#         lon      lat     State        City Calls
# 1 -91.14032 30.45828 louisiana Baton Rouge    10
# 2 -90.07153 29.95107 louisiana New Orleans     5
# 3 -93.75018 32.52515 louisiana  Shreveport     8
# 4 -92.01984 30.22409 louisiana   Lafayette    13
# 5 -90.06563 30.35825 louisiana  Mandeville     2

ggplot(LA, aes(x=long, y=lat)) +
  geom_polygon() +
  coord_map() +
  geom_point(data =salesCalls, aes(x=lon, y=lat), color="orange")




# blank black USA map
# usa <- map_data("usa") # we already did this, but we can do it again
# ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
#   coord_fixed(1.3)






