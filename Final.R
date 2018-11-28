library(ggplot2)
library(tidyverse)
library(rvest)
library(stringr) 

get_school_type = function(info){
  school_type = str_replace_all(info[1], "\n", "")
  school_type = str_replace_all(school_type, " ", "")
  school_type = str_split(school_type, ",", n = 2)
  privPub = school_type[[1]][1]
  privPub = str_remove(privPub, "schooltype")
  return(privPub)
}
get_year_founded = function(info){
  year = str_extract(str_replace_all(str_replace_all(info[2], "\n", ""), "  ", ""),regex("[0-9]{4}"))
  return(year)
}
get_religion = function(info){
  religion = str_replace_all(info[3], "\n", "")
  religion = str_replace_all(religion, "   ", "")
  religion = str_split(religion, "  ", n = 3)
  religion_final = religion[[1]][2]
  if (religion_final == "religious affiliation"){
    religion_final = "None"
  }
  return(religion_final)
}
get_endowment = function(info){
  endowment = str_replace_all(info[6], "\n", "")
  endowment = str_split(endowment, "                                ", n = 3)
  endowment = endowment[[1]][2]
  # choose unit of million or billion
  if (str_detect(endowment, pattern = "million")) {
    endowment = str_remove_all(endowment,regex("[a-z$+]"))
    endowment = str_replace_all(endowment, " ", "")
  } else {
    endowment = str_remove_all(endowment,regex("[a-z$+]"))
    endowment = str_replace_all(endowment, " ", "")
    endowment = as.numeric(endowment)* 1000
  }
  return(endowment)
}
get_median_starting_salary = function(info){
  salary = info[1]
  salary = str_remove_all(salary, regex("[$,*]"))
  salary = str_remove_all(salary, regex("\\\n"))
  salary = str_remove_all(salary, regex("/s*"))
  salary = str_extract(salary, regex("/d+"))
  return(salary)
}
get_acceptance_rate=function(info){
  accept = info[8]
  accept = str_remove_all(accept, regex("[%]"))
  accept = str_remove_all(accept, regex("\\\n"))
  accept = str_remove_all(accept, regex("/s*"))
  accept = str_extract(accept, regex("/d+"))
  return(accept)
}
get_stu_fac_ratio=function(info){
  ratio = info[11]
  ratio = str_remove_all(ratio, regex("\\\n"))
  ratio = str_remove_all(ratio, regex("/s*"))
  ratio = str_extract(ratio, regex("/d+:/d+"))
  return(ratio)
}
get_grad_rate=function(info){
  grad = info[12]
  grad = str_remove_all(grad, regex("[%]"))
  grad = str_remove_all(grad, "\\\n")
  grad = str_remove_all(grad, "/s*")
  grad = str_extract(grad, "/s/d{1,2}/s")
  grad = str_extract(grad, "/d+")
  return(grad)
}
get_score=function(details){
  score = details[2]
  score = str_remove_all(score, regex("[a-zA-Z]"))
  score = str_remove_all(score, "/s")
  score = str_split(score, "/", n = 2)
  score = score[[1]][1]
  if(score == ", "){
    score = NA
  }
  return(score)
}
get_location=function(details){
  location = details[3]
}
get_tuition=function(details){
  tuition = details[7]
  tuition = str_split(tuition, " \\(", n = 2)
  tuition = tuition[[1]][1]
  tuition = str_remove_all(tuition, regex("[$,]"))
  return(tuition)
}
get_room_board=function(details){
  rb = details[8]
  rb = str_split(rb, " \\(", n = 2)
  rb = rb[[1]][1]
  rb = str_remove_all(rb, regex("[$,]"))
  return(rb)
}
get_enrollment=function(details){
  enroll = details[9]
  enroll = str_remove_all(enroll, regex("[,]"))
  return(enroll)
}

universities = rep(NA, 312)
links_u = rep(NA, 312)
count = 0
while(TRUE){
  #change url
  count = count + 1
  url = str_c("https://www.usnews.com/best-colleges/rankings/national-universities?_mode=table&amp;_page=", as.character(count))
  tryCatch(webpage <- read_html(url), error = function()break)
  
  #university names
  names = html_text(html_nodes(webpage, "td.full-width > div > a"))
  universities[(sum(!is.na(universities))+1):(sum(!is.na(universities))+length(names))] = names
  
  #links
  semi_links = html_attr(html_nodes(webpage, "div.text-strong.text-large.block-tighter > a"), "href")
  links_u[(sum(!is.na(links_u))+1):(sum(!is.na(links_u))+length(semi_links))] = str_c("https://www.usnews.com", semi_links)
}

year_founded = rep(NA, length(universities))
religion = rep(NA, length(universities))
endowment = rep(NA, length(universities))
school_type = rep(NA, length(universities))
median_start_sal = rep(NA, length(universities))
acc_rate = rep(NA, length(universities))
stu_fac_ratio = rep(NA, length(universities))
grad_rate = rep(NA, length(universities))
score = rep(NA, length(universities))
location = rep(NA, length(universities))
tuition = rep(NA, length(universities))
room_board = rep(NA, length(universities))
enrollment = rep(NA, length(universities))

for (i in 1:length(universities)){
  link = read_html(links_u[i])
  info = html_text(html_nodes(link, ".flex-small"))
  details = html_text(html_nodes(link, ".full-width , strong"))
  info2 = html_text(html_nodes(link, ".medium-end"))
  year_founded[i] = get_year_founded(info)
  religion[i] = get_religion(info)
  endowment[i] = get_endowment(info)
  school_type[i] = get_school_type(info)
  median_start_sal[i] = get_median_starting_salary(info2)
  acc_rate[i] = get_acceptance_rate(info2)
  stu_fac_ratio[i] = get_stu_fac_ratio(info2)
  grad_rate[i] = get_grad_rate(info2)
  score[i] = get_score(details)
  location[i] = get_location(details)
  tuition[i] = get_tuition(details)
  room_board[i] = get_room_board(details)
  enrollment[i] = get_enrollment(details)
}

df = data.frame(universities, 
                year_founded,
                religion,
                endowment,
                school_type,
                median_start_sal,
                acc_rate,
                stu_fac_ratio,
                grad_rate,
                score,
                location,
                tuition,
                room_board,
                enrollment
)

colnames(df) = c("University", 
                 "Year_Founded", 
                 "Religion", 
                 "Endowment", 
                 "School_Type", 
                 "Median_Start_Sal", 
                 "Acc_Rate", 
                 "Stu_Fac_Ratio", 
                 "Graduation_Rate",
                 "Score",
                 "Location",
                 "Tuition",
                 "Room_Board",
                 "Enrollment"
)

df = df %>%
  mutate(Ranking = row_number())

#Visualizations 

df.copy = as.tbl(df) #converting into a tibble
df.copy$endowment = as.numeric(as.character(df.copy$endowment)) #converting endowment into numeric values
df.copy = df.copy %>%
  mutate(ranking = row_number()) #creating ranking values

#pulling final dataframe into a new variable, df.final, where it just pulls the top 200 schools
df.final = head(df.copy, 200) 


#df.final.copy = df.final
#df.final = df.final.copy

#Cleaning up enrollment column to a useable format and then converting it into numeric
df.final$enrollment = gsub(pattern = "(2018-19)" ,replacement = "", df.final$enrollment)
df.final$enrollment = gsub(pattern = "(2017-18)" ,replacement = "", df.final$enrollment)
df.final$enrollment = gsub(pattern = " \\()", replacement = "", df.final$enrollment)
df.final$enrollment = gsub(pattern = "\\$",replacement = "", df.final$enrollment)
df.final$enrollment = gsub(pattern = "\\()", replacement = "", df.final$enrollment)


df.final$enrollment = as.numeric(as.character(df.final$enrollment)) #converting enrollment column into numeric
df.final$tuition = as.integer(as.character(df.final$tuition)) #converting tuition column into integer

# 1st Visualization: Scatterplot of National Rank Vs Tuition, colored by type of school (private or public), size of point based on enrollment
Plot1 <- ggplot(df.final, aes(x = desc(ranking), y = tuition)) + 
  geom_point(aes(col = school_type)) +
  xlab("National Rank of University or College") +
  ylab("School Tuition") +
  labs(title = "Scatterplot of United States Universities and Colleges", 
       subtitle = "National Rank vs Tuition") + 
      geom_smooth(aes(col = school_type),formula = y ~ x) 
      
Plot1
# Within this scatterplot, two very unique observations can be concluded. First, there is a large disparity between
# public and private school tuitions. The private school tuitions are two to three times larger compared to the public
# school tuitions. Secondly, and more significantly, As the ranking increasingly approaches the top schools near ranks of 1 to 20,
# you can see an upward trend in tuition. This observation allows us to conclude that the higher ranked schools
# are more expensive and they charge more.

#2nd visualization - boxplot
Plot2 <- ggplot(df.final, aes(x = school_type, y = tuition)) +
  geom_boxplot() + geom_jitter(alpha = 0.3) + xlab("School Type") +
  ylab("School Tuition in $") 
Plot2

#As we can clearly see with the two boxplots, private school tuition is significantly higher
# than public school tuition. The outliers for the public school tuitions don't even each the minimum
# tuition for private schools. As the boxplots show, Private school tuitions are consistently 
# two or three time larger compared to public school tuitions.


# 3rd visualization - Diverging Bars (bar chart) 


# Diverging Bar Chart of several public schools based on normalized endowment
# A value of 0 represents the average endowment for the public schools in the data frame.
# A value of 1 represents an endowment that is 1 standard deviation above the average endowment
# A value of -1 represents an endowment that is 1 standard deviation below the average endowment

df.public = df.final %>%
  filter(school_type == "Public") %>% # filtering df to show only public schools
  mutate(meanEndow = mean(endowment, na.rm = T)) %>% #calculating the mean, sd, and normalized endowment and adding it to the new df
  mutate(sdEndow = sd(endowment, na.rm = T)) %>%
  mutate(endow_z = (endowment-meanEndow)/sdEndow) %>%
  mutate(type = ifelse(endow_z < 0, "below", "above")) %>%
  select(universities, endowment, endow_z, type) %>%
  arrange(desc(endowment))



df.public = head(df.public, 100)

#coverting the columns into proper classes
df.public$endowment = as.integer(df.public$endowment)
df.public$NormEndow = as.integer(df.public$NormEndow)
df.public$universities = as.character(df.public$universities)


#Visualization in ggplot
theme_set(theme_bw())  
Plot3 <- ggplot(df.public[c(15:41),], aes(x = reorder(universities, endow_z), y = endow_z, label = endow_z)) + #ordered by endowment value from greatest to least
  geom_bar(stat = 'identity', aes(fill = type), width = 0.5) + 
  xlab("Public Universities") +
  ylab("Normalized Endowment") +
  scale_fill_manual(name = "Endowment",
                    labels = c("Above Average", "Below Average"),
                    values = c("above"="#00ba38", "below"="#f8766d")) +
  labs(subtitle = "Normalized Endowment from United States Public Schools",
       title = "Diverging Bar Chart") +
  coord_flip()

Plot3

# As shown in the graph, public schools like Penn State has an endowment that is roughly 0.5 standard deviations higher than the average endowment.
# University of California-Irvine has an endowment that is roughly 0.2 standard deviations below the mean endowment.


# Plot4 

#str(df.final)
df.final$year_founded = as.integer(as.character(df.final$year_founded)) #data cleaning, changing from factor to numeric

# Plot4 

#str(df.final)
df.final$year_founded = as.integer(as.character(df.final$year_founded)) #data cleaning, changing from factor to numeric


Plot4 = ggplot(data = df.final, aes(x = year_founded, y = endowment, col = school_type)) + 
  geom_point() +xlab("Year the School was Founded") + ylab("School Endowment") +
  geom_smooth(aes(col = school_type), se = F) + labs(title = "Scatterplot of Year Founded vs Endowment")

Plot4

#This visualization offers a particularly interesting observation. It appears as though for the private schools
#that there is a sharp increase in endowment within the school once the school founding date reaches below a certain threshold.
#Roughly any school prior to 1800 indicates a level of high, prosperous endowment.


# Plot5

first_ten = sum(df.final$endowment[1:10])
last_190 = sum(df.final$endowment[11:200], na.rm = T)

sum = first_ten + last_190 #creating sum

bar_chart_data = data.frame(first_ten, last_190) / sum #converting to percentages

bar_chart_data = bar_chart_data %>% #manipulating dataframe 
  gather()

colnames(bar_chart_data) = c("group", "summed_endowment") #naming the columns

Bar_plot = ggplot(bar_chart_data, aes(x="", y= summed_endowment, fill = group)) +
  geom_bar(width = 1, stat = "identity")

pie_chart <- Bar_plot + coord_polar("y", start=0) + 
  xlab("") + ggtitle("Pie Chart of total Endowment for the first 10 ranked schools vs the last 190 ranked schools")

pie_chart


#This pie_chart shows a shocking disparity between wealth. It clearly shows that nearly 10 out of all 200 schools 
# hold wealth for nearly half of all of the schools endowments combined! Roughly sitting around 40%, the top 10
# universities in the ranking around for almost half of all the total money at these schools.








