library(ggplot2)
library(tidyverse)
library(rvest)
library(stringr) 
library(rebus)
library(stringi)
library(httr)
library(dplyr)

# removes spaces, new lines, some symbols from all scraped data
clean_str = function(strg) {
  strg = str_remove_all(strg, "\n")
  strg = str_remove_all(strg, " ")
  strg = str_remove_all(strg, regex("[$%+]"))
}

# returns school type
get_school_type = function(info){
  school_type = clean_str(info[1])
  school_type = str_split(school_type, ",", n = 2)
  school_type = school_type[[1]][1]
  return(school_type)
}

# returns year founded
get_year_founded = function(info){
  year = clean_str(info[2])
  year = str_remove_all(year, regex("[a-z]"))
  return(year)
}

# returns religious affiliation
get_religion = function(info){
  religion = str_remove_all(info[3], "\n")
  religion = str_remove_all(religion, "religious affiliation")
  religion = str_remove_all(religion, SPC)
  return(religion)
}

# returns endowment
get_endowment = function(info){
  endowment = str_remove_all(info[6], regex("[a-z]"))
  endowment = str_remove_all(endowment, regex("20[0-9]{2}"))
  endowment = clean_str(endowment)
  # choose unit of million or billion
  if (str_detect(info[6], pattern = "million")) {
    return(endowment)
  } else {
    endowment = as.numeric(endowment)* 1000
    return(endowment)
  }
}

# returns median starting salary for new graduates
get_median_starting_salary = function(info2){
  if (str_detect(info2[1], regex("[0-9]"))) {
    salary = clean_str(info2[1])
    salary = str_remove_all(salary, regex("[$,*]"))
    return(salary)
  } else {
    return(NA)
  }
}

# returns acceptance rate NEEDS WORK
get_acceptance_rate = function(info2){
  lon = 2:9
  accept = NA
  for (i in lon) {
    if (str_detect(info2[i], "%")) {
      accept = info2[2]
      accept = clean_str(info2[i])
      break
    }
  }
  print(accept)
}

# get student faculty ratio
get_stu_fac_ratio = function(info2){
  lon = 9:13
  ratio = NA
  for (i in lon) {
    if (str_detect(info2[i], ":")) {
      ratio = clean_str(info2[i])
      break
    }
  }
  print(ratio)
}

# get 4 year graduation rate
get_grad_rate=function(info){
  lon = 10:14
  grad = NA
  for (i in lon) {
    if (str_detect(info2[i], "%")) {
      grad = clean_str(info2[i])
      break
    }
  }
  print(grad)
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

