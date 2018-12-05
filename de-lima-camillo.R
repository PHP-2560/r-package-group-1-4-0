library(twitteR)
library(tibble)
library(tidytext)
library(stringr)
library(base64enc)

check_packages = function(names) {
  for(name in names) {
    if (!(name %in% installed.packages())){
      install.packages(name, repos="http://cran.us.r-project.org")
    }
    library(name, character.only=TRUE)
  }
}




#TWITTER
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tw = twitteR::searchTwitter('Harvard', n = 1e3, since = '2018-09-08', retryOnRateLimit = 1e4, lang = "en")
d = twitteR::twListToDF(tw)
d = as_tibble(d)

#this function sets up the twitter api. The parameters are optional and I'm using the ones from my twitter account
setup_tweets = function(consumer_key = "kmp7IUYAhZQftCrYkFSICCjuz",
                        consumer_secret = "EMtBaYhcYV7V3vAEmSMSvApaSDJfj101fCD5TYAjicf4Z3ncy6",
                        access_token = "1060241795240599557-Qyvx7TY0kYQ2ovPLYnMi6E4GNgirko",
                        access_secret = "Q0AEBJvFx8kAh1p5tOCFppWppT12Mm8iEEj0zaf7dFvxL"){
  check_packages(c("twitteR", "base64enc"))
  invisible(capture.output(setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)))
}

#This function gets the positivity associate with the tweets related to a word from -4 to 4. There are some parameters and prints a ttest.
get_twitter_feelings = function(string, n_tweets = 20, since_date = Sys.Date() - 30, ttest = TRUE){
  setup_tweets()
  check_packages(c("tidytext", "stringr", "twitteR", "utf8"))
  twt = twitteR::searchTwitter(string, n = n_tweets, since = as.character(since_date), retryOnRateLimit = 1e4, lang = "en") 
  df_tw = twitteR::twListToDF(twt)
  words = vector()
  for (i in 1:nrow(df_tw)){
    text = vector()
    count = 1
    while(!is.na(word(df_tw$text[i], count))){
      text[count] = str_extract(word(df_tw$text[i], count), regex("[A-Za-z]+"))
      count = count + 1
    }
    words = append(words, text)
  }
  afinn = get_sentiments("afinn")
  scores = vector()
  count = 1
  for (i in 1:length(words)){
    if (words[i] %in% afinn$word) {
      scores[count] = afinn$score[which(afinn$word == words[i])]
      count = count + 1
    }
  }
  p_value = t.test(scores)[[3]]
  avg = t.test(scores)[[5]]
  if (avg > 1.5) {
    message = "Wow, that's very positive!"
    emoji = "\U1F601"
  } else if (avg > 0.5) {
    message = "That's positive!"
    emoji = "\U1F603"
  } else if (avg > 0) {
    message = "That's close to neutral, slightly positive."
    emoji = "\U1F60A"
  } else if (avg > -0.5) {
    message = "That's close to neutral, slightly negative"
    emoji = "\U1F623"
  } else if (avg > -1.5) {
    message = "That's negative..."
    emoji = "\U1F628"
  } else {
    message = "That's very, very negative."
    emoji = "\U1F631"
  }
  cat("The positivity/negativity score of the ", n_tweets, " tweets is ", avg, " (p-value = ", p_value, "). ", message, " ",sep = '')
  cat(utf8_format(emoji))
}

#shows one tweet
show_tw = function(string, n_tweets = 1){
  setup_tweets()
  check_packages("twitteR")
  twt = twitteR::searchTwitter(string, n = n_tweets, since = as.character(Sys.Date() - 30))
  df_tw = twitteR::twListToDF(twt)
  print(df_tw$text)
}

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





# KITTY

library(ggplot2)
library(tidyverse)
library(rvest)
library(stringr) 
library(rebus)
library(stringi)
library(httr)

# removes spaces, new lines, some symbols from all scraped data
clean_str = function(strg) {
  strg = str_remove_all(strg, "\n")
  strg = str_remove_all(strg, " ")
  strg = str_remove_all(strg, regex("[$%]"))
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
  religion = str_remove_all(religion, regex("/s"))
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
get_acc_rate = function(info2){
  lon = 2:9
  accept = NA
  for (i in lon) {
    if (sum(str_detect(info2[i], regex("%")))>0) {
      accept = info2[2]
      accept = clean_str(info2[i])
      break
    }
  }
  return(accept)
}

# get student faculty ratio
get_stu_fac_ratio = function(info2){
  lon = 9:13
  ratio = NA
  try({
    for (i in lon) {
      if (str_detect(info2[i], ":")) {
        ratio = clean_str(info2[i])
        break
      }
    }
  }, silent = TRUE)
  return(ratio)
}

# get 4 year graduation rate
get_grad_rate=function(info){
  lon = 10:14
  grad = NA
  try({
    for (i in lon) {
      if (str_detect(info2[i], "%")) {
        grad = clean_str(info2[i])
        break
      }
    }
  }, silent = TRUE)
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
  acc_rate[i] = get_acc_rate(info2)
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



