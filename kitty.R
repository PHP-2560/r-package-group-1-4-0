library(rebus)
library(stringi)
library(stringr)
library(httr)
library(rvest)


#scraping functions

princeton = read_html("https://www.usnews.com/best-colleges/best-colleges/princeton-university-2627")
cumber = read_html("https://www.usnews.com/best-colleges/university-of-the-cumberlands-1962")
bu = read_html("https://www.usnews.com/best-colleges/boston-university-2130")

info = html_text(html_nodes(bu, ".flex-small"))
details = html_text(html_nodes(link, ".full-width , strong"))
info2 = html_text(html_nodes(bu, ".medium-end"))

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