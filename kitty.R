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
details = html_text(html_nodes(cumber, ".full-width , strong"))
info2 = html_text(html_nodes(bu, ".medium-end"))

# removes spaces, new lines, some symbols from all scraped data
clean_str = function(strg) {
  strg = str_remove_all(strg, "\n")
  strg = str_remove_all(strg, " ")
  strg = str_remove_all(strg, regex("[$%,]"))
}

# gets score 
get_score=function(details){
  score = NA
  if (str_detect(details[2], pattern = "Overall")) {
    score = clean_str(details[2])
    score = str_remove_all(score, regex("[a-zA-Z]"))
    score = str_split(score, "/", n = 2)
    score = score[[1]][1]
  }
  return(score)
}

# gets location
get_location=function(details){
  lon = 1:4
  location = NA
  for (i in lon) {
    if (str_detect(details[i], ",")) {
      location = details[i]
      break
    }
  }
  return(location)
}

# gets tuition
get_tuition=function(details){
  lon = 4:9
  tuition = NA
  for (i in lon) {
    if (str_detect(details[i], "Quick")) {
      ind = i + 1
      tuition = clean_str(details[ind])
      tuition = str_split(tuition, "\\(", n = 2)
      tuition = tuition[[1]][1]
    }
  }
  return(tuition)
}


# get room & board
get_room_board=function(details){
  lon = 4:9
  rb = NA
  for (i in lon) {
    if (str_detect(details[i], "Quick")) {
      ind = i + 2
      rb = clean_str(details[ind])
      rb = str_split(rb, "\\(", n = 2)
      rb = rb[[1]][1]
    }
  }
  return(rb)
}

# get enrollment
get_enrollment=function(details){
    lon = 4:9
    enroll = NA
    for (i in lon) {
      if (str_detect(details[i], "Quick")) {
        ind = i + 3
        enroll = clean_str(details[ind])
      }
    }
    return(enroll)
}


#------------------------------------------------------




# returns school type
get_school_type = function(info){
  school_type = str_split(info[1], ",", n = 2)
  school_type = school_type[[1]][1]
  school_type = clean_str(school_type)
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
    salary = str_remove_all(salary, regex("[,*]"))
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
#---------------------------------------------------
