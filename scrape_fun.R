#scraping functions

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