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
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
}

#This function gets the positivity associate with the tweets related to a word from -4 to 4. There are some parameters and prints a ttest.
get_twitter_feelings = function(string, n_tweets = 100, since_date = Sys.Date() - 30, ttest = TRUE){
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

show_tw = function(string, n_tweets = 1){
  invisible(capture.output(setup_tweets()))
  check_packages(c("tidytext", "twitteR"))
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






