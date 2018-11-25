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

# Change the next four lines based on your own consumer_key, consume_secret, access_token, and access_secret. 
consumer_key <- "kmp7IUYAhZQftCrYkFSICCjuz"
consumer_secret <- "EMtBaYhcYV7V3vAEmSMSvApaSDJfj101fCD5TYAjicf4Z3ncy6"
access_token <- "1060241795240599557-Qyvx7TY0kYQ2ovPLYnMi6E4GNgirko"
access_secret <- "Q0AEBJvFx8kAh1p5tOCFppWppT12Mm8iEEj0zaf7dFvxL"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tw = twitteR::searchTwitter('Harvard', n = 1e3, since = '2018-09-08', retryOnRateLimit = 1e4, lang = "en")
d = twitteR::twListToDF(tw)
d = as_tibble(d)

setup_tweets = function(){
  check_packages(c("twitteR", "base64enc"))
  consumer_key <- "kmp7IUYAhZQftCrYkFSICCjuz"
  consumer_secret <- "EMtBaYhcYV7V3vAEmSMSvApaSDJfj101fCD5TYAjicf4Z3ncy6"
  access_token <- "1060241795240599557-Qyvx7TY0kYQ2ovPLYnMi6E4GNgirko"
  access_secret <- "Q0AEBJvFx8kAh1p5tOCFppWppT12Mm8iEEj0zaf7dFvxL"
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
}

get_twitter_feelings = function(string, n_tweets = 100, since_date = Sys.Date() - 30, ttest = TRUE){
  setup_tweets()
  check_packages(c("tidytext", "stringr", "twitteR"))
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
  print(words[1:20])
  print(length(scores))
  t.test(scores)
}







