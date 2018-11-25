#twitter

#install.packages("twitteR")
library(twitteR)

# Change the next four lines based on your own consumer_key, consume_secret, access_token, and access_secret. 
consumer_key <- "kmp7IUYAhZQftCrYkFSICCjuz"
consumer_secret <- "EMtBaYhcYV7V3vAEmSMSvApaSDJfj101fCD5TYAjicf4Z3ncy6"
access_token <- "1060241795240599557-2ik3E5cuXZxtKDG7d5zP0TiPQcSlKv"
access_secret <- "YYhkQxzqOTu6jLuKd1W5loq3JQYq24xOUA6at5uZqUhS2"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tw = twitteR::searchTwitter('Harvard University', n = 1e3, since = '2018-09-08', retryOnRateLimit = 1e4)
d = twitteR::twListToDF(tw)