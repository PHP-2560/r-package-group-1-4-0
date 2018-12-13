library(twitteR)
library(tibble)
library(tidytext)
library(stringr)
library(base64enc)

check_packages = function(names) {
    for (name in names) {
        if (!(name %in% installed.packages())) {
            install.packages(name, repos = "http://cran.us.r-project.org")
        }
        library(name, character.only = TRUE)
    }
}


# TWITTER
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tw = twitteR::searchTwitter("Harvard", n = 1000, since = "2018-09-08", retryOnRateLimit = 10000, lang = "en")
d = twitteR::twListToDF(tw)
d = as_tibble(d)


# this function sets up the twitter api. The parameters are optional and I'm using the ones from my twitter api account
setup_tweets = function(consumer_key = "kmp7IUYAhZQftCrYkFSICCjuz", consumer_secret = "EMtBaYhcYV7V3vAEmSMSvApaSDJfj101fCD5TYAjicf4Z3ncy6", 
    access_token = "1060241795240599557-Qyvx7TY0kYQ2ovPLYnMi6E4GNgirko", access_secret = "Q0AEBJvFx8kAh1p5tOCFppWppT12Mm8iEEj0zaf7dFvxL") {
    check_packages(c("twitteR", "base64enc"))
    invisible(capture.output(setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)))
}

#---------------------------------------------------------------------------------------
get_tw_words = function(string, n_tweets, since_date) {
    setup_tweets()
    check_packages(c("tidytext", "stringr", "twitteR", "utf8"))
    twt = twitteR::searchTwitter(string, n = n_tweets, since = as.character(since_date), retryOnRateLimit = 10000, lang = "en")
    df_tw = twitteR::twListToDF(twt)
    words = vector()
    for (i in 1:nrow(df_tw)) {
        text = vector()
        count = 1
        while (!is.na(word(df_tw$text[i], count))) {
            text[count] = str_extract(word(df_tw$text[i], count), regex("[A-Za-z]+"))
            count = count + 1
        }
        words = append(words, text)
    }
    return(words)
}

# This function gets the positivity associate with the tweets related to a word from -4 to 4. There are some parameters and prints a ttest.
#---------------------------------------------------------------------------------------
get_tw_feelings = function(string, n_tweets = 20, since_date = Sys.Date() - 30) {
    words = get_tw_words(string, n_tweets, since_date)
    afinn = get_sentiments("afinn")
    scores = vector()
    count = 1
    for (i in 1:length(words)) {
        if (words[i] %in% afinn$word) {
            scores[count] = afinn$score[which(afinn$word == words[i])]
            count = count + 1
        }
    }
    p_value = t.test(scores)[[3]]
    avg = t.test(scores)[[5]]
    if (avg > 1.5) {
        message = "Wow, that's very positive!"
        emoji = "\U0001f601"
    } else if (avg > 0.5) {
        message = "That's positive!"
        emoji = "\U0001f603"
    } else if (avg > 0) {
        message = "That's close to neutral, slightly positive."
        emoji = "\U0001f60a"
    } else if (avg > -0.5) {
        message = "That's close to neutral, slightly negative"
        emoji = "\U0001f623"
    } else if (avg > -1.5) {
        message = "That's negative..."
        emoji = "\U0001f628"
    } else {
        message = "That's very, very negative."
        emoji = "\U0001f631"
    }
    cat("The positivity/negativity score of the ", n_tweets, " tweets is ", avg, " (p-value = ", p_value, "). ", message, " ", sep = "")
    cat(utf8_format(emoji))
}

# shows one tweet ---------------------------------------------------------------------------------------
show_tw = function(string, n_tweets = 1) {
    setup_tweets()
    check_packages("twitteR")
    twt = twitteR::searchTwitter(string, n = n_tweets, since = as.character(Sys.Date() - 30))
    df_tw = twitteR::twListToDF(twt)
    print(df_tw$text)
}

# plots pie chart with feelings ---------------------------------------------------------------------------------------
plot_moody_pie = function(string, n_tweets = 20, since_date = Sys.Date() - 30) {
    check_packages("tibble")
    words = get_tw_words(string, n_tweets, since_date)
    nrc = get_sentiments("nrc")
    feelings = vector()
    count = 1
    for (i in 1:length(words)) {
        if (words[i] %in% nrc$word) {
            for (j in 1:length(which(nrc$word == words[i]))) {
                feelings[count] = nrc$sentiment[which(nrc$word == words[i])[j]]
                count = count + 1
            }
        }
    }
    f_df = tibble(feelings)
    colnames(f_df) = "feelings"
    f_df[[1]] = as.factor(f_df[[1]])
    ggplot(f_df, aes(x = 1, fill = feelings, col = feelings)) +
      geom_bar() + 
      coord_polar(theta = "y") +
      ggtitle(string)
}

# ---------------------------------------------------------------------------------------------------------------
pay_loans = function(university, salary = 70000, interest = 0.03, per_sal = 0.15) {
    if (length(which(df$University == university)) == 0) {
        stop("That is not a valid university")
    }
    tuition = as.integer(as.character(df$Tuition[which(df$University == university)]))
    if (is.na(tuition)) {
        stop("Sorry, there is no tuition data avaiable for that university")
    }
    bill = 4 * tuition
    year = 0
    while (bill > 0) {
        bill = bill - salary * per_sal
        bill = bill * (1 + interest)
        year = year + 1
    }
    cat("Assuming a salary of ", salary, " dollars per year, that ", per_sal * 100, "% (", per_sal * salary, "$) would be used to pay the loan annualy, and the interest rate at ", 
        0.02 * 100, "% y/y, it would take ", year, " year(s) to pay for the student loans", sep = "")
}



