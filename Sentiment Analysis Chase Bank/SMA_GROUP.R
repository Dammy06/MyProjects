rm(list=ls())
# 1 Use package rtweet
if(!require("rtweet")) install.packages("rtweet"); library("rtweet")
if(!require("twitteR")) install.packages("twitteR"); library("twitteR")
if(!require("dendroTools")) install.packages("dendroTools"); library("dendroTools")
library(dendroTools)
if(!require("tidytext")) install.packages("tidytext");library("tidytext")
if ("dplyr" %in% installed.packages()[, "Package"]){ 
  cat("'dplyr' is installed.")
} else {
  install.packages("dplyr",dependencies=T)
}
# Install
#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
#install.packages("magrittr")
# Load
library("tm")
library("SnowballC")

library("RColorBrewer")
library(dplyr)
library(base64enc)
library(httr)
library(jsonlite)
library(dplyr)
if (!require("ggplot2")) install.packages("ggplot2", quiet=TRUE) ; require("ggplot2")
library(janeaustenr)
library(stringr)
library(syuzhet)
library(magrittr)


if(!require("maps")) install.packages("maps"); library("maps")
for (i in c('SnowballC','slam','tm','Matrix','tidytext','dplyr','hunspell','purrr',"textdata")){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}
if (!require("wordcloud")) {
  install.packages("wordcloud",repos="https://cran.rstudio.com/",
                   quiet=TRUE)
  require("wordcloud")
}
#install.packages("textdata")
library(textdata)
library(glue)
library(cowplot)
library(plotly)
library(tidyverse)
library(widyr)

library(hms)
library(lubridate) 



# Network Analysis
library(igraph)
# Network Visualization (D3.js)
if(!require("networkD3")) install.packages("networkD3"); library("networkD3")
if(!require("wordcloud2")) install.packages("wordcloud2"); library("wordcloud2")
if(!require("devtools")) install.packages("devtools");library(devtools)
devtools::install_github("lchiffon/wordcloud2")
# 2 create the access token (authentication)
#Project_token <- create_token(
#  app = "Kanishk_API",
#  consumer_key = "L76seJn1kMRRkOLkUwJ9hVCwP",
#  consumer_secret = "LItpxkElG27M5YQPcTnvoHLi4iNvqmo5FCRK69ECy4TiU5VhPZ",
#  access_token = "1218606874867773440-ctsHc2DRsswdR9ItCnAnBHXs2p9fbY",
#  access_secret = "uZhTmLzZwWXWEhx5jkkW5z2gxgCYPelmteymQN6mw0EWU",
#  set_renv=FALSE)

## search for 1000 tweets using the #rstats hashtag
#project_rtweets <- rtweet::search_tweets(q = "@Chase",
#                                 n = 1000,include_rts = FALSE)

#project_rtweets2 <- rtweet::search_tweets(q = "@ChaseSupport",
#                                         n = 1000,include_rts = FALSE)
#Most recent tweets from chase
#projet_timeline <- get_timeline("@Chase", n = 3000)
#projet_timeline2 <- get_timeline("@ChaseSupport", n = 3000)

#Saving the file
#save(project_rtweets,file="project_rtweets.Rda")
#save(project_rtweets2,file="project_rtweets2.Rda")
#save(projet_timeline,file="projet_timeline.Rda")
#save(projet_timeline2,file="projet_timeline2.Rda")


load("project_rtweets.Rda")
load("project_rtweets2.Rda")
load("projet_timeline.Rda")
load("projet_timeline2.Rda")


#Keep only organic tweets and retweets and removing redundant data (@chase)
# Remove retweets
project_tweets_organic <- projet_timeline[projet_timeline$is_retweet==FALSE,]
# Remove replies
project_tweets_organic <- subset(project_tweets_organic, is.na(project_tweets_organic$reply_to_status_id)) 

#Analyzing engagement (@Chase)
#Determining the favourite count
project_tweets_organic <- project_tweets_organic %>% arrange(-favorite_count)
project_tweets_organic[1,5]

#Determining retweet count
project_tweets_organic <- project_tweets_organic %>% arrange(-retweet_count)
project_tweets_organic[1,5]

# Keep only retweets and replies i.e no organic data (@chase)
# Remove retweets
# Keeping only the retweets
project_retweets <- projet_timeline[projet_timeline$is_retweet==TRUE,]
# Keeping only the replies
project_replies <- subset(projet_timeline, !is.na(projet_timeline$reply_to_status_id))

# separate data frame containing the number of organic tweets, retweets, and replies
# Creating a data frame
proj_data <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(1566, 275, 1156)
)

#PIE CHART REPRESENTATION (@Chase)
# Adding columns 
proj_data$fraction = proj_data$count / sum(proj_data$count)
proj_data$percentage = proj_data$count / sum(proj_data$count) * 100
proj_data$ymax = cumsum(proj_data$fraction)
proj_data$ymin = c(0, head(proj_data$ymax, n=-1))
# Rounding the data to two decimal points

proj_data <- round_df(proj_data, 2)
# Specify what the legend should say
Type_of_Tweet <- paste(proj_data$category, proj_data$percentage, "%")
ggplot(proj_data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

# frequency of tweets by month (@Chase)
colnames(projet_timeline)[colnames(projet_timeline)=="screen_name"] <- "Twitter_Account"
ts_plot(dplyr::group_by(projet_timeline, Twitter_Account), "month") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets of Chase",
    subtitle = "Tweet counts aggregated by year",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# Source from which the tweets are published
Project_app <- projet_timeline %>% 
  select(source) %>% 
  group_by(source) %>%
  summarize(count=n())
Project_app<- subset(Project_app, count > 11)

# Pie Chart Representation
data2 <- data.frame(
  category=Project_app$source,
  count=Project_app$count
)
data2$fraction = data2$count / sum(data2$count)
data2$percentage = data2$count / sum(data2$count) * 100
data2$ymax = cumsum(data2$fraction)
data2$ymin = c(0, head(data2$ymax, n=-1))
data2 <- round_df(data2, 2)
Source <- paste(data2$category, data2$percentage, "%")
ggplot(data2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

# SOME BASIC CLEANING
project_tweets_organic$text <-  gsub("https\\S*", "", project_tweets_organic$text)
project_tweets_organic$text <-  gsub("@\\S*", "", project_tweets_organic$text) 
project_tweets_organic$text  <-  gsub("amp", "", project_tweets_organic$text) 
project_tweets_organic$text  <-  gsub("[\r\n]", "", project_tweets_organic$text)
project_tweets_organic$text  <-  gsub("[[:punct:]]", "", project_tweets_organic$text)

# remove stop words
Project_new_tweets <- project_tweets_organic %>%
  select(text) %>%
  unnest_tokens(word, text)

Project_new_tweets <- Project_new_tweets%>%
  anti_join(stop_words)

#Plotting most frequent words (@Chase) Use for Rshiny
Project_new_tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets of Chase",
       subtitle = "Stop words removed from the list")

#Most frequently used hashtags (@chase) Use for Rshiny
project_tweets_organic$hashtags <- as.character(project_tweets_organic$hashtags)
project_tweets_organic$hashtags <- gsub("c\\(", "", project_tweets_organic$hashtags)
set.seed(1234)
wordcloud(project_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Accounts fromwhich most retweets generate (@chase) Use for Rshiny
set.seed(1234)
wordcloud(project_retweets$retweet_screen_name, min.freq=3, scale=c(2, 0.25), random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))

X1<-get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", 
                          "negative")) %>% 
  count(sentiment)

y1<- get_sentiments("bing") %>% 
  count(sentiment)

# Creating a new variable to know the sentiments of selected words
Project_new_tweets_Sentiment <- inner_join(Project_new_tweets,get_sentiments("bing"))

summarySentiment2 <- Project_new_tweets_Sentiment %>%  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

par(oma=c(0,4,0,0),mfrow=c(1,2))
barplot(summarySentiment2[summarySentiment2$sentiment == "negative",3],names.arg = summarySentiment2[summarySentiment2$sentiment == "negative",1],horiz=TRUE,las=1,xlim=c(0,15),col="red",axes=TRUE)                
barplot(summarySentiment2[summarySentiment2$sentiment == "positive",3],names.arg = summarySentiment2[summarySentiment2$sentiment == "positive",1],horiz=TRUE,las=1,xlim=c(0,15),col="green")                

#
if (!require("ggplot2")) install.packages("ggplot2", quiet=TRUE) ; require("ggplot2")

#better visualization of sentiment score
summarySentiment2 %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
####################################################
#Keep only organic tweets and retweets and removing redundant data (@chasesupport)
# Remove retweets
project_tweets_organic2 <- projet_timeline2[projet_timeline2$is_retweet==FALSE,]
# Remove replies
project_tweets_organic2 <- subset(project_tweets_organic2, is.na(project_tweets_organic2$reply_to_status_id)) 

#Analyzing engagement (@Chasesupport)
#Determining the favourite count
project_tweets_organic2 <- project_tweets_organic2 %>% arrange(-favorite_count)
project_tweets_organic2[1,5]

#Determining retweet count
project_tweets_organic2 <- project_tweets_organic2 %>% arrange(-retweet_count)
project_tweets_organic2[1,5]

# Keep only retweets and replies i.e no organic data (@chasesupport)
# Remove retweets
# Keeping only the retweets
project_retweets2 <- projet_timeline2[projet_timeline2$is_retweet==TRUE,]
# Keeping only the replies
project_replies2 <- subset(projet_timeline2, !is.na(projet_timeline2$reply_to_status_id))

# separate data frame containing the number of organic tweets, retweets, and replies (@chasesupport)
# Creating a data frame
proj_data2 <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(2725, 0, 2988)
)

#PIE CHART REPRESENTATION (@Chasesupport)
# Adding columns 
proj_data2$fraction = proj_data2$count / sum(proj_data2$count)
proj_data2$percentage = proj_data2$count / sum(proj_data2$count) * 100
proj_data2$ymax = cumsum(proj_data2$fraction)
proj_data2$ymin = c(0, head(proj_data2$ymax, n=-1))
# Rounding the data to two decimal points
proj_data2 <- round_df(proj_data2, 2)
# Specify what the legend should say
Type_of_Tweet_2 <- paste(proj_data2$category, proj_data2$percentage, "%")
ggplot(proj_data2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet_2)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

# frequency of tweets by month (@Chasesupport)
colnames(projet_timeline2)[colnames(projet_timeline2)=="screen_name"] <- "Twitter_Account"
ts_plot(dplyr::group_by(projet_timeline2, Twitter_Account), "month") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets of ChaseSupport",
    subtitle = "Tweet counts aggregated by year",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# Source from which the tweets are published(@chasesupport)
Project_app2 <- projet_timeline2 %>% 
  select(source) %>% 
  group_by(source) %>%
  summarize(count=n())
Project_app2<- subset(Project_app2, count > 11)

# Pie Chart Representation
data2_1 <- data.frame(
  category=Project_app2$source,
  count=Project_app2$count
)
data2_1$fraction = data2_1$count / sum(data2_1$count)
data2_1$percentage = data2_1$count / sum(data2_1$count) * 100
data2_1$ymax = cumsum(data2_1$fraction)
data2_1$ymin = c(0, head(data2_1$ymax, n=-1))
data2_1 <- round_df(data2_1, 2)
Source <- paste(data2_1$category, data2_1$percentage, "%")
ggplot(data2_1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

# SOME BASIC CLEANING(@chasesupport)
project_tweets_organic2$text <-  gsub("https\\S*", "", project_tweets_organic2$text)
project_tweets_organic2$text <-  gsub("@\\S*", "", project_tweets_organic2$text) 
project_tweets_organic2$text  <-  gsub("amp", "", project_tweets_organic2$text) 
project_tweets_organic2$text  <-  gsub("[\r\n]", "", project_tweets_organic2$text)
project_tweets_organic2$text  <-  gsub("[[:punct:]]", "", project_tweets_organic2$text)

# remove stop words
Project_new_tweets2 <- project_tweets_organic2 %>%
  select(text) %>%
  unnest_tokens(word, text)

Project_new_tweets2 <- Project_new_tweets2%>%
  anti_join(stop_words)

#Plotting most frequent words (@Chasesupport)
Project_new_tweets2 %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets of Chase",
       subtitle = "Stop words removed from the list")

#Most frequently used hashtags (@chasesupport)
project_tweets_organic2$hashtags <- as.character(project_tweets_organic2$hashtags)
project_tweets_organic2$hashtags <- gsub("c\\(", "", project_tweets_organic2$hashtags)
set.seed(1234)
wordcloud(project_tweets_organic2$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Accounts fromwhich most retweets generate (@chasesupport)
set.seed(1234)
wordcloud(project_retweets2$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))

#Getting access to dictionary (Bing)
X<-get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", 
                          "negative")) %>% 
  count(sentiment)

y<- get_sentiments("bing") %>% 
  count(sentiment)

# Creating a new variable to know the sentiments of selected words
Project_new_tweets2_Sentiment <- inner_join(Project_new_tweets2,get_sentiments("bing"))

summarySentiment <- Project_new_tweets2_Sentiment %>%  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

par(oma=c(0,4,0,0),mfrow=c(1,2))
barplot(summarySentiment[summarySentiment$sentiment == "negative",3],names.arg = summarySentiment[summarySentiment$sentiment == "negative",1],horiz=TRUE,las=1,xlim=c(0,15),col="red",axes=TRUE)                
barplot(summarySentiment[summarySentiment$sentiment == "positive",3],names.arg = summarySentiment[summarySentiment$sentiment == "positive",1],horiz=TRUE,las=1,xlim=c(0,15),col="green")                

#
if (!require("ggplot2")) install.packages("ggplot2", quiet=TRUE) ; require("ggplot2")

#better visualization of sentiment score
summarySentiment %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

############################################################################################################
#######################################################################
###################### Twitter Tokens ##################################

#appname <- "ALZ_SMA_2"
#consumer_key <- "6RlLoeYKLO5mYnKwjD8aNhOHj"
#consumer_secret <- "pYTDXQK0nrKjgawJEgs9XzroVrz9qPC1LJXPqc3sj0s0GrhRug"
#access_token <- "532583537-lFnDEng2fKeyxIYSV6ZAeqiEbzImqWeyIinpwKWN"
#access_secret <- "1uCscNMcFX7dSM88WPskdf3x6hM15bFckhvIwULfIufTB"

#twitter_token <- create_token(
#  app = appname,
#  consumer_key = consumer_key,
#  consumer_secret = consumer_secret,
#  access_token = access_token,
#  access_secret = access_secret,
#  set_renv=FALSE)

######################### Downloading Tweets ######################################

#chase_sup <- rtweet::search_tweets(q = "@ChaseSupport",
#n = 100,include_rts = FALSE)

#chase_sup_feb19 <- search_fullarchive(q = "@ChaseSupport", n = 1000,env_name="alz20200128",fromDate="201902010000",toDate="201902280000")
#chase_sup_feb19_2 <- search_fullarchive(q = "@ChaseSupport", n = 1000,env_name="alz20200128",fromDate="201902010000",toDate="201902182300")
#chase_sup_feb19_3 <- search_fullarchive(q = "@ChaseSupport", n = 1000,env_name="alz20200128",fromDate="201902010000",toDate="201902150000")

#chase_sup_Jan20 <- search_30day(q = "@ChaseSupport", n = 1000,env_name="alz1991",fromDate="201912290000",toDate="202001280000")
#chase_sup_Jan20_2 <- search_30day(q = "@ChaseSupport", n = 1000,env_name="alz1991",fromDate="201912290000",toDate="202001140000")

#chase_sup_Dec19_1 <- search_fullarchive(q = "@ChaseSupport", n = 5000,env_name="alz1991",fromDate="201912010000",toDate="201912310000")
#chase_sup_Nov19_1 <- search_fullarchive(q = "@ChaseSupport", n = 5000,env_name="alz1991",fromDate="201911010000",toDate="201911300000")

#chase_sup_Jan20 <- search_30day(q = "@ChaseSupport", n = 5000,env_name="alz91",fromDate="201912310000",toDate="202001290000")
#chase_sup_Jan20_13 <- search_30day(q = "@ChaseSupport", n = 5000,env_name="alz91",fromDate="201912310000",toDate="202001131000")

#chase_sup_jan2019 <- search_fullarchive(q = "@ChaseSupport", n = 5000,env_name="alz1991",fromDate="201901010000",toDate="201901040000")

######################### Saving Data ######################################

#save(chase_sup_feb19_3,file="chase_feb19_1.Rda")
#save(chase_sup_feb19_2,file="chase_feb19_2.Rda")
#save(chase_sup_feb19,file="chase_feb19_3.Rda")
#save(chase_sup_full_2,file="chase_Jan19_1.Rda")
#save(chase_sup_full,file="chase_Jan19_2.Rda")
#save(chase_sup_Jan20_2,file="chase_Jan20_1.Rda")
#save(chase_sup_Jan20,file="chase_Jan20_2.Rda")
#save(chase_sup_Dec19_1,file="chase_Dec.Rda")
#save(chase_sup_Dec19_1,file="chase_Dec.Rda")
#save(chase_sup_Nov19_1,file="chase_Nov.Rda")
#save(chase_nov19_jan20,file="chase_nov19_jan20.Rda")
#save(chase_jan_feb19,file="chase_jan_feb19.Rda")

######################### Merging Data ######################################

#novdec19 <- rbind(chase_sup_Nov19_1, chase_sup_Dec19_1)
#jan2020 <- rbind(chase_sup_Jan20, chase_sup_Jan20_13)
#chase_jan19 <- rbind(chase_sup_full, chase_sup_full_2)
#chase_fec19 <- rbind(chase_sup_feb19, chase_sup_feb19_2, chase_sup_feb19_3)
#chase_jan_feb19 <- rbind(chase_jan19_feb19, chase_sup_jan2019)
#chase_jan19_feb19 <- rbind(chase_jan19, chase_fec19)
#chase_nov19_jan20 <- rbind(novdec19, jan2020)

############################# Loading Data ########################################

load("chase_nov19_jan20.Rda")

############################# Packages ########################################

if(!require("rtweet")) install.packages("rtweet"); library("rtweet")
if(!require("maps")) install.packages("maps"); library("maps")
for (i in c('SnowballC','slam','tm','Matrix','tidytext','dplyr','hunspell','purrr')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}
if (!require("wordcloud")) {
  install.packages("wordcloud",repos="https://cran.rstudio.com/",
                   quiet=TRUE)
  require("wordcloud")
}
#install.packages("textdata")
library(textdata)
if (!require("ggplot2")) install.packages("ggplot2", quiet=TRUE) ; require("ggplot2")
library(tidyverse)


###################### Tweets Location - Map ##################################

rt <- lat_lng(chase_nov19_jan20)

par(mar = c(0, 0, 0, 0))
maps::map("world", lwd = .25)
points(rt$lng, rt$lat, pch = 20, cex = 1,col="red")

###################### Tweets Distribution ########################################

ts_plot(chase_nov19_jan20, by = "60 secs")

###################### PreProcessing - Tekanization ##################################

# 1. Remove punctuation and numbers with regular expressions
chase_comments <- mutate(chase_nov19_jan20, message = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

# 2. Tokenization (+ going to lowercase)
#chase_tokenized <- chase_comments %>% unnest_tokens(output = "word", # how should the new column be named?
#                                                                           input = message, # where can we find the text? 
#                                                                            token = "words",# which tokenization scheme should we follow?
#                                                                            drop=FALSE,to_lower=TRUE) # drop=FALSE specifies that we want to keep our text; to_lower puts everyting to lowercase

# 3. Remove some other elements such as # and @ signs if they might occur
#chase_tokenized <- filter(chase_tokenized, substr(word, 1, 1) != '#', 
#                                     substr(word, 1, 1) != '@') # This compares for the first letter of a token# omit hashtags

# 4. Correct Spelling
correct_spelling <- function(input) {
  output <- case_when(
    # any manual corrections
    input == 'license' ~ 'licence',
    # check and (if required) correct spelling
    !hunspell_check(input, dictionary('en_GB')) ~
      hunspell_suggest(input, dictionary('en_GB')) %>%
      # get first suggestion, or NA if suggestions list is empty
      map(1, .default = NA) %>%
      unlist(),
    TRUE ~ input # if word is correct
  )
  # if input incorrectly spelled but no suggestions, return input word
  ifelse(is.na(output), input, output)
}

#chase_tokenized <- chase_tokenized %>%  mutate(suggestion = correct_spelling(word))

# 5. remove stopwords
#chase_tokenized <- chase_tokenized %>% anti_join(get_stopwords()) # note that I continue with the 'uncorrected' words here

# 6. Stemming
#chase_tokenized <- chase_tokenized %>% mutate(word = wordStem(word)) 

# 7. Create the document-term matrix
#chase_tokenized <- chase_tokenized %>% count(user_id,word)
#save(chase_tokenized,file="chase_tokenized.Rda")
load("chase_tokenized.Rda")

chase_tokenized <- chase_tokenized %>% count(user_id,word)
ChaseDTM <- chase_tokenized %>% cast_dtm(user_id,word,n,weighting = tm::weightTfIdf)

# 8. Inspect text, frequency, and word cloud (Raw Tweets and Processed)

findAssocs(ChaseDTM, terms = "Chase", corlimit = 0.1)

Chasefreq <- chase_tokenized %>% group_by(word) %>% # for this, we need to have the sum over all documents
  summarize(freq = n()) %>%
  arrange(-freq)                  # arrange = order; from most frequent term to lowest frequent
head(Chasefreq)

tf <- termFreq(chase_comments$message)

wordcloud(names(tf),tf,
          max.words=40,
          scale=c(0.7,0.7))

wordcloud(Chasefreq$word, Chasefreq$freq,
          max.words=40,
          scale=c(0.9,0.9))

#################################### Sentiment Analysis ################################

get_sentiments("bing") %>% 
  count(sentiment)

chase_sentiment <- inner_join(chase_tokenized,get_sentiments("bing"))

#Sentiment 
summary_sentiment <- chase_sentiment %>%  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

par(oma=c(0,4,0,0),mfrow=c(1,2))
barplot(summary_sentiment[summary_sentiment$sentiment == "negative",3],names.arg = summary_sentiment[summary_sentiment$sentiment == "negative",1],horiz=TRUE,las=1,xlim=c(0,15),col="red",axes=TRUE)                
barplot(summary_sentiment[summary_sentiment$sentiment == "positive",3],names.arg = summary_sentiment[summary_sentiment$sentiment == "positive",1],horiz=TRUE,las=1,xlim=c(0,15),col="green")                

#Sentiment Plot - GGPlot
summary_sentiment %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#Summary per Post
status_sentiment <- chase_sentiment %>%
  count(user_id, sentiment) %>%                # count the positives and negatives per id (status)
  spread(sentiment, n, fill = 0) %>%      # we want overall sentiment, and here we have text (positive and negative), making it difficult to direclty calculate sentiment. 
  mutate(sentiment = positive - negative)


status_sentiment <- inner_join(chase_tokenized,get_sentiments("afinn")) %>%
  group_by(user_id) %>%                      # here we get numeric values, so we can just sum them per post
  summarize(Sentiment = sum(value)) 

mean(status_sentiment$Sentiment)

status_sentiment <- chase_comments %>% left_join(status_sentiment,by="user_id") %>% 
  mutate(Sentiment = ifelse(is.na(Sentiment),0,Sentiment))


###################################################################################################################"
###################Topic Modelling and Bigram Analysis#####################################################################



load("chase_nov19_jan20.Rda")

# 1. Remove punctuation and numbers with regular expressions
chase_comments <- mutate(chase_nov19_jan20, message = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))


tweets.df <- chase_comments %>% 
  select(text) %>% 
  mutate(text= text %>% str_remove_all(pattern = '\\n')) %>% 
  mutate(text = text %>% str_remove_all(pattern = '&amp')) %>% 
  mutate(text = text %>% str_remove_all(pattern = 'https://t.co/[a-z,A-Z,0-9]*')) %>% 
  mutate(text = text %>% str_remove_all(pattern = 'http://t.co/[a-z,A-Z,0-9]*')) %>% 
  mutate(text = text %>% str_remove_all(pattern = 'https')) %>% 
  mutate(text = text %>% str_remove_all(pattern = 'http')) %>% 
  
  # Remove hashtags.
  mutate(text = text %>% str_remove_all(pattern = '#[a-z,A-Z]*')) %>% 
  # Remove accounts.
  mutate(text = text %>% str_remove_all(pattern = '@[a-z,A-Z]*')) %>% 
  # Remove retweets.
  mutate(text = text %>% str_remove_all(pattern = 'rt [a-z,A-Z]*: ')) %>% 
  mutate(text = text %>% str_remove(pattern = '^(rt)')) %>% 
  mutate(text = text %>% str_remove_all(pattern = '\\_')) 

# Replace accents. 
replacement.list <- list('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u')

tweets.df %<>% 
  mutate(text = chartr(old = names(replacement.list) %>% str_c(collapse = ''), 
                       new = replacement.list %>% str_c(collapse = ''),
                       x = text))  

corpus <-  Corpus(x = VectorSource(x = tweets.df$text))

tweets.text <- corpus %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords('english')) %>% 
  tm_map(PlainTextDocument) # %>% 
# We could also use stemming by uncommenting the folowing line. 
# tm_map(stemDocument, 'english')

# Recover data into original tibble.
tweets.df %<>% mutate(text = tweets.text[[1]]$content)


#function for hashtags
GetHashtags <- function(tweet) {
  
  hashtag.vector <- str_extract_all(string = tweet, pattern = '#\\S+', simplify = TRUE) %>% 
    as.character()
  
  hashtag.string <- NA
  
  if (length(hashtag.vector) > 0) {
    
    hashtag.string <- hashtag.vector %>% str_c(collapse = ', ')
    
  } 
  
  return(hashtag.string)
}

#apply it to our data

hashtags.df <- tibble(
  Hashtags = chase_comments$text %>% map_chr(.f = ~ GetHashtags(tweet = .x))
)

hashtags.df %>% head()

#merge these data frames together.

tweets.df %<>% bind_cols(hashtags.df) 

#We begin by counting the most popular words in the tweets.

# Remove the stopwords

stopwords.df <- tibble(
  word = c(stopwords(kind = 'en'))
)

words.df <- tweets.df %>% 
  unnest_tokens(input = text, output = word) %>% 
  anti_join(y = stopwords.df, by = 'word')

word.count <- words.df %>% count(word, sort = TRUE)

word.count %>% head(10)

#visualize these counts in a bar plot

plt <- word.count %>% 
  # Set count threshold. 
  filter(n > 500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  theme_light() + 
  geom_col(fill = 'black', alpha = 0.8) +
  xlab(NULL) +
  coord_flip() +
  ggtitle(label = 'Top Word Count')

plt %>% ggplotly()

#wordcloud
wordcloud(
  words = word.count$word, 
  freq = word.count$n, 
  min.freq = 100,
  colors = brewer.pal(8, 'Dark2')
)


#We can run an analogous analysis for hastags.

hashtags.unnested.df <- tweets.df %>% 
  select( Hashtags) %>% 
  unnest_tokens(input = Hashtags, output = hashtag)

hashtags.unnested.count <- hashtags.unnested.df %>% 
  count(hashtag) %>% 
  drop_na()

#We plot the correspondinng word cloud.

wordcloud(
  words = str_c('#',hashtags.unnested.count$hashtag), 
  freq = hashtags.unnested.count$n, 
  min.freq = 5, 
  colors=brewer.pal(8, 'Dark2')
)

wordcloud2(hashtags.unnested.count, 
            
           size = 1.5,
           color = "skyblue", 
           backgroundColor = "whitesmoke")

#letterCloud(hashtags.unnested.count, word = "Dammy", wordSize = 1)
#######################################################################################
#####################Bigram Analysis###############################################
##################################################################################"


#count pairwise occurences of words which apperar together in the text

bi.gram.words <- tweets.df %>% 
  unnest_tokens(
    input = text, 
    output = bigram, 
    token = 'ngrams', 
    n = 2
  ) %>% 
  filter(! is.na(bigram))

bi.gram.words %>% 
  select(bigram) %>% 
  head(10)

#Next, we filter for stop words and remove white spaces.

bi.gram.words %<>% 
  separate(col = bigram, into = c('word1', 'word2'), sep = ' ') %>% 
  filter(! word1 %in% stopwords.df$word) %>% 
  filter(! word2 %in% stopwords.df$word) %>% 
  filter(! is.na(word1)) %>% 
  filter(! is.na(word2)) 

#group and count by bigram.

bi.gram.count <- bi.gram.words %>% 
  count(word1, word2, sort = TRUE) %>% 
  # We rename the weight column so that the 
  # associated network gets the weights (see below).
  rename(weight = n)

bi.gram.count %>% head()


bi.gram.count %>% 
  ggplot(mapping = aes(x = weight)) +
  theme_light() +
  geom_histogram() +
  labs(title = "Bigram Weight Distribution")
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.


#Note that curve is very skewed, for visualization purposes it might be a good idea to perform a transformation, e.g. log transform:

bi.gram.count %>% 
  mutate(weight = log(weight + 1)) %>% 
  ggplot(mapping = aes(x = weight)) +
  theme_light() +
  geom_histogram( binwidth = NULL) +
  labs(title = "Bigram log-Weight Distribution")
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.


#For visualization purposes, we can set a threshold which defines the minimal weight allowed in the graph.
#Remark: It is necessary to set the weight column name as weight (see igraph docs).

threshold <- 30

# For visualization purposes we scale by a global factor. 
ScaleWeight <- function(x, lambda) {
  x / lambda
}

network <-  bi.gram.count %>%
  filter(weight > threshold) %>%
  mutate(weight = ScaleWeight(x = weight, lambda = 2E3)) %>% 
  graph_from_data_frame(directed = FALSE)

# verify we have a weighted network:
is.weighted(network) 


#Visualization :To visualize the network (here is a great reference for it) we can simply use the plot function with some additional parameters:

plot(
  network, 
  vertex.size = 1,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.7, 
  vertex.label.dist = 1,
  edge.color = 'gray', 
  main = 'Bigram Count Network', 
  sub = glue('Weight Threshold: {threshold}'), 
  alpha = 50
)


# Store the degree.
V(network)$degree <- strength(graph = network)

# Compute the weight shares.
E(network)$width <- E(network)$weight/max(E(network)$weight)

plot(
  network, 
  vertex.color = 'lightblue',
  # Scale node size by degree.
  vertex.size = 2*V(network)$degree,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.6, 
  vertex.label.dist = 1.6,
  edge.color = 'gray', 
  # Set edge width proportional to the weight relative value.
  edge.width = 3*E(network)$width ,
  main = 'Bigram Count Network', 
  sub = glue('Weight Threshold: {threshold}'), 
  alpha = 50
)

# Get all connected components.
clusters(graph = network)



# Select biggest connected component.  
V(network)$cluster <- clusters(graph = network)$membership

cc.network <- induced_subgraph(
  graph = network,
  vids = which(V(network)$cluster == which.max(clusters(graph = network)$csize))
)

cc.network

# Store the degree.
V(cc.network)$degree <- strength(graph = cc.network)

# Compute the weight shares.
E(cc.network)$width <- E(cc.network)$weight/max(E(cc.network)$weight)

plot(
  cc.network, 
  vertex.color = 'lightblue',
  # Scale node size by degree.
  vertex.size = 10*V(cc.network)$degree,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.6, 
  vertex.label.dist = 1.6,
  edge.color = 'gray', 
  # Set edge width proportional to the weight relative value.
  edge.width = 3*E(cc.network)$width ,
  main = 'Bigram Count Network (Biggest Connected Component)', 
  sub = glue('Weiight Threshold: {threshold}'), 
  alpha = 50
)   

# Treshold
threshold <- 30

network <-  bi.gram.count %>%
  filter(weight > threshold) %>%
  graph_from_data_frame(directed = FALSE)

# Store the degree.
V(network)$degree <- strength(graph = network)
# Compute the weight shares.
E(network)$width <- E(network)$weight/max(E(network)$weight)

# Create networkD3 object.
network.D3 <- igraph_to_networkD3(g = network)
# Define node size.
network.D3$nodes %<>% mutate(Degree = (1E-2)*V(network)$degree)
# Degine color group (I will explore this feature later).
network.D3$nodes %<>% mutate(Group = 1)
# Define edges width. 
network.D3$links$Width <- 10*E(network)$width

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)   


#decrease the threshold to get a more complex network      
# Treshold
threshold <- 10

network <-  bi.gram.count %>%
  filter(weight > threshold) %>%
  graph_from_data_frame(directed = FALSE)

# Store the degree.
V(network)$degree <- strength(graph = network)
# Compute the weight shares.
E(network)$width <- E(network)$weight/max(E(network)$weight)

# Create networkD3 object.
network.D3 <- igraph_to_networkD3(g = network)
# Define node size.
network.D3$nodes %<>% mutate(Degree = (1E-2)*V(network)$degree)
# Degine color group (I will explore this feature later).
network.D3$nodes %<>% mutate(Group = 1)
# Define edges width. 
network.D3$links$Width <- 10*E(network)$width

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)   


###########################################################################################################
#####################LDA Topic Modelling######################################################

tweetsLDA_df <- chase_comments %>% 
  select(text,status_id) %>% 
  mutate(text= text %>% str_remove_all(pattern = '\\n')) %>% 
  mutate(text = text %>% str_remove_all(pattern = '&amp')) %>% 
  mutate(text = text %>% str_remove_all(pattern = 'https://t.co/[a-z,A-Z,0-9]*')) %>% 
  mutate(text = text %>% str_remove_all(pattern = 'http://t.co/[a-z,A-Z,0-9]*')) %>% 
  mutate(text = text %>% str_remove_all(pattern = 'https')) %>% 
  mutate(text = text %>% str_remove_all(pattern = 'http')) %>% 
  
  # Remove hashtags.
  mutate(text = text %>% str_remove_all(pattern = '#[a-z,A-Z]*')) %>% 
  # Remove accounts.
  mutate(text = text %>% str_remove_all(pattern = '@[a-z,A-Z]*')) %>% 
  # Remove retweets.
  mutate(text = text %>% str_remove_all(pattern = 'rt [a-z,A-Z]*: ')) %>% 
  mutate(text = text %>% str_remove(pattern = '^(rt)')) %>% 
  mutate(text = text %>% str_remove_all(pattern = '\\_')) 

# Replace accents. 
replacement.list <- list('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u')

tweetsLDA_df %<>% 
  mutate(text = chartr(old = names(replacement.list) %>% str_c(collapse = ''), 
                       new = replacement.list %>% str_c(collapse = ''),
                       x = text))  



## Step 2. Make the dtm 
# note that we will make it relatively quick and dirty here

tweetsTokenized <- tweetsLDA_df %>% unnest_tokens(output = "word",
                                                  input = text,
                                                  token = "words",
                                                  drop=FALSE,to_lower=TRUE)

# do some basic preprocessing steps:

tweetsTokenized <- tweetsTokenized %>%
  anti_join(stop_words) %>%       # note that we use all stopword dictionaries here
  count(status_id,word , sort=TRUE) %>%
  cast_dtm(document = status_id, term = word,
           value = n, weighting = tm::weightTf)


if (!require("topicmodels")) install.packages("topicmodels", quiet=TRUE) ; require("topicmodels")


tweets_lda <- LDA(tweetsTokenized, k = 3,method="gibbs",control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 2:6) )
tweets_lda




tweet_topics <- tidy(tweets_lda, matrix = "beta")

# you can use the following code to get the top terms per topic
top_tweet_terms <- tweet_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_tweet_terms

if (!require("ggplot2")) install.packages("ggplot2", quiet=TRUE) ; require("ggplot2")

top_tweet_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()


# 2. get the topics per document: which topics determine the documents?
# in this model, this is captured in @gamma of the model
# These values give the estimated proportion of words in that tweet from topic 1, topic 2, .... 
# The higher the value, the better, because we have better distinction between topics

tweet_documents <- tidy(tweets_lda, matrix = "gamma")
# CHoose, per tweet, the most important topic (the one with the highest weight)
tweet_doc_topic <- tweet_documents %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1) 

tweet_doc_topic %>%
  group_by(topic) %>% 
  summarise(nbr_documents = n())




