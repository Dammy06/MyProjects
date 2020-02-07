#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
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

library(shiny)
shinyUI(pageWithSidebar(
    
    headerPanel("Chase Twitter Analysis"),
    
    # Getting User Inputs
    
    sidebarPanel(
        
        HTML("This Dashboard showcases the information extracted from the analysis of Twitter data of Chase Bank, One of the largest banks in America.
             The idea is to try and assess the overall sentiment related to this brand's online presence by analyzing data collected through Twitter API.
             
             *Kindly allow for sometime to infographs to load")
    ),
    
    mainPanel(
        
        
        tabsetPanel(
            
            tabPanel("Location & Timeline Analysis",HTML("Global Footprint of Chase Tweets"),
                     
                    
                     plotOutput("geomap"),HTML("Timeline of tweets generated"), plotOutput("Tweets_Distribution"),HTML("<div><h3> The data for these infographics have been collected for the duration of Nov 2019-Jan2020.   </h3></div>"),
                     tableOutput("trendtable"),
                     HTML
                     ("<div> </div>")),
            
            tabPanel("Source of Tweets",HTML("<div>Source of Chase_Support Tweets"),plotOutput("Source_Chase_Support"),HTML("<div>Types of tweets for Chase Support</div>"),plotOutput("typesoftweets_Chase_Support"),HTML("<div>Sources of Tweets of Chase </div>"),plotOutput("Source_Chase"),HTML("Types of tweets for Chase </div>"),plotOutput("typesoftweets_Chase"),HTML
                     ("<div> <h3>Different sources from which tweets are derived</h3></div>")),
            
            
            tabPanel("Text Analysis",HTML("<div><h3>Wordclouds of words used and Trending Hashtag</h3></div>"),plotOutput("wordcloudChasesupport"),plotOutput("wordcloudhashtag"),
                     HTML
                     ("<div><h4> A Wordcloud is a visual representation of text data, typically used to depict keyword metadata (tags) on websites, or to visualize free form text.
                 This format is useful for quickly perceiving the most prominent terms and for locating a term alphabetically to determine its relative prominence.
                 </h4></div>")
                     ),
            
           
            
            
            tabPanel("Sentiments_words",HTML("<div>Sentiments of Chase Support</div>"),plotOutput("Sentiments_Chase_Support"),HTML("<div>Sentiments of Chase</div>"),plotOutput("Sentiments_Chase"),HTML
                     ("<div> <h3>Sentiments of selected words </h3></div>")
                     ),
            
            tabPanel("Topic Modelling",HTML
                     ("<div><h4>The following graphs represent Topic Analysis for Chase. Topic 1 represents Tweets involving Customer Service, Topic 2 represents Tweets involving Banking Services, Topic 3 represents Tweets involving Credit Card Services. 
			</h4></div>"),plotOutput("TopicModelling")
                     ),
            
            tabPanel("Network Analysis",HTML( "<div><h3> This shows the relationship between words used in a tweet which contribute more to the assesment of sentiment in case of word joins. </h3></div>"), plotOutput("Network_Analysis"),
                     HTML ("<div><h4> A detailed version of this network can be found in the source code which allows us to identify the longest connections also. </h4></div>")
            ),
            
            
            
            tabPanel("Conclusion",HTML
                     ("<div> <h2>Overall Conclusion
<div> <h4>.	Data extracted using full archive, search 30 days, and get timeline functions.
<div> <h4>.	Data extracted and analyzed using @Chase and @ChaseSupport
<div> <h4>.	Data from @ChaseSupport ranges from November 1st, 2019 to January 29th, 2020. 
<div> <h4>.	Approximately 6,200 tweets gathered from @ChaseSupport
<div> <h4>.	@ChaseSupport account used exclusively for customer service (Only Organic Tweets and Replies, no Retweets).
<div> <h4>.	Overall Sentiment Analysis show customers reach out to Chase for immediate solutions on real-time problems.
<div> <h4>.	Analysis shows Chase is highly engaged with their customers on Twitter, replying to their mentions regularly and frequently.
<div> <h4>.	@Chase account used for promotions, mentions, and customer's complaints. 



<div> <h3>

<div> <h2>Application functionalities
<div> <h4>.	The Focus of the current project was on getting the sentiments of several accounts that are in frequent contact with two working twitter accounts of Chase Company i.e. @Chase and @ChaseSupport.
<div> <h4>.	We analyzed many aspects in this data such as estimation source of tweets, about retweets, popular words and hashtags, positive and negative sentiments and much more.
<div> <h4>.	We have focused especially on simple navigation with R shiny app with the approach that focus on simple design with effective results.
<div> <h4>.	The Sentiments are mostly positive in case of both @Chase and @ChaseSupport
<div> <h4>.	@Chase Support demonstrated positive response, as there were no retweets, which indicates that all the complaints were handled effectively.


<div> <h3>

<div> <h2>Main Challenges Faced
<div> <h4>.	Collection of data because of presence of certain restrictions.
<div> <h4>.	We were not able to make shiny work on dynamic data.
<div> <h4>.	Implementation of machine learning 
<div> <h4>

<div> <h2>Future Scope
<div> <h4>.	More in depth analysis could have been performed
<div> <h4>.	Map the Sentiment data with the stock price movement.
<div> <h4>.	Machine learning could be implemented.
</h3></div>")
            )
            
            
        )#end of tabset panel
    )#end of main panel
    
))#end of shinyUI