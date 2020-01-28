# Gambling R Project
install.packages("haven")
library(haven)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(date)
library(datetime)
library(readxl)
library(readr)
if(!require("plyr")) install.packages("plyr");library("plyr")

#Data Cleaning
#Demographics data
#rm(list=ls())

#Reading data from SAS File

RawDataIDemographics <- read_sas("RawDataIDemographics.sas7bdat", NULL)
RawDataIIUserDailyAggregation <- read_sas("RawDataIIUserDailyAggregation.sas7bdat", NULL)
RawDataIIIPokerChipConversions <- read_sas("RawDataIIIPokerChipConversions.sas7bdat", NULL)
AnalyticDataInternetGambling <- read_sas("AnalyticDataInternetGambling.sas7bdat", NULL)


#Format date
RawDataIDemographics$FirstPay <- ymd(RawDataIDemographics$FirstPay)

#Separate the column transaction date and time 
RawDataIIIPokerChipConversions <- separate(RawDataIIIPokerChipConversions,TransDateTime, c("TransDate", "TransTime"),sep =" ")
RawDataIIIPokerChipConversions$TransDate <- as.Date(RawDataIIIPokerChipConversions$TransDate)

#Merge and Filter out records for 8 months
DemoChipMerge<- merge(x = RawDataIIIPokerChipConversions, y = RawDataIDemographics[ , c("UserID", "FirstPay")], by = "UserID", all.x=TRUE)

#Filtering Out Users for TransDate>= users first betting money 
#deposition date and less than 30th Sep
RawDataIIIPokerChipConversions <- DemoChipMerge %>% filter(TransDate >= FirstPay & TransDate <= as.Date("2005-09-30") & TransDate >= as.Date("2005-02-01"))

#Drop the Merge table,since PokerConversion table has now valid records
DemoChipMerge <- NULL
RawDataIIIPokerChipConversions$FirstPay <- NULL

# set the Amount to 0 where TransAmount is less than 0,as it does not make a difference.
RawDataIIIPokerChipConversions$TransAmount <- ifelse(RawDataIIIPokerChipConversions$TransAmount < 0 , 0, RawDataIIIPokerChipConversions$TransAmount)

# Converting UserID into integer                  
RawDataIIIPokerChipConversions$UserID <- as.integer(RawDataIIIPokerChipConversions$UserID)

#Convert the type column values into sell and buy
RawDataIIIPokerChipConversions$TransType <- 
  ifelse(RawDataIIIPokerChipConversions$TransType == 124, "Sell", "Buy")

#Convert into tibble
RawDataIIIPokerChipConversions <- as_tibble(RawDataIIIPokerChipConversions)

#Create summarise columns to examine the behaviour of each user
#Using ddply
#Reference: http://qsel.columbia.edu/Nigeria_R_Training/Day3.html

RawDataIIIPokerChipConversions<-    ddply(RawDataIIIPokerChipConversions,.(UserID),summarize,
                                          First_TransDate_Po = min(TransDate),
                                          Last_TransDate_Po = max(TransDate),
                                          No_Months_Played_Po = n_distinct(month(TransDate)),
                                          Most_Played_Month_Po = names(which.max(table(month(TransDate)))),
                                          Most_Played_Day_Po = names(which.max(table(weekdays(TransDate)))),
                                          No_Days_Played_Po = n_distinct(TransDate),
                                          Total_Amount_Sold = sum(TransAmount[TransType == "Sell"]),
                                          Total_Amount_Bought = sum(TransAmount[TransType == "Buy"]),
                                          Mean_Amount_Sold = mean(TransAmount[TransType == "Sell"]),
                                          Mean_Amount_Bought = mean(TransAmount[TransType == "Buy"]),
                                          Max_Amount_Sold = max(TransAmount[TransType == "Sell"]),
                                          Max_Amount_Bought = max(TransAmount[TransType == "Buy"]),
                                          Amount_Sold_Last_Day = sum(TransAmount[TransDate %in% max(TransDate) & TransType == "Sell"]),
                                          Amount_Bought_Last_Day = sum(TransAmount[TransDate %in% max(TransDate) & TransType == "Buy"]),.drop = FALSE)


#Conver UseiId to int
RawDataIIIPokerChipConversions$UserID <- as.integer(RawDataIIIPokerChipConversions$UserID)

# Reading the country, region and application data to get the meaningfyl names 
# Fetched Latitude and Longitude using geo map locations by API key and added in the
#Country csv file, to make at first run to make it robust.
#register_google(key = "register_google(key = "AIzaSyC8tRJl2yBKsQ3fGfzPGArP5m64ETU7ZGo") ")
#Country <- mutate_geocode(Country, CountryName) 

Country <- read_csv("Country.csv") 
Language <- read_excel("Language.xlsx")
Application <- read_excel("Application.xlsx")


#Filter the records from feb till Sep
RawDataIDemographics <- RawDataIDemographics %>% filter(FirstPay >= as.Date("2005-02-01") 
                                                        & FirstPay <= as.Date("2005-09-30"))
#replace 0 and 1 Gender as Female and Male 
RawDataIDemographics$Gender <- ifelse(RawDataIDemographics$Gender == 0,"Female","Male" )

#There is one NA value in Gender
#RawDataIDemographics[is.na(RawDataIDemographics$Gender),]

#Replace that Gender value with Unknown
RawDataIDemographics$Gender[is.na(RawDataIDemographics$Gender)] <- "Unknown"

# Replace Country Language and Application Id Description codes with Real values 
RawDataIDemographics <- merge(x = RawDataIDemographics, y = Country, by = "Country", all.x = TRUE)
RawDataIDemographics <- merge(x = RawDataIDemographics, y = Language, by = "Language", all.x = TRUE)
RawDataIDemographics <- merge(x = RawDataIDemographics, y = Application,by = "ApplicationID", all.x = TRUE)

#Convet into Date
RawDataIDemographics$FirstPo= as.Date(RawDataIDemographics$FirstPo, "%Y%m%d") 
RawDataIDemographics$FirstSp= as.Date(RawDataIDemographics$FirstSp, "%Y%m%d") 
RawDataIDemographics$FirstCa= as.Date(RawDataIDemographics$FirstCa, "%Y%m%d") 
RawDataIDemographics$FirstGa= as.Date(RawDataIDemographics$FirstGa, "%Y%m%d") 
RawDataIDemographics$FirstAct= as.Date(RawDataIDemographics$FirstAct, "%Y%m%d")

#Removing unwanted columns and renaming
RawDataIDemographics$ApplicationID<-NULL
RawDataIDemographics$Language<-NULL
RawDataIDemographics$Country<-NULL

# Replace Country Language and Application Id Description codes with Real values 
AnalyticDataInternetGambling <- merge(x = AnalyticDataInternetGambling, y = Language, by = "Language","LANGUAGE" ,all.x = TRUE)
AnalyticDataInternetGambling <- merge(x = AnalyticDataInternetGambling, y = Country, by = "Country","COUNTRY", all.x = TRUE)

#Replace 0 and 1 Gender as Female and Male and other format
AnalyticDataInternetGambling$GENDER <- ifelse(AnalyticDataInternetGambling$GENDER == 0,"Female","Male" )
AnalyticDataInternetGambling$RegistrationDate <- as.Date(AnalyticDataInternetGambling$RegistrationDate)

# Remove Unwanted Columns
RawDataIDemographics$Language<-NULL
RawDataIDemographics$Country<-NULL

#Daily Aggregations:
RawDataIIUserDailyAggregation$Date <- ymd(RawDataIIUserDailyAggregation$Date)

#Merge data to filter out the records from feb till sep
DemoAggreMerge <- merge(x = RawDataIIUserDailyAggregation, y = RawDataIDemographics[ , c("UserID", "FirstPay")], by = "UserID", all.x=TRUE)
RawDataIIUserDailyAggregation <- DemoAggreMerge %>% filter(Date >= FirstPay & Date <= as.Date("2005-09-30") & Date >= as.Date("2005-02-01"))

#Remove the demo table and column
DemoAggreMerge <- NULL
RawDataIIUserDailyAggregation$FirstPay <- NULL

# Create Summarise columns for each product
#for sports book live and odd
RawDataIIUserDailyAggregation_Sp <- RawDataIIUserDailyAggregation %>% filter(ProductID == 1 | ProductID == 2)

RawDataIIUserDailyAggregation_Sp_clean<-ddply(RawDataIIUserDailyAggregation_Sp,.(UserID),summarize,
                                              First_TransDate_Sp = min(Date),
                                              Total_Stakes_Sp = round(sum(Stakes),1), 
                                              Max_Stakes_Sp = max(Stakes), 
                                              Mean_Stakes_Sp=round(mean(Stakes),1),
                                              Total_Winnings_Sp=round(sum(Winnings),1),
                                              Max_Winnings_Sp = max(Winnings),
                                              Mean_Winnings_Sp=round(mean(Winnings),1),
                                              Total_Bets_Sp= round(sum(Bets),1), 
                                              Length_Play_Sp= (max(Date)-min(Date)) ,
                                              LastDay_Sp = max(Date),
                                              No_Days_Played_Sp = n_distinct(Date),
                                              No_Months_Played_Sp = n_distinct(month(as.Date(Date,format="%Y%m%d"))),
                                              Most_Played_Month_Sp = names(which.max(table(month(as.Date(Date,format="%Y%m%d"))))),
                                              MostPlayed_Day_Sp = names(which.max(table(weekdays(as.Date(Date))))), 
                                              Stakes_Last_Day_Sp = sum(Stakes[Date %in% max(Date) ]),
                                              Winnings_Last_Day_Sp = sum(Winnings[Date %in% max(Date) ]),
                                              Bets_Last_Day_Sp = sum(Bets[Date %in% max(Date) ]),
                                              .drop = FALSE)

#For casino
RawDataIIUserDailyAggregation_Ca <- RawDataIIUserDailyAggregation %>% filter(ProductID == 4 | ProductID == 8)

RawDataIIUserDailyAggregation_Ca_clean<- ddply(RawDataIIUserDailyAggregation_Ca,.(UserID),summarize,
                                               First_TransDate_Ca = min(Date),
                                               Total_Stakes_Ca = round(sum(Stakes),1), 
                                               Max_Stakes_Ca = max(Stakes), 
                                               Mean_Stakes_Ca=round(mean(Stakes),1),
                                               Total_Winnings_Ca =round(sum(Winnings),1),
                                               Max_Winnings_Ca = max(Winnings),
                                               Mean_Winnings_Ca=round(mean(Winnings),1),
                                               Total_Bets_Ca= round(sum(Bets),1), 
                                               Length_Play_Ca= (max(Date)-min(Date)) ,
                                               LastDay_Ca = max(Date),
                                               No_Days_Played_Ca = n_distinct(Date),
                                               No_Months_Played_Ca = n_distinct(month(as.Date(Date,format="%Y%m%d"))),
                                               Most_Played_Month_Ca = names(which.max(table(month(as.Date(Date,format="%Y%m%d"))))),
                                               MostPlayed_Day_Ca = names(which.max(table(weekdays(as.Date(Date))))), 
                                               Stakes_Last_Day_Ca = sum(Stakes[Date %in% max(Date) ]),
                                               Winnings_Last_Day_Ca = sum(Winnings[Date %in% max(Date) ]),
                                               Bets_Last_Day_Ca = sum(Bets[Date %in% max(Date) ]),
                                               .drop = FALSE)
#for Games Sports Book
RawDataIIUserDailyAggregation_Ga <- RawDataIIUserDailyAggregation %>% filter(ProductID == 5 | ProductID == 6 | ProductID == 7)

RawDataIIUserDailyAggregation_Ga_clean<- ddply(RawDataIIUserDailyAggregation_Ga,.(UserID),summarize,
                                               First_TransDate_Ga = min(Date),
                                               Total_Stakes_Ga = round(sum(Stakes),1), 
                                               Max_Stakes_Ga = max(Stakes), 
                                               Mean_Stakes_Ga=round(mean(Stakes),1),
                                               Total_Winnings_Ga=round(sum(Winnings),1),
                                               Max_Winnings_Ga = max(Winnings),
                                               Mean_Winnings_Ga=round(mean(Winnings),1),
                                               Total_Bets_Ga= round(sum(Bets),1), 
                                               Length_Play_Ga= (max(Date)-min(Date)) ,
                                               LastDay_Ga = max(Date),
                                               No_Days_Played_Ga = n_distinct(Date),
                                               No_Months_Played_Ga = n_distinct(month(as.Date(Date,format="%Y%m%d"))),
                                               Most_Played_Month_Ga = names(which.max(table(month(as.Date(Date,format="%Y%m%d"))))),
                                               MostPlayed_Day_Ga = names(which.max(table(weekdays(as.Date(Date))))), 
                                               Stakes_Last_Day_Ga = sum(Stakes[Date %in% max(Date) ]),
                                               Winnings_Last_Day_Ga = sum(Winnings[Date %in% max(Date) ]),
                                               Bets_Last_Day_Ga = sum(Bets[Date %in% max(Date) ]),
                                               .drop = FALSE)

# Merging Sports with Sports Analytics to have an overall summary of Sports data (including FO and LA)

Gambling <- merge(x=AnalyticDataInternetGambling, y=RawDataIIUserDailyAggregation_Sp_clean, by = "UserID","USERID", all.x = T)

Gambling$LANGUAGE<-NULL
Gambling$COUNTRY<-NULL
#Gambling[is.na(Gambling$CountryName),]
#Replace that Gender value with Unknown
Gambling$GENDER[is.na(Gambling$GENDER)] <- "Unknown"

########################## Creation of Final Gambling Data(Data Mart) ####################################
# Creating and Merging new olumns on the basis of RFM: Recency Frequency and Monetary

# Creating the profit made by online Website, played Casino
RawDataIIUserDailyAggregation_Ca_clean$Profit_Ca <- RawDataIIUserDailyAggregation_Ca_clean$Total_Stakes_Ca - RawDataIIUserDailyAggregation_Ca_clean$Total_Winnings_Ca

# Creating the profit made by online Website, played Games
RawDataIIUserDailyAggregation_Ga_clean$Profit_Ga <- RawDataIIUserDailyAggregation_Ga_clean$Total_Stakes_Ga - RawDataIIUserDailyAggregation_Ga_clean$Total_Winnings_Ga

# Creating the profit made by online Website,played Poker
RawDataIIIPokerChipConversions$Profit_Po <- RawDataIIIPokerChipConversions$Total_Amount_Bought - RawDataIIIPokerChipConversions$Total_Amount_Sold

# Creating the profit made by online Website, played Sports
Gambling$Profit_Sp <-  Gambling$Total_Stakes_Sp - Gambling$Total_Winnings_Sp
Gambling$Profit_FO <-  Gambling$FOTotalStakes - Gambling$FOTotalWinnings
Gambling$Profit_LA <- Gambling$LATotalStakes - Gambling$LATotalWinnings

Gambling$USERID <- as.integer(Gambling$USERID)
RawDataIDemographics$UserID <- as.integer(RawDataIDemographics$UserID)

# Final Base Table: Merge
Basetable_Gambling <- merge(x=RawDataIDemographics, y=Gambling, by="UserID", by.y = "USERID", all.x = T)

Basetable_Gambling <- merge(x=Basetable_Gambling, y=RawDataIIIPokerChipConversions, by = "UserID", all.x = T)
Basetable_Gambling <- merge(x=Basetable_Gambling, y=RawDataIIUserDailyAggregation_Ga_clean, by = "UserID", all.x = T)
Basetable_Gambling <- merge(x=Basetable_Gambling, y=RawDataIIUserDailyAggregation_Ca_clean, by = "UserID", all.x = T)

#Create New Columns
Basetable_Gambling$Country_Name<-Basetable_Gambling$CountryName.x
Basetable_Gambling['Application_Description']<-Basetable_Gambling['Application Description']
Basetable_Gambling['Language_Description']<-Basetable_Gambling['Language Description.y']

# DROP unwanted columns
Basetable_Gambling$RegDate <-NULL               
Basetable_Gambling$FirstPay<-NULL             
Basetable_Gambling$FirstAct<-NULL               
Basetable_Gambling$FirstSp <-NULL              
Basetable_Gambling$FirstCa <-NULL                
Basetable_Gambling$FirstGa <-NULL               
Basetable_Gambling$GENDER <-NULL               
Basetable_Gambling$CountryName.y  <-NULL
Basetable_Gambling$CountryName.x  <-NULL
Basetable_Gambling['Language Description.x']<-NULL
Basetable_Gambling['Language Description.y']<-NULL
Basetable_Gambling['Application Description']<-NULL

#Replace that Gender value with Unknown
Gambling$GENDER[is.na(Gambling$GENDER)] <- "Unknown"


#Calculate Total Stakes and Winnings
Basetable_Gambling$Total_Stakes <- Basetable_Gambling$Total_Stakes_Sp + Basetable_Gambling$Total_Stakes_Ga+Basetable_Gambling$Total_Stakes_Ca + Basetable_Gambling$Total_Amount_Bought
Basetable_Gambling$Total_Winnings <- Basetable_Gambling$Total_Winnings_Sp + Basetable_Gambling$Total_Winnings_Ga + Basetable_Gambling$Total_Winnings_Ca + Basetable_Gambling$Total_Amount_Sold

#Calculate Total Profit
Basetable_Gambling$Total_Profit_Gambling<- Basetable_Gambling$Profit_FO + Basetable_Gambling$Profit_LA + 
Basetable_Gambling$Profit_Po + Basetable_Gambling$Profit_Ga + Basetable_Gambling$Profit_Ca

#Calculate Total Bets
Basetable_Gambling$Total_Bets_Gambling <- Basetable_Gambling$Total_Bets_Ca + Basetable_Gambling$Total_Bets_Ga + Basetable_Gambling$Total_Bets_Sp

#Calculate Total No Of Days Played
Basetable_Gambling$Total_No_Days_Played_Gambling <- Basetable_Gambling$No_Days_Played_Ca + Basetable_Gambling$No_Days_Played_Ga + Basetable_Gambling$No_Days_Played_Sp + Basetable_Gambling$No_Days_Played_Po

#Calculate Profit margin gained or loss

Basetable_Gambling$Total_Profit_Margin_Pcnt <- ((Basetable_Gambling$Total_Stakes - Basetable_Gambling$Total_Winnings) / Basetable_Gambling$Total_Stakes ) * 100 
Basetable_Gambling$Total_Profit_Margin_Pcnt <- replace(Basetable_Gambling$Total_Profit_Margin_Pcnt, is.na(Basetable_Gambling$Total_Profit_Margin_Pcnt), 0)

#Create a new column Age Group 
Basetable_Gambling$Age_Group <- ifelse(Basetable_Gambling$AGE <= 18, '< 18 Years old',
                                       ifelse((Basetable_Gambling$AGE >18 & Basetable_Gambling$AGE <= 35),'18-35 Years old',
                                              ifelse((Basetable_Gambling$AGE >35 & Basetable_Gambling$AGE <= 55),'35-55 Years old',
                                                     ifelse((Basetable_Gambling$AGE > 55 & Basetable_Gambling$AGE <= 75),'55-75 Years old',
                                                            ifelse(Basetable_Gambling$AGE > 75,'75+ Years old','Not Known')))))


Basetable_Gambling$Age_Group[is.na(Basetable_Gambling$Age_Group)] <- "Unknown"

#the below few lineds of Code is to check Loyal Gamblers

#Last Transaction of the Gamblers done
Basetable_Gambling[, "Last_Played_Date"] <- as.Date(apply(Basetable_Gambling[, c("LastDay_Sp", "LastDay_Ga", "Last_TransDate_Po", "LastDay_Ca")], 1, max, na.rm=TRUE),"%Y-%m-%d")

#First Transaction of the Gamblers done
Basetable_Gambling[, "First_Played_Date"] <- as.Date(apply(Basetable_Gambling[, c("First_TransDate_Sp", "First_TransDate_Po",  "First_TransDate_Ga","First_TransDate_Ca")], 1, min, na.rm=TRUE),"%Y-%m-%d")

#Length OF RelationShip Of Gamblers
Basetable_Gambling$Length_Of_Relationship <-Basetable_Gambling$Last_Played_Date - Basetable_Gambling$First_Played_Date


# Creating a new column Loyalty, As in the decsription of the BRD given, the cut off date is Sep30 2005
#so to check the loyalty, we have subtracted the last date of transaction from sep 30

Basetable_Gambling$Loyalty <- as.Date("20050930","%Y%m%d")- Basetable_Gambling$Last_Played_Date

#Creating Gambling behavious of the Gamblers can help in identifying the churn status
Basetable_Gambling$Loyalty_Status <- ifelse(Basetable_Gambling$Loyalty <= 60, 'High Loyalty',
                          ifelse((Basetable_Gambling$Loyalty>60 &Basetable_Gambling$Loyalty<=154),'Medium Loyalty',
                                 ifelse(Basetable_Gambling$Loyalty>154,'Low Loyalty','Not Known')))


#Assign Latitude and Longitude Columns:
#Create New Columns
Basetable_Gambling$Latitude<-Basetable_Gambling$lat.x
Basetable_Gambling$Longitude<-Basetable_Gambling$lon.x

#Remove Unwanted Columns and Clean the data:
Basetable_Gambling$Age<-NULL
Basetable_Gambling$FirstPo<-NULL
Basetable_Gambling$Age<-NULL
Basetable_Gambling$lat.x<-NULL
Basetable_Gambling$lon.x<-NULL
Basetable_Gambling$lat.y<-NULL
Basetable_Gambling$lon.y<-NULL


Basetable_Gambling$Loyalty_Status[is.na(Basetable_Gambling$Loyalty_Status)] <- "Unknown"

#Creating Factors:
Basetable_Gambling$Gender <- as.factor(Basetable_Gambling$Gender)
Basetable_Gambling$Application_Description <- as.factor(Basetable_Gambling$Application_Description)
Basetable_Gambling$Age_Group <- as.factor(Basetable_Gambling$Age_Group)
Basetable_Gambling$Country_Name <- as.factor(Basetable_Gambling$Country_Name) 

#Write to Files
write.csv(Basetable_Gambling, file = "Basetable_Gambling.csv")
write.csv(RawDataIDemographics, file = "RawDataIDemographics.csv")
write.csv(AnalyticDataInternetGambling, file = "AnalyticDataInternetGambling.csv")
write.csv(RawDataIIIPokerChipConversions, file = "RawDataIIIPokerChipConversions.csv")
write.csv(RawDataIIUserDailyAggregation, file = "RawDataIIUserDailyAggregation.csv")
write.csv(RawDataIIUserDailyAggregation_Ca_clean, file = "RawDataIIUserDailyAggregation_Ca_clean.csv")
write.csv(RawDataIIUserDailyAggregation_Ga_clean, file = "RawDataIIUserDailyAggregation_Ga_clean.csv")
write.csv(RawDataIIUserDailyAggregation_Sp_clean, file = "RawDataIIUserDailyAggregation_Sp_clean.csv")
save(Basetable_Gambling, file = "Basetable_Gambling.RData")


summary(Basetable_Gambling)

