#Packages
if (!require(sas7bdat)) install.packages('sas7bdat')
library(sas7bdat)

if (!require(stringr)) install.packages('stringr')
library(stringr)

if (!require(data.table)) install.packages('data.table')
library(data.table)

if (!require(tidyr)) install.packages('tidyr')
library(tidyr)

if (!require(dplyr)) install.packages('dplyr')
library(dplyr)

if (!require(jsonlite)) install.packages('jsonlite')
library(jsonlite)

if (!require(rvest)) install.packages('rvest')
library(rvest)

if (!require(lubridate)) install.packages('lubridate')
library(lubridate)

if (!require(readxl)) install.packages('readxl')
library(readxl)

if (!require(stringr)) install.packages('stringr')
library(stringr)

if (!require(haven)) install.packages('haven')
library(haven)

if (!require(gridExtra)) install.packages('gridExtra')
library(gridExtra)

if (!require(tidyr)) install.packages('tidyr')
library(tidyr)

# Data Exploration function
explore <- function(df){
  print(colnames(df))
  print("")
  print(dim(df))
  print("")
  print(str(df))
  for (col in colnames(df)){
    print(paste("NA's in:     ", col))
    print(any(is.na(conversion[col])))
    if (any(is.na(conversion[col])) == TRUE){
      print(unique(conversion[col]))
    }
  }
}


# Read Datasets
path = "C:/Users/pborchert/Documents/GitHub/R_Group_Assignment/"
conversion <- read_sas(paste0(path, "RawDataIIIPokerChipConversions.sas7bdat"))
ADIG       <- read.sas7bdat(paste0(path, "AnalyticDataInternetGambling.sas7bdat"))
Demo       <- read.sas7bdat(paste0(path, "RawDataIDemographics.sas7bdat"))
DaylyAGG   <- read.sas7bdat(paste0(path, "RawDataIIUserDailyAggregation.sas7bdat"))

#-------------------------------------Demo--------------------------------------------
# 1. RawDataIDemographics
Demo1 <- Demo
explore(Demo1)
glimpse(Demo1)
sum(is.na(Demo1$FirstCa))
sum(is.null(Demo1$FirstCa))

#formating as date: RegDate, FirstPay, FirstAct, FirstSp, FirstCa, 
                    #FirstGa, FirstPo, 
Demo1$RegDate  <-as.character(as.Date.character(Demo1$RegDate,tryFormats = c("%Y-%m-%d","%Y%m%d")))
Demo1$RegDate  <- as.Date.character(as_date(Demo1$RegDate))
Demo1$FirstPay <- as.character(as.Date.character(Demo1$FirstPay,tryFormats = c("%Y-%m-%d","%Y%m%d")))
Demo1$FirstPay <- as.Date.character(as_date(Demo1$FirstPay))
Demo1$FirstAct <- as.character(as.Date.character(Demo1$FirstAct,tryFormats = c("%Y-%m-%d","%Y%m%d")))
Demo1$FirstAct <- as.Date.character(as_date(Demo1$FirstAct))
Demo1$FirstSp  <- as.character(as.Date.character(Demo1$FirstSp,tryFormats = c("%Y-%m-%d","%Y%m%d")))
Demo1$FirstSp  <- as.Date.character(as_date(Demo1$FirstSp))
Demo1$FirstCa  <- as.character(as.character(Demo1$FirstCa))
Demo1$FirstCa  <- as.character(as_date(Demo1$FirstCa))
Demo1$FirstCa  <- as.Date.character(as_date(Demo1$FirstCa))
Demo1$FirstGa  <- as.character(as.character(Demo1$FirstGa))
Demo1$FirstGa  <- as.character(as_date(Demo1$FirstGa))
Demo1$FirstGa  <- as.Date.character(as_date(Demo1$FirstGa))
Demo1$FirstPo  <- as.character(as.character(Demo1$FirstPo))
Demo1$FirstPo  <- as.character(as_date(Demo1$FirstPo))
Demo1$FirstPo  <- as.Date.character(as_date(Demo1$FirstPo))
sum(is.na(Demo1))

#now the NULL values are saved as NA
sum(is.na(Demo1$RegDate))
sum(is.na(Demo1$FirstPay))
sum(is.na(Demo1$FirstAct)) #participants who create an account but never played (need to verity with the rest of data sets)
sum(is.na(Demo1$FirstSp)) #participants who havent used sports book 
sum(is.na(Demo1$FirstCa)) #participants who havent play casino
sum(is.na(Demo1$FirstGa)) #participants who havent play games
sum(is.na(Demo1$FirstPo)) #participants who havent play poker

#renaming columns
Demo1<-rename(Demo1, First_time_sports= FirstSp)
Demo1<-rename(Demo1, First_time_casino= FirstCa)
Demo1<-rename(Demo1, First_time_games= FirstGa)
Demo1<-rename(Demo1, First_time_poker= FirstPo)

glimpse(Demo1)

# creating new columns based on the variables (not yet)
#Demo1$Year_reg <-  year(Demo1$RegDate)
#Demo1$Month_reg <-  month(Demo1$RegDate)
#Demo1$Day_reg <-  day(Demo1$RegDate)

#glimpse(Demo1)

# checking USERID
count(unique(Demo1$UserID))
sum(duplicated(Demo1$UserID))
sum(is.na(Demo1$UserID)) 
sum(is.null(Demo1$UserID))

#-------------------------------------DaylyAGG------------------------------------------
# 2. RawDataIIUserDailyAggregation
DaylyAGG_1 <- DaylyAGG
glimpse(DaylyAGG_1)
explore(DaylyAGG_1)
sum(is.na(DaylyAGG_1))
sum(is.null(DaylyAGG_1))
head(DaylyAGG_1)

#formating as date: RegDate,
DaylyAGG_1$Date <- as.character(as.Date.character(DaylyAGG_1$Date,tryFormats = c("%Y-%m-%d","%Y%m%d")))
DaylyAGG_1$Date <- as.Date.character(as_date(DaylyAGG_1$Date))
glimpse(DaylyAGG_1)

# chage name of Date to betting_date to not get confused
DaylyAGG_1<-rename(DaylyAGG_1, betting_date= Date)
glimpse(DaylyAGG_1)

#separating betting_date in order to be able to group_by year, month or day (will see haha)
DaylyAGG_1$betting_year  <-  year(DaylyAGG_1$betting_date)
DaylyAGG_1$betting_month <-  month(DaylyAGG_1$betting_date)
DaylyAGG_1$betting_day   <-  day(DaylyAGG_1$betting_date)
glimpse(DaylyAGG_1)

#check the unique values in betting_year/month,day to see a count 
unique(DaylyAGG_1$betting_year) #not by year because it is only 1 year.
unique(DaylyAGG_1$betting_month) # its possible because we have 8
unique(DaylyAGG_1$betting_day) # its possible because we have 31
sum(is.na(DaylyAGG_1)) 
sum(is.null(DaylyAGG_1))
DaylyAGG_1$UserID["1324354"]

# group by month because if we do it by day we will have 248 columns 
# because are 31 days per month (8*31)
DaylyAGG_2 <- DaylyAGG_1
head(DaylyAGG_2, n=100)
unique(DaylyAGG_2$ProductID)
glimpse(DaylyAGG_2)

#new varibale with product description
DaylyAGG_2 <-mutate(DaylyAGG_2, 
                    product_description=case_when(ProductID==1 ~ 'Betted_Sports_fixed_odd',
                                                  ProductID==2 ~ 'Betted_Sports_fixed_live',
                                                  ProductID==4 ~ 'Betted_Casino_boss',
                                                  ProductID==5 ~ 'Betted_Supertoto',
                                                  ProductID==6 ~ 'Betted_Games_VS',
                                                  ProductID==7 ~ 'Betted_Games_bwin',
                                                  ProductID==8 ~ 'Betted_Casino_chartwell'))

head(DaylyAGG_2)

# 2.1 the next table is for creating charts because is very informative
informative_table<- DaylyAGG_2 %>% 
  group_by(UserID,betting_month, product_description) %>% 
  summarise(avg_bet=mean(Bets),
            min_bet=min(Bets),
            max_bet=max(Bets),
            total_number_bets=sum(Bets),
            avg_winnings=mean(Winnings),
            min_winnings=min(Winnings),
            max_winnings=max(Winnings),
            total_money_bets=sum(Stakes),
            min_money_bets=min(Stakes),
            max_money_bets=max(Stakes),
            mean_money_bets=mean(Stakes),
            total_times_product = n())

# test1<-DaylyAGG_1[order(DaylyAGG_1$UserID),] ### I used this for verify "the total_times_product"
# and it is correct i count the number of times per client, month and product

# 2.3 the next table if for mergin
mergin1<-informative_table
head(mergin1)

mergin2<-spread(mergin1,product_description,total_times_product)
mergin2$betting_month <- mergin2$betting_month <- NULL #drop this for the merge
glimpse(mergin2)
#replacing 0 in NA de new columns
mergin2$Betted_Casino_boss      <- replace(mergin2$Betted_Casino_boss, is.na(mergin2$Betted_Casino_boss), 0)
mergin2$Betted_Casino_chartwell <- replace(mergin2$Betted_Casino_chartwell, is.na(mergin2$Betted_Casino_chartwell), 0)
mergin2$Betted_Games_bwin       <- replace(mergin2$Betted_Games_bwin, is.na(mergin2$Betted_Games_bwin), 0)
mergin2$Betted_Games_VS         <- replace(mergin2$Betted_Games_VS, is.na(mergin2$Betted_Games_VS), 0)
mergin2$Betted_Sports_fixed_live<- replace(mergin2$Betted_Sports_fixed_live, is.na(mergin2$Betted_Sports_fixed_live), 0)
mergin2$Betted_Sports_fixed_odd <- replace(mergin2$Betted_Sports_fixed_odd, is.na(mergin2$Betted_Sports_fixed_odd), 0)
mergin2$Betted_Supertoto        <- replace(mergin2$Betted_Supertoto, is.na(mergin2$Betted_Supertoto), 0)
glimpse(mergin2)

# Group data by UserID
DaylyAGG_for_mergin<-mergin2%>%
  group_by(UserID)%>%
  summarise(avg_bet=mean(avg_bet),
            min_bet=min(min_bet),
            max_bet=max(max_bet),
            total_number_bets=sum(total_number_bets),
            avg_winnings=mean(avg_winnings),
            min_winnings=min(min_winnings),
            max_winnings=max(max_winnings),
            total_money_bets=sum(total_money_bets),
            min_money_bets=min(min_money_bets),
            max_money_bets=max(max_money_bets),
            mean_money_bets=mean(mean_money_bets),
            Betted_Casino_boss=sum(Betted_Casino_boss),
            Betted_Casino_chartwell=sum(Betted_Casino_chartwell),
            Betted_Games_bwin=sum(Betted_Games_bwin),
            Betted_Games_VS=sum(Betted_Games_VS),
            Betted_Sports_fixed_live=sum(Betted_Sports_fixed_live),
            Betted_Sports_fixed_odd=sum(Betted_Sports_fixed_odd),
            Betted_Supertoto=sum(Betted_Supertoto))

#-------------------------------------ADIG-------------------------------------------
# 3. AnalyticDataInternetGambling
ADIG1<-ADIG
head(ADIG1)
glimpse(ADIG1)
explore(ADIG1)

#change type to date
ADIG1$RegistrationDate<-as_date(as.numeric(ADIG1$RegistrationDate))
ADIG1$FOFirstActiveDate<-as_date(as.numeric(ADIG1$FOFirstActiveDate))
ADIG1$FOLastActiveDate<-as_date(as.numeric(ADIG1$FOLastActiveDate))
ADIG1$LAFirstActiveDate<-as_date(as.numeric(ADIG1$LAFirstActiveDate))
ADIG1$LALastActiveDate<-as_date(as.numeric(ADIG1$LALastActiveDate))
ADIG1$FirstSportsActiveDate<-as_date(as.numeric(ADIG1$FirstSportsActiveDate))

glimpse(ADIG1)

#4. mergin tables

# mergin Demo & country_table in order to obtain the name of each country
country_table <- read_excel("C:/Users/fceli/Desktop/BIG DATA/7. Business Analytics Tools - Open Source/Group Assignment/FC/CountryTable.xlsx")

#
#-------------------------------------Conversions--------------------------------------

# Read Dataset
path = "C:/Users/pborchert/Documents/GitHub/R_Group_Assignment/"
conversion <- read_sas(paste0(path, "RawDataIIIPokerChipConversions.sas7bdat"))

#Explore Dataset

explore <- function(df){
  print(colnames(df))
  print("")
  print(dim(df))
  print("")
  print(str(df))
  for (col in colnames(df)){
    print(paste("NA's in:     ", col))
    print(any(is.na(conversion[col])))
    if (any(is.na(conversion[col])) == TRUE){
      print(unique(conversion[col]))
    }
  }
}

explore(conversion)
glimpse(conversion)

# TransDateTime column to date format
conversion$TransDateTime <- ymd_hms(conversion$TransDateTime)

# Count Nr. of conversions for each UserID
count_conv <- conversion %>% 
  count(UserID) %>%
  rename(nr_conversions = n)

# AVG time difference between conversions
conversion <- conversion %>% arrange(UserID, TransDateTime)
conversion$timediff_hours <- NA
conversion$timediff_hours <- mapply(function(date, date1, id, id1) ifelse(id==id1, difftime(date1, date, units="hours"), NA),
                                    conversion$TransDateTime, conversion$TransDateTime[2:length(conversion$TransDateTime)], 
                                    conversion$UserID, conversion$UserID[2:length(conversion$UserID)])


# columns buy/sell amount
conversion$sell_amount <- 0
conversion$buy_amount <- 0

conversion <- conversion %>% 
  mutate(sell_amount = ifelse(TransType == 124, TransAmount, 0),
         buy_amount = ifelse(TransType == 24, TransAmount, 0))

# Check correct Data Assignment
#View(conversion)
any(conversion$sell_amount == conversion$buy_amount)
conversion[which(conversion$sell_amount == conversion$buy_amount),]

# group conversion table by UserID and join nr_conversions
conversion_merge <- conversion %>% 
  group_by(UserID) %>% 
  summarize(total_conv_amount = sum(TransAmount),
            avg_conv_amount   = mean(TransAmount),
            total_sell_amount = sum(sell_amount),
            total_buy_amount  = sum(buy_amount),
            avg_timediff_hours= mean(timediff_hours, na.rm=TRUE)) %>%
  left_join(count_conv, by = "UserID")

# Validate final table
dim(conversion_merge)[1] == length(unique(conversion$UserID))

for (col in colnames(conversion_merge)){
  print(col)
  print(any(is.na(conversion_merge[col])))
  print(any(is.null(conversion_merge[col])))
}
# For Users with 1 conversion: avg_timediff_hours = NA
conversion %>% filter(UserID == 1324418)


#---------------------------------------Datamart--------------------------------------

# Inspect Tables for merge
dim(ADIG1)
dim(Demo1)
dim(DaylyAGG_for_mergin)
dim(conversion_merge)

length(unique(ADIG1$USERID))
length(unique(Demo1$UserID))
length(unique(DaylyAGG_for_mergin$UserID))
length(unique(conversion_merge$UserID))
# different UserID's -> outer join

# Merge tables

ADIG1 <- ADIG1 %>% rename(UserID = USERID)
ADIG1 <- ADIG1 %>% rename(Gender = GENDER)
ADIG1 <- ADIG1 %>% rename(Country = COUNTRY)
ADIG1 <- ADIG1 %>% rename(Language = LANGUAGE)
ADIG1 <- ADIG1 %>% rename(Age = AGE)

datamart <- conversion_merge %>%
              full_join(ADIG1, by = "UserID") %>%
              full_join(Demo1, by = "UserID") %>%
              full_join(DaylyAGG_for_mergin, by = "UserID")

# Duplicate Country Language Gender (.x, .y) is removed 
# If a valid .x value is found it will be kept (if not the .y value will be kept - can be NA as well)
datamart$Gender <- mapply(function(gen.x, gen.y) ifelse(is.na(gen.x) == TRUE, gen.y, gen.x),
                                    datamart$Gender.x, datamart$Gender.y)
datamart$Gender.x <- datamart$Gender.y <- NULL

datamart$Language <- mapply(function(lan.x, lan.y) ifelse(is.na(lan.x) == TRUE, lan.y, lan.x),
                          datamart$Language.x, datamart$Language.y)
datamart$Language.x <- datamart$Language.y <- NULL

datamart$Country <- mapply(function(con.x, con.y) ifelse(is.na(con.x) == TRUE, con.y, con.x),
                          datamart$Country.x, datamart$Country.y)
datamart$Country.x <- datamart$Country.y <- NULL

# Country Codes
# CSV document is table from Appendix2 "Codebook_for_Actual_Internet_Sports_Gambling_Activity_from_February_2005_through_September_2005"
c_codes <- read_csv(paste0(path, "c_codes.csv"),col_names=FALSE)

codes <- c_codes[seq(1,nrow(c_codes),2),]
countries <- c_codes[seq(2,nrow(c_codes),2),]

country_tbl <- data.frame(as.numeric(codes$X1), countries)
names(country_tbl) <- c("country_code", "country_name")

# Merge Country Code in datamart
datamart <- datamart %>%
  left_join(country_tbl, by = c("Country" = "country_code"))

# Merge geodat (latitude & longitude data) in datamart
geodat <- read.delim2(paste0(path, "geodat.txt"), sep="\t", dec=".", skip=1)

datamart <- datamart %>%
  left_join(geodat, by = c("country_name" = "name"))

# lat and long to numeric
data_test$latitude <- as.numeric(data_test$latitude)
data_test$longitude <- as.numeric(data_test$longitude)

# Join continent data in datamart (based on country code)
url <- "https://pkgstore.datahub.io/JohnSnowLabs/country-and-continent-codes-list/country-and-continent-codes-list-csv_csv/data/b7876b7f496677669644f3d1069d3121/country-and-continent-codes-list-csv_csv.csv"
continents <- read_csv(url)

#Remove columns not needed from continents
continents$Three_Letter_Country_Code <- continents$Country_Number <- continents$Country_Name <- NULL

#Keep first occurence of Two Letter Country Code (no duplicates)
continents <- continents[!duplicated(continents$Two_Letter_Country_Code),]

datamart <- datamart %>%
  left_join(continents, by = c("country" = "Two_Letter_Country_Code"))

# Replace Application ID in Datamart
datamart <-mutate(datamart, 
                  application_description=case_when(ApplicationID==1 ~ 'BETANDWIN.COM',
                                                    ApplicationID==2 ~ 'TRIPLE-A-CASINO.COM',
                                                    ApplicationID==3 ~ 'BETANDWIN.DE',
                                                    ApplicationID==4 ~ 'WAP.BETANDWIN.COM',
                                                    ApplicationID==5 ~ 'SMS MOBILE BETTING APPLICATION',
                                                    ApplicationID==6 ~ 'CHARITY',
                                                    ApplicationID==7 ~ 'DOLCEVITACASINO.COM',
                                                    ApplicationID==8 ~ 'BALLS OF FIRE',
                                                    ApplicationID==9 ~ 'BETEUROPE.COM',
                                                    ApplicationID==10 ~ 'BAHSEGIR.COM',
                                                    ApplicationID==11 ~ 'CASINO.BETEUROPE.COM',
                                                    ApplicationID==12 ~ 'WWW.CASINOTURK.COM',
                                                    ApplicationID==13 ~ 'WWW.SANALCASINO.COM',
                                                    ApplicationID==14 ~ 'BETOTO.COM',
                                                    ApplicationID==15 ~ 'PLAYIT.COM',
                                                    ApplicationID==16 ~ 'CASINO.PLAYIT.COM',
                                                    ApplicationID==17 ~ 'THECROUPIER.COM',
                                                    ApplicationID==18 ~ 'SMS.BETANDWIN.COM',
                                                    ApplicationID==19 ~ 'WAP.BETANDWIN.DE',
                                                    ApplicationID==21 ~ 'BOF.PLAYIT.COM',
                                                    ApplicationID==22 ~ 'BOF.BETEUROPE.COM',
                                                    ApplicationID==23 ~ 'BETANDWIN POKER',
                                                    ApplicationID==24 ~ 'BETANDWIN CASINO',
                                                    ApplicationID==26 ~ 'SMS.BETANDWIN.DE',
                                                    ApplicationID==27 ~ 'LOTTERY.BETOTO.COM',
                                                    ApplicationID==28 ~ 'BETWORK.COM',
                                                    ApplicationID==29 ~ 'WAP.PLAYIT.COM',
                                                    ApplicationID==30 ~ 'PLAYIT POKER',
                                                    ApplicationID==31 ~ 'BETEUROPE POKER',
                                                    ApplicationID==32 ~ 'BETOTO POKER',
                                                    ApplicationID==33 ~ 'BETANDWIN GAMES',
                                                    ApplicationID==36 ~ 'BETOTO CASINO',
                                                    ApplicationID==38 ~ 'BETEUROPE GAMES',
                                                    ApplicationID==42 ~ 'PLAYIT GAMES'))
#Add Total Days Active for both LA and FO
x <- datamart
x$FOLastActiveDate <- ifelse(is.na(x$LATotalDaysActive) == TRUE, 0, x$LATotalDaysActive)
x$LALastActiveDate <- ifelse(is.na(x$FOTotalDaysActive) == TRUE, 0, x$FOTotalDaysActive)
datamart$TotalDaysActive <- x$LATotalDaysActive + x$FOTotalDaysActive

#Add column for last date the User was active in both FO and LA
x <- datamart
x$FOLastActiveDate <- ifelse(is.na(x$FOLastActiveDate) == TRUE, 0, x$FOLastActiveDate)
x$LALastActiveDate <- ifelse(is.na(x$LALastActiveDate) == TRUE, 0, x$LALastActiveDate)

datamart$LastActiveDate <- ifelse(x$FOLastActiveDate > x$LALastActiveDate, x$FOLastActiveDate, x$LALastActiveDate) 
datamart$LastActiveDate <- as_date(datamart$LastActiveDate)

x <- NULL 

#Create Gender Description based on dummy encoded Gender
datamart <-mutate(datamart, 
                  gender_description=case_when(Gender==0 ~ 'Female',
                                               Gender==1 ~ 'Male'))

# Validate Datamart
dim(datamart)
length(unique(datamart$UserID))

View(datamart)

# Join Country_name in Info Table (for Analysis)
data_country <- datamart %>%
  select(UserID, country_name)

informative_table <- informative_table %>%
  left_join(data_country, by = c("UserID" = "UserID"))

# save to csv
write.csv(datamart,paste0(path, "datamart.csv"), row.names = FALSE)
write.csv(datamart,paste0(path, "/apptest/datamart.csv"), row.names = FALSE)
write.csv(informative_table,paste0(path, "informative_table.csv"), row.names = FALSE)
write.csv(datamart,paste0(path, "/apptest/informative_table.csv"), row.names = FALSE)

