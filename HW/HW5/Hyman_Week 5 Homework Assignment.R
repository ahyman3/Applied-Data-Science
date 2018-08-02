#Homework 5 - 2 Aug 2018

#Step 1: Load the data
#-------------------------------------------------------------------------
#Read in the following JSON dataset
#Library for http get requests
library(RCurl)
#Library for json parsing
library(jsonlite)
#HTTP get request
urlObj <- getURL("http://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD")
#Converting the url string to json
jsonObj <- fromJSON(urlObj)
#Getting the data from the JSON list
data <- jsonObj$data
#Converting the list into the data frame
df <- data.frame(data, stringsAsFactors = F)
#Viewing the structure of the data frame, making sure all looks good
str(df)

#Step 2: Clean the data
#-------------------------------------------------------------------------
#deleting the first 8 columns
df <- df[,-1:-8]
#names of the columns
namesOfColumns <- c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME","ACC_TIME_CODE",
    "DAY_OF_WEEK","ROAD","INTERSECT_ROAD","DIST_FROM_INTERSECT","DIST_DIRECTION",
    "CITY_NA ME","COUNTY_CODE","COUNTY_NAME","VEHICLE_COUNT","PROP_DEST","INJURY",
    "COLLISION_WITH_1","COLLISION_WITH_2")
#Assigning the names of teh coulmns to the data frame
colnames(df) <- namesOfColumns
#making sure the column headers make sense
head(df)
#Deleting the space at the end of the day of the week
df$DAY_OF_WEEK <- gsub(" ", "", df$DAY_OF_WEEK)
#Converting the distance from intersection into numeric format
df$DIST_FROM_INTERSECT <- as.numeric(df$DIST_FROM_INTERSECT)
#Converting vehicle count into numeric
df$VEHICLE_COUNT <- as.numeric(df$VEHICLE_COUNT)
#Making day of week a factor
df$DAY_OF_WEEK <- as.factor(df$DAY_OF_WEEK)

#Step 3: Understand the data using SQL (via SQLDF)
#-------------------------------------------------------------------------
#Loading library fir sql commands
library(sqldf)
#How many accidents happen on SUNDAY
sqldf("SELECT count(CASE_NUMBER) AS 'Sunday Crashes' FROM df WHERE df.DAY_OF_WEEK = 'SUNDAY'")
#How many accidents had injuries
sqldf("SELECT count(CASE_NUMBER) AS 'Accidents with Injuries' FROM df WHERE INJURY = 'YES'")
# INJURIES by day
#Creating the select statement with paste
selectStr = paste("SELECT DAY_OF_WEEK, count(CASE_NUMBER) AS Injuries FROM df WHERE INJURY = 'YES'",
  "GROUP BY DAY_OF_WEEK ORDER BY Injuries DESC")
#Selecting the number of accidents with injuries by day
sqldf(selectStr)

#Step 4: Understand the data using tapply
#-------------------------------------------------------------------------
#How many accidents happen on SUNDAY
#True are accidents on Sunday
tapply(df$CASE_NUMBER, df$DAY_OF_WEEK == "SUNDAY", length)
#How many accidents had injuries
#True had accidents
tapply(df$CASE_NUMBER, df$INJURY == "YES", length)
#Injuries by day
tapply(df[df$INJURY == "YES",]$INJURY, df[df$INJURY =="YES",]$DAY_OF_WEEK, length)




