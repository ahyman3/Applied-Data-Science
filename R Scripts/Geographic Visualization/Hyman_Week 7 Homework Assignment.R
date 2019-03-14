## Homework 7 - 21 Aug 2018
#------------------------------------
# 1. Read the Data

#Loading lobrary
library(gdata)
library(readxl)
#Setting the working directory
setwd("/Users/alexhyman/Documents/Syracuse/Current Classes/Applied Data Science/HW/HW7/")
#Reading the excel file
df <- read_xlsx("MedianZIP_2_2.xlsx")

# 2. Clean the data frame 
#The colnames are in the first row
col.names <- df[1,]
#Deleting the first row
df <- df[-1,]
#Assigning the columns names
colnames(df) <- col.names

library(stringr)
#Replacing all the commas in median
df$Median <- str_replace_all(df$Median, ",", "")
#Converting median column to numeric
df$Median <- as.numeric(df$Median)
#Replacing all the commas in mean
df$Mean <- str_replace_all(df$Mean, ",", "")
#Converting mean column to numeric
df$Mean <- as.numeric(df$Mean)
df[is.na(df$Mean), "Mean"] <- df[is.na(df$Mean), "Median"]
#Replacing all the commas in pop
df$Pop <- str_replace_all(df$Pop, ",", "")
#Converting population column to numeric
df$Pop <- as.numeric(df$Pop)


# 3. Load the zipcode package
library(zipcode)

df$Zip <- clean.zipcodes(df$Zip)
# 4. Merge the zipcode information
data(zipcode)
#merging the two data frames
zipdf <- merge(zipcode, df, by.x = "zip", by.y = "Zip")

# 5. Remove Alaska and Hawaii
#Subsetting lower 48 states
zipdf <- subset(zipdf, state != "AK" & state != "HI" & state != "DC")

## Step 2: Show income and population per state
#-----------------------------------------------

# 1. Create a simple data frame with teh avg median income and the populations for each state

#Making the states the factors
zipdf$state <- as.factor(zipdf$state)

#Average median income per state
incomedf <- tapply(zipdf$Median, zipdf$state, mean)
#Summation of the populations
popdf <- tapply(zipdf$Pop, zipdf$state, sum)
#combing the apply's to create a single data frame
simple.df <- data.frame(income = incomedf, pop = popdf)

# 2. Add state names and state abbreviations as columns

#creating a column for the statename
simple.df$state.abb <- rownames(simple.df)
#reindexing the data frame
rownames(simple.df) <- NULL
#Creating a dataframe for the states
states <- data.frame(state.name, state.abb)
#Merging the data frame on the abbreviations
simple.df <- merge(simple.df, states, by = "state.abb")
#Making the state name lowercase
simple.df$state <- tolower(simple.df$state.name)

# 3. Median income for each of the states
#for plotting
library(ggplot2)
#for mapping
library(ggmap)
#loading the map data
us <- map_data("state")
#Creating the ggplot object witht eh data frame and using the state column
incomeMap <- ggplot(simple.df, aes(map_id = state))
#Creatign the map filling with the income
incomeMap <- incomeMap + geom_map(map = us, aes(fill = income))
#Limiting the scale of the map
incomeMap <- incomeMap + expand_limits(x = us$long, y = us$lat)
#Fixing the coordinates and adding the title
incomeMap <- incomeMap + coord_map() + ggtitle("Average Median Income by State")
#Showing the map
incomeMap

# 4. Create a map for the population
#Creating ggplot object using the state name as the mapping id
popMap <- ggplot(simple.df, aes(map_id = state))
#adding the map and filling it by the population
popMap <- popMap + geom_map(map = us, aes(fill = pop))
#Limiting the scale of the map
popMap <- popMap + expand_limits(x = us$long, y = us$lat)
#Fixing the coordinates and adding the title
popMap <- popMap + coord_map() + ggtitle("Population by State")
#Showing the map
popMap

## Step 3: Show the income by zipcode
#-------------------------------------------
#making the abbreviation column
zipdf$state.abb <- zipdf$state
#deleting the state column
zipdf <- zipdf[,-3]
#merging the zipdf with states so we can have the state names
zipdf <- merge(zipdf, states, by = "state.abb")
#Making the state column the lower case of the state names
zipdf$state <- tolower(zipdf$state.name)

#Creating the ggplot object
zipMap <- ggplot(zipdf, aes(map_id = state))
#adding the map object to the ggplot
zipMap <- zipMap + geom_map(map = us, fill = "black", "color" = "white")
#using zipcode lat and long in the zipdf to make points and using the 
#median as the color
zipMap <- zipMap + geom_point(aes(x = longitude, y = latitude, color = Median))
#Limiting the x and y  to be just the lower 48
zipMap <- zipMap + expand_limits(x = us$long, y = us$lat)
#Making the coordinates fit and adding a title
zipMap <- zipMap + coord_map() + ggtitle("Median Income by Zip Code")
#Showing the zip code income map
zipMap

## Step 4: Zip Code density
#--------------------------------------------------------------------------
#Create ggplot object
zipDensity <- ggplot(zipdf, aes(map_id = state))
#Add the map data
zipDensity <- zipDensity + geom_map(map = us, fill = "black", color = "white")
#use the stat_density2d to show density of zipcodes
zipDensity <- zipDensity + stat_density2d(aes(x = longitude, y = latitude))
#Limiting the graph to the lower 48
zipDensity <- zipDensity + expand_limits(x = us$long, y = us$lat)
#Fixing the projection, and adding a title
zipDensity <- zipDensity + coord_map() + ggtitle("Density of Zip Codes in United States")
#Showing the density map
zipDensity

## Step 5: Zoom to NY Region
#Geocode for Central NY
ny <- geocode("Albany, NY")
#Getign the x center
centerx <- ny$lon
#getting the y center 
centery <- ny$lat
#Range of 8 degrees around center
zoomAmount <- 5
#Limits for the y
ylimit <- c(centery - zoomAmount, centery + zoomAmount)
#Limits for the x
xlimit <- c(centerx - zoomAmount, centerx + zoomAmount)
#Creating a generalized ny map
nyMap <- ggplot(zipdf, aes(map_id = state))
#adding the map data
nyMap <- nyMap + geom_map(map = us, fill = "black", color = "white")
#Zooming the map around the center
nyMap <- nyMap + expand_limits(x = xlimit, y = ylimit)
#Fixing the coordinates for the map
nyMap <- nyMap + coord_map()

#Creating df for zipcodes between the y limit
zipZoom <- zipdf[zipdf$latitude > ylimit[1] & zipdf$latitude < ylimit[2],]
#Filtering the df to only zipcodes between teh x limit
zipZoom <- zipZoom[zipZoom$longitude > xlimit[1] & zipZoom$longitude < xlimit[2],]
#Creating the zipcode density map for the northeast with the filtered data
nyZip <- nyMap + stat_density2d(data = zipZoom, aes(x = longitude, y = latitude))
#Adding a title to the plot
nyZip <- nyZip + ggtitle("Density of Zip Codes in the Northeast")
#Showing the zip code density plot
nyZip

#Using the filtered data and the ny map to add points for the zipcode
nyIncome <- nyMap + geom_point(data = zipZoom, aes(x = longitude, y = latitude, color = Median))
#Adding a title to the norhteast income map
nyIncome <- nyIncome + ggtitle("Median Income for Zip Code Northeast")
#Showing the income map for ny
nyIncome






