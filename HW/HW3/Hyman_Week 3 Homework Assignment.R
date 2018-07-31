## Homework #3 - 7 Aug 2018

#Step 1 Create a function (named readStates) to read a CSV file into R
#--------------------------

#creating a default for the url argument
readStates <- function(url = 
        "http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"){
    # Reading the csv and preventing the strings to be read as factors
    df <- read.csv(url(url), stringsAsFactors = F)
    
    #Step 2 Clean the dataframe
    #--------------------------
    
    #deleting the first 8 rows and the last 5 columns
    df <- df[-1:-8, -6:-10]
    #deleting the last 7 rows
    df <- df[-52:-58,]
    #Creating a vector of column names
    colNames <- c("stateName", "Jul2010", "Jul2011", "base2010", "base2011")
    #Assigning the column names to the data frame
    colnames(df) <- colNames
    #resetting the row numbers
    rownames(df) <- NULL
    #Getting rid of the period before all the state names
    df$stateName <- gsub("\\.", "", df$stateName)
    #Writing a for loop to go through each of the columns 2 through 5
    for (col in 2:5){
        #Replacing the comma with nothing
        df[,col] <- gsub(",", "", df[,col])
        #replacing any spaces with nothing
        df[,col] <- gsub(" ", "", df[,col])
        #Converting the strings to numbers
        df[,col] <- as.numeric(df[,col])
    }
    # returning the data frame
    return(df)
}


#Step 3 Stor and explore
#--------------------------

#Assigning the data frame to a variable dfStates
dfStates <- readStates()

#Testing to see if the mean for July2011 is 6,053,834
#Saving as the variable pop.avg
(pop.avg <- mean(dfStates$Jul2011))


#Step 4 Find the state name with the highest population
#--------------------------
#State with the highest population
 (dfStates[which.max(dfStates$Jul2011), "stateName"])

#Sorting the data based on July2011 in increasing order
(dfStates.sorted <- dfStates[order(dfStates$Jul2011),])


#Step 5 Explore the distrubution of the states
#--------------------------
# Write a function that takes two parameters. The first is a vector and the second is a number.

#Defining the function
myFunction <- function(vec, num){
    #getting the number of points that are less than the given number
    numLess <- sum(vec < num)
    #retunring the number of points less than the number 
    return(numLess / length(vec))
}

#testing the function with the play data
vec <- c(1,2,3,4,5)
num <- 2
(myFunction(vec, 2))

#exploring percentage of states with a population below the mean
myFunction(dfStates$Jul2011, pop.avg)
# 2/3s of the states are below the mean