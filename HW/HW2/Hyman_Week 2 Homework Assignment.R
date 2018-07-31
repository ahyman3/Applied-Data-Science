## Homework 2 - 20 Jul 2018

#-------------------Introduction----------------------

# copying the original dataset
myCars <- mtcars

#-----------------------------------------------------
# Step 1: What is the hp (hp stands for “horse power”) 

# Q1) What is the highest hp?
(maxHp <- max(myCars$hp))

# Q2) Which car has the highest HP?
(maxHp.car <- rownames(myCars[myCars$hp == maxHp, ]))

#-----------------------------------------------------
#Step 2: Explore mpg (mpg stands for “miles per gallon”)

# Q3) What is the highest mpg?
(maxMpg <- max(myCars$mpg))

# Q4) What car has the highest mpg?
(maxMpg.car <- rownames(myCars[myCars$mpg == maxMpg,]))

# Q5) Create a sorted dataframe, based on mpg 
(mpgCars <- myCars[order(-myCars$mpg),])

#-----------------------------------------------------
# Step 3: Which car has the “best” combination of mpg and hp?

# Q6) What logic did you use?
# I ranked both of the mpg and the hp and took with the worst car getting a 1 and the best car
#getting a 32

# Q7) Which cars?
# Getting the max score from the number of rows
bestScore <- dim(myCars)[1]
# Setting up scoring from 1 to max score
scores <- 1:bestScore
# Combining myCars ordered by mpg with the rankings from 1 to 32
myCars <- cbind(myCars[order(myCars$mpg), ], mpgScore = scores)
# Combining myCars ordered by hp with the rankings from 1 to 32
myCars <- cbind(myCars[order(myCars$hp), ], hpScore = scores)
# Summing the rankings of the two cars
myCars$finalScore <- myCars$mpgScore + myCars$hpScore
# Selecting the first score returned by the descending order function and getting the rowname
# This returns the best car by this scoring method
(bestCar <- rownames(myCars[order(-myCars$finalScore)[1], ]))
# Returns Lotus Europa

#-----------------------------------------------------
# Step 4: Which car has “best” car combination of mpg and 
# hp, where mpg and hp must be given equal weight?

# Going to standardize the mpg and the hp so there is a mean of 0 and a sd of 1
# So will 
# First creating a function to feature scale
standardize <- function(feature){
    #getting the mean
    xbar <- mean(feature)
    # getting the standard deviation
    xsd <- sd(feature)
    #returning the feature subtracted by the minimum and divided by the max - min
    return((feature - xbar) / xsd)
}
# Feature scaling mpg
myCars$mpgStd <- standardize(myCars$mpg)
# Feature Scaling hp
myCars$hpStd <- standardize(myCars$hp)
# Adding the new hp and mpg together
myCars$finalStdScore <- myCars$mpgStd + myCars$hpStd
# Getting the best finalScore rowname (aka the car) from descending order funtion
(bestCar <- rownames(myCars[order(-myCars$finalStdScore)[1], ]))
# returns Maserati Bora

