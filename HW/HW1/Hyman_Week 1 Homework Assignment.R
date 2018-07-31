## Homework 1 - 19 Jul 2018

#--------------Intro--------------
# Defining height, weight, and a
height <- c(59,60,61,58,67,72,70)
weight <- c(150,140,180,220,160,140,130) 
a <- 150 

#---------------------------------
# Step 1: Calculating means

# Q1)  Compute, using R, the average height (called mean in R)
mean(height)
# Q2) Compute, using R, the average weight (called mean in R) 
mean(weight)
# Q3)  Calculate the length of the vector ‘height’ and ‘weight’ 
length(height)
length(weight)
# Q4)  Calculate the sum of the heights
sum(height)
# Q5)  Compute the average of both height and weight, by dividing the sum 
# (of the height or the width, as appropriate), by the length of the vector. 
# How does this compare to the ‘mean’ function?
sum(height) / length(height)
sum(weight) / length(weight)
# The values are the same!

#---------------------------------
# Step 2: Using max/min functions 

# Q6) Compute the max height, store the result in ‘maxH’ 
maxH <- max(height)
# Q7) Compute the min weight, store the results in ‘minW’ 
minW <- min(weight)

#---------------------------------
# Step 3: Vector Math

# Q8)  Create a new vector, which is the weight + 5 (every person gained 5 pounds)
newWeight <- weight + 5
# Q9) Compute the weight/height for each person, using the new weight just created
newWeight / height

#---------------------------------
# Step 4: Using Conditional if statements
# Q10) Write the R code to test if max height is greater than 60 (output “yes” or “no”)
if(maxH > 60) print("yes") else print("no")
# Q11) Write the R code to if min weight is greater than the variable ‘a’ (output “yes” or “no”)
if(minW > a) print("yes") else print("no")



