## Homework 9 - 28 Aug 2018
#------------------------------------
#Step 1 - load the data
#------------------------------------
#aq is the temporary data frame
aq <- airquality

#which columns had null values
library(matrixStats)
#number of nulls in
colSums(is.na(aq))

#Replacing ozone with the median ozone
aq[is.na(aq$Ozone), "Ozone"] <- median(aq$Ozone, na.rm = T)
#Replacing Solar.R with median soalr.r
aq[is.na(aq$Solar.R), "Solar.R"] <- median(aq$Solar.R, na.rm = T)
aq <- aq[,-5:-6]


#Step 2 - create training and test set
#------------------------------------

#Creating a vector or random number from 1 to number of rows in aq
#and putting them in a random order
randIndex <- sample(1:dim(aq)[1])
#getting the index that is at the 2/3 point of the number of rows
cutOff <- floor(2 * dim(aq)[1] / 3)
#using the first 2/3 of the random index to randomly select 2/3 of data to train
aq.train <- aq[randIndex[1:cutOff],]
#Randomly selecting the remaining 1/3 of data as the test set
aq.test <- aq[randIndex[(1 + cutOff):nrow(aq)],]

#Step 3 - create a model to predict ozone
#------------------------------------
library(kernlab)
#Creating a model predicting the Ozone using the solar, wind, and temp
#from the training dataset
model.1 <- ksvm(Ozone ~ ., data = aq.train,
                kernel = "rbfdot", kpar = "automatic", cross = 3,
                prob.model = T)

#Creating a function to calculate the rmse that takes actual and predicted values
rmse <- function(actual, predicted){
  #squres the difference between actual and predicted
  se <- (actual - predicted)^2
  #Sums the square error
  se <- sum(se)
  #gets the number of instances
  n <- length(actual)
  #Divides the summed error by the instnaces and takes the square root
  rmse <- sqrt(se / n)
  return(rmse)
}

#Predict the ozone using the test data set
model.1.pred <- predict(model.1, aq.test, type = "votes")
#What is the rmse of the model
model.1.rmse <- rmse(aq.test$Ozone, model.1.pred[,1])
#Show rmse
cat("The RMSE for RSVM Model is: ", model.1.rmse)

#Putting the predicted value inside the test dataset
aq.test$Predicted.Model1 <- model.1.pred[,1]

#Plotting temp as x-axis and wind as y-axis and using the absolute value of the error as size and color
p1 <- ggplot(aq.test, aes(Temp, Wind, size = abs(Ozone - Predicted.Model1), color = abs(Ozone - Predicted.Model1)))
#adding points to the model and making the scale go from blue to red
p1 <- p1 + geom_point() + scale_color_gradient(low = "blue", high = "red", name = "Absolute Error")
#Changing the name of the size legend
p1 <- p1 + scale_size_continuous(name = "Absolute Error") +ggtitle("RSVM Error")
#Showing plot
p1

library(e1071)

#Using svm function to create model.2
model.2 <- svm(Ozone ~ ., data = aq.train)
#Predicting the ozone using the test set
model.2.pred <- predict(model.2, aq.test, type = "votes")
#Calculating rmse with the function I created
model.2.rmse <- rmse(aq.test$Ozone, model.2.pred)
#Displaying RMSE
cat("The RMSE for SVM model is: ", model.2.rmse)

#adding the predicted value of the ozone to the test dataset
aq.test$Predicted.Model2 <- model.2.pred

#Plotting temp as x-axis and wind as y-axis and using the absolute value of the error as size and color
p2 <- ggplot(aq.test, aes(Temp, Wind, size = abs(Ozone - Predicted.Model2), color = abs(Ozone - Predicted.Model2)))
#adding points to the model and making the scale go from blue to red
p2 <- p2 + geom_point() + scale_color_gradient(low = "blue", high = "red", name = "Absolute Error")
#Changing the name of the size legend
p2 <- p2 + scale_size_continuous(name = "Absolute Error") + ggtitle("SVM Error")
#Showing plot
p2


#Linear Modeling
model.3 <- lm(Ozone ~ ., data = aq.train)
#Predicting the ozone using the test set
model.3.pred <- predict(model.3, aq.test)
#Calculating rmse with the function I created
model.3.rmse <- rmse(aq.test$Ozone, model.3.pred)
#Displaying RMSE
cat("The RMSE for the linear modeling is: ", model.3.rmse)


#adding the predicted value of the ozone to the test dataset
aq.test$Predicted.Model3 <- model.3.pred


#Plotting temp as x-axis and wind as y-axis and using the absolute value of the error as size and color
p3 <- ggplot(aq.test, aes(Temp, Wind, size = abs(Ozone - Predicted.Model3), color = abs(Ozone - Predicted.Model3)))
#adding points to the model and making the scale go from blue to red
p3 <- p3 + geom_point() + scale_color_gradient(low = "blue", high = "red", name = "Absolute Error")
#Changing the name of the size legend
p3 <- p3 + scale_size_continuous(name = "Absolute Error") + ggtitle("Linear Modeling Error")
#Showing plot
p3

library(gridExtra)
library(grid)

#Using grid.arrange to show the differences in error
grid.arrange(p1 + theme(legend.position = "none"), p2 + 
               theme(legend.position = "none"), p3 + theme(legend.position = "none"))


##Step	4:	Create	a	‘goodOzone’	variable
#------------------------------------

#Adding good ozone to the training set
aq.train$goodOzone <- ifelse(aq.train$Ozone >= mean(aq$Ozone), 1, 0)
aq.train$goodOzone <- as.factor(aq.train$goodOzone)
#Adding good ozone to the test set
aq.test$goodOzone <- ifelse(aq.test$Ozone >= mean(aq$Ozone), 1, 0)
aq.test$goodOzone <- as.factor(aq.test$goodOzone)

#Step	5:	See	if	we	can	do	a	better	job	predicting	‘good’	and	‘bad’	days
#------------------------------------

#Creating a ksvm categorical predictor 
model.ksvm.cat <- ksvm(goodOzone ~ Solar.R + Temp + Wind, data = aq.train, kernel = "rbfdot",
      cross = 3, kpar = "automatic", prob.model = T)

#Getting predictions
model.ksvm.cat.pred <- predict(model.ksvm.cat, aq.test, type = "votes")
#Row 2 is a 1 if it is above the average ozone
compFrame <- data.frame(actual = aq.test$goodOzone, predicted = model.ksvm.cat.pred[2,])
compTable <- table(compFrame)
#Showing confusion matrix
compTable

#Percent of correctly good ozone days
percent <- compTable[2,2] / (compTable[1,2] + compTable[2,2]) * 100
cat("The percentage that the ozone was good and correctly predicted healthy was ", percent, "%", sep = "")

#Giving the prediction to the test frame
aq.test$Predicted.KSVM.good <- model.ksvm.cat.pred[2,]

#Plotting with x as temp and y as wind and color goodozone being the factor
pb1 <- ggplot(aq.test, aes(x = Temp, y = Wind, color = as.factor(goodOzone)))
#The size of the point depends on whether the prediction was not equal
pb1 <- pb1 + geom_point(aes(size = (goodOzone != Predicted.KSVM.good)))
#Adding a title to the legend and changing labels
pb1 <- pb1 + scale_color_discrete(name = "Ozone Status", labels = c("Good", "Bad"))
#Adding title to the legend and changing labels
pb1 <- pb1 + scale_size_discrete(name = "Prediction Result", labels = c("Correct", "Incorrect"))
pb1 <- pb1 + ggtitle("KSVM Classification Plot")
pb1

#Creating a ksvm categorical predictor 
model.svm.cat <- svm(goodOzone ~ Solar.R + Temp + Wind, data = aq.train)

#Getting predictions
model.svm.cat.pred <- predict(model.svm.cat, aq.test, type = "votes")

#Giving the prediction to the test frame
aq.test$Predicted.SVM.good <- model.svm.cat.pred

#Plotting with x as temp and y as wind and color goodozone being the factor
pb2 <- ggplot(aq.test, aes(x = Temp, y = Wind, color = as.factor(goodOzone)))
#The size of the point depends on whether the prediction was not equal
pb2 <- pb2 + geom_point(aes(size = (goodOzone != Predicted.SVM.good)))
#Adding a title to the legend and changing labels
pb2 <- pb2 + scale_color_discrete(name = "Ozone Status", labels = c("Good", "Bad"))
#Adding title to the legend and changing labels
pb2 <- pb2 + scale_size_discrete(name = "Prediction Result", labels = c("Correct", "Incorrect"))
pb2 <- pb2 + ggtitle("SVM Classification Plot")
pb2


#Creating a nb categorical predictor 
model.nb.cat <- naiveBayes(goodOzone ~ Solar.R + Temp + Wind, data = aq.train)

#Getting predictions
model.nb.cat.pred <- predict(model.nb.cat, aq.test)

#Giving the prediction to the test frame
aq.test$Predicted.nb.good <- model.nb.cat.pred

#Plotting with x as temp and y as wind and color goodozone being the factor
pb3 <- ggplot(aq.test, aes(x = Temp, y = Wind, color = as.factor(goodOzone)))
#The size of the point depends on whether the prediction was not equal
pb3 <- pb3 + geom_point(aes(size = (goodOzone != Predicted.nb.good)))
#Adding a title to the legend and changing labels
pb3 <- pb3 + scale_color_discrete(name = "Ozone Status", labels = c("Good", "Bad"))
#Adding title to the legend and changing labels
pb3 <- pb3 + scale_size_discrete(name = "Prediction Result", labels = c("Correct", "Incorrect"))
pb3 <- pb3 + ggtitle("Naive Bayes Classification Plot")
pb3

grid.arrange(pb1 + theme(legend.position = "none"), pb2 + 
               theme(legend.position = "none"), pb3 + theme(legend.position = "none"))

