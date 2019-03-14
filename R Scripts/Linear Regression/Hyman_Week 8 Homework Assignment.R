## Homework 8 - 23 Aug 2018
#------------------------------------
#Step 1 - Read in the data
#------------------------------------

library(gdata)
url = 'http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/excel/mlr01.xls'
df <- read.xls(url)
col.names <- c("numFawn", "popAnt", "precip", "winter")
colnames(df) <- col.names

#Step 3 - Check the structure of the data
#-----------------------------------

str(df)
#eight observations of four variables
#Columns are numeric

#Step 4: Plot baby fawn vs independent variables
#-----------------------------------
library(ggplot2)

gPop <- ggplot(df, aes(popAnt, numFawn))
gPop <- gPop + geom_point()
gPop <- gPop + ylab("Number of Fawn") + xlab("Adult Antelope Population")
gPop <- gPop + ggtitle("Adult Population vs. Fawn Born")
gPop <- gPop + theme(plot.title = element_text(hjust = 0.5))
gPop

gRain <- ggplot(df, aes(precip, numFawn))
gRain <- gRain + geom_point()
gRain <- gRain + ylab("Number of Fawn") + xlab("Annual Precipitation")
gRain <- gRain + ggtitle("Annual Precipitation vs. Fawn Born")
gRain <- gRain + theme(plot.title = element_text(hjust = 0.5))
gRain

#Plot
gWinter <- ggplot(df, aes(winter, numFawn))
gWinter <- gWinter + geom_point()
gWinter <- gWinter + ylab("Number of Fawn") + xlab("Harshness of Winter")
gWinter <- gWinter + ggtitle("Harshness of Winter vs. Fawn Born")
gWinter <- gWinter + theme(plot.title = element_text(hjust = 0.5))
gWinter

#Step 5: Regression Models

#winter vs fawn model
model1 <- lm(numFawn ~ winter, data = df)

#winter and adult population vs fawn
model2 <- lm(numFawn ~ winter + popAnt, data = df)

#winter, adult population, and precipitation vs fawn
model3 <- lm(numFawn ~ winter + precip + popAnt, data = df)

#Looking at the model summaries
#Model 1
summary(model1)
#adjusted R2 of 0.47

#Model 2
summary(model2)
#adjusted R2 of 0.84

#Model 3
summary(model3)
#adjusted R2 of 0.96

#Which model works best?

#Model 3 is the best model. All variables are significant 
#and it has the highest R2 value of 0.955. Meaning that
#the change in fawn born is 95.5% due to the variability
#in the winter, adult population, and precipitation

#Which predictors are statistically significant in each of the models?

#With an alpha of 0.05, the winter variable is significant. In model 2, 
#only the population of adult antelope is significant. In model 3, all 
#of the variables (num antelope, precipitation, and winter) are statistically
#significant

#If you wanted to create the most parsimonious model (i.e., the one that 
#did the best job with the fewest predictors), what would it contain?

#To create the best linear model with the fewest parameters, I would use
#Precipitation as the only parameter. The adult population had teh lowest p-value,
# but it looked like it had a non-linear (exponential) correlation, and therefore 
#would not be best for a linear model without some sort of data transformation
