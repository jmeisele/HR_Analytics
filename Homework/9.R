library(e1071)
library(grid)
library(gridExtra)
library(lattice)
library(ggplot2)
#Import airquality dataset as a dataframe
df <- airquality
dim(df)

# For loop to fill NA values with mean
for(i in 1:ncol(df)){
  df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
}

#Command to check for NA values
is.na(df)

#Drop Month and Day columns from df.test
df$Month <- NULL
df$Day <- NULL

#Split dataset into training and test
nrows = nrow(df)
cutPoint <- floor(nrows/3*2)
rand <- sample(1:nrows)
head(rand)
df.train <- df[rand[1:cutPoint],]
df.test <- df[rand[(cutPoint+1):nrows],]
str(df.train)
df.train
str(df.test)
df.test

#Install kernlab pacakage
library(kernlab)

#Instantiate a model class
ksvm_model <- ksvm(Ozone ~Solar.R + Wind + Temp, data = df.train,kernel="rbfdot",kpar="automatic",C=50,cross=3,prob.model=TRUE)
ksvm_model
df.test$pred <- predict(ksvm_model, df.test)
df.test$error <- df.test$Ozone - df.test$pred

#Calculate the Root Mean Sqaured Error
library(Metrics)
rmse(df.test$Ozone, df.test$pred)
#Plot the results using a scatter plot, X axis = Temeprature, y axis = wind, size = error
ksvm_plot <- ggplot(df.test, aes(x=Temp, y=Wind)) + geom_point(aes(size=df.test$error, color = df.test$error)) + ggtitle("ksvm_plot")

#Generate models and plot results for 'svm' (in the e1071 package)
library(e1071)
svm_model2 <- svm(Ozone ~Solar.R + Wind + Temp, data = df.train,kpar="automatic",C=50,cross=3,prob.model=TRUE)
df.test$pred2 <- predict(svm_model2, df.test)
df.test$error2 <- df.test$Ozone - df.test$pred2
#Calculate the rmse for 'svm' model of e1071 package
rmse(df.test$Ozone, df.test$pred2)
#Plot the results using a scatter plot, X axis = Temeprature, y axis = wind, size = error
svm_plot <- ggplot(df.test, aes(x=Temp, y=Wind)) + geom_point(aes(size=df.test$error2, color = df.test$error2)) + ggtitle('svm_plot')

# Generate a similar result using lm model
lm_model <- lm(formula = Ozone ~ Solar.R + Wind + Temp, data = df.train)
df.test$pred3 <- predict(lm_model, df.test)
df.test$error3 <- df.test$Ozone - df.test$pred3
summary(lm_model)
abline(lm_model)
#Plot the results use ggplot scatter plot, use gridarrange function
lm_plot <- ggplot(df.test, aes(x = Temp, y = Wind))+ geom_point() + stat_smooth(method = 'lm', col = "red") + ggtitle("lm_plot")

grid.arrange(ksvm_plot, svm_plot, lm_plot, nrow = 2)
