#Libraries used in this project
library(ggplot2)
library(lattice)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(ggpubr)
set.seed(123)

#Load the data and summarize
df <- read.csv("C:\\Users\\jmeis\\Desktop\\HR.csv")
summary(df)
head(df, 4)
str(df)

#Any Missing Values?
apply(df, 2, function(x) any(is.na(x)))

#Change binary values for 0 (Stayed) and 1 (Left)
df$left[df$left == 1] <- 'Left'
df$left[df$left == 0] <- 'Stayed'

#Change string data types we just created to factors
df$left <- as.factor(df$left)

#Lets do some initial EDA (Exploratory Data Analysis) to examine our data against our target variable "left"
##Histogram of Monthly Hours
hist_hours <- ggplot(df, aes(x=average_montly_hours, fill=left, color=left)) + 
  geom_histogram(position="identity", alpha=0.1) + ggtitle("Distribution of Average Monthly Hours")+ theme_bw() + labs(x = 'Monthly Hours', y = 'Frequency')
hist_hours
monthlyhours_mean <- mean(df$average_montly_hours)
monthlyhours_sd <- sd(df$average_montly_hours)

#Combined % bar chart for Number of Projects
bar_pro <- ggplot(df, aes(x = number_project, fill = left)) + geom_bar(position = 'fill') + 
  theme_bw() + labs(x = 'number of projects', y = 'Attrition %') + ggtitle("Attrition by Number of Projects")
bar_pro

# Violin plot of Seniority
violin_seniority <- ggplot(df, aes(x=left, y = time_spend_company, color=left)) + 
  geom_violin(scale = 'area') + theme_bw() + ggtitle("Attrition by Seniority") + labs(x = 'Attrition', y = 'Years with company')
violin_seniority

#Satisfaction Level Box Plot by Attrition
box_sat <- ggplot(df, aes(x = left, y = satisfaction_level, fill = left)) +geom_boxplot() + theme_bw() + 
  labs(x = 'Attrition', y = 'Satisfaction Level') + ggtitle("Attrtition by Satisfaction")
box_sat
satisfaction_mean <- mean(df$satisfaction_level)*100
satisfaction_sd <- sd(df$satisfaction_level)

#Attrition by Department
bar_dept <- ggplot(df, aes(x = Department, fill = left)) +geom_bar(position = 'fill') + 
  theme_bw() + labs(x = 'Department', y = 'Attrition %') + ggtitle("Attrition by Department")
bar_dept

# Reload our original dataset to include binary values for our target variable 'left'
df <- read.csv("C:\\Users\\jmeis\\Desktop\\HR.csv")

# Retransform factor data types to numeric 
df$Department <- as.numeric(df$Department)
df$salary <- as.numeric(df$salary)

#Split dataset into training and test
nrows = nrow(df)
cutPoint <- floor(nrows/3*2)
rand <- sample(1:nrows)
head(rand)
df.train <- df[rand[1:cutPoint],]
df.test <- df[rand[(cutPoint+1):nrows],]
str(df.train)
str(df.test)

#Instantiate, fit and predict using Logistic Regression Model or "Logit"
logit_model <- glm(left ~ satisfaction_level + last_evaluation + 
                     number_project + average_montly_hours + time_spend_company + 
                     Work_accident + promotion_last_5years + Department + salary, data = df.train, family = "binomial")
summary(logit_model)

# We see the features with very low p-values are satisfaction_level, number_projects, time_spend_company, Work_accident
# Let's regrenerate a logit model with only these features
logit_model <- glm(left ~ satisfaction_level + last_evaluation + number_project + 
                     + average_montly_hours + time_spend_company + 
                     Work_accident + promotion_last_5years, data = df.train, family = "binomial")
summary(logit_model)

#Create a column for predicted probabilities
df.test$logit_proba <- predict(logit_model, df.test, type = 'response')

#If predicted prbability if > 50% classify as 1~Left otherwise 0~Stayed
df.test$logit_pred <- ifelse(df.test$logit_proba >0.50, "1", "0")

#Convert predicted values to factor data type
df.test$logit_pred <- as.factor(df.test$logit_pred)

#Confusion Matrix for Logistic Regression Model
table(df.test$left, df.test$logit_pred)

#Decision Tree Classifier for Attrition
tree_model <- rpart(left ~ satisfaction_level + last_evaluation + number_project + 
                      + average_montly_hours + time_spend_company + 
                      Work_accident + promotion_last_5years, data = df.train, method="class")

# Plot tree 
fancyRpartPlot(tree_model)

#Create a column for predicted classes
df.test$tree_pred <- predict(tree_model, df.test, type = 'class')

#Confusion Matrix for Decision Tree Model
table(df.test$left, df.test$tree_pred)
