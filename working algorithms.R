##### Chapter 6: Regression Methods -------------------

#### Part 1: Linear Regression -------------------

## Understanding regression ----
## Example: Space Shuttle Launch Data ----
launch <- read.csv("D:/Machine Learning/MLwR-master/Machine Learning with R (2nd Ed.)/Chapter 06/challenger.csv")

str(launch)

# estimate beta manually
b <- cov(launch$temperature, launch$distress_ct) / var(launch$temperature)
b

# estimate alpha manually
a <- mean(launch$distress_ct) - b * mean(launch$temperature)
a

# calculate the correlation of launch data
r <- cov(launch$temperature, launch$distress_ct) /
  (sd(launch$temperature) * sd(launch$distress_ct))
r
cor(launch$temperature, launch$distress_ct)

# computing the slope using correlation
r * (sd(launch$distress_ct) / sd(launch$temperature))

# confirming the regression line using the lm function (not in text)
model <- lm(DataSet$QuoteConversion_Flag ~ ., data = DataSet)
model
summary(model)

# creating a simple multiple regression function
reg <- function(y, x) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  b <- solve(t(x) %*% x) %*% t(x) %*% y
  colnames(b) <- "estimate"
  print(b)
}


# examine the launch data
str(launch)

# test regression model with simple linear regression
reg(y = DataSet$QuoteConversion_Flag, x = DataSet[2])

# use regression model with multiple regression
reg(y = launch$distress_ct, x = launch[2:4])

# confirming the multiple regression result using the lm function (not in text)
model <- lm(distress_ct ~ temperature + field_check_pressure + flight_num, data = launch)
model

## Example: Predicting Medical Expenses ----
## Step 2: Exploring and preparing the data ----
insurance <- read.csv("D:/Machine Learning/MLwR-master/Machine Learning with R (2nd Ed.)/Chapter 06/insurance.csv", stringsAsFactors = TRUE)
str(insurance)

# summarize the charges variable
summary(insurance$expenses)

# histogram of insurance charges
hist(insurance$expenses)

# table of region
table(insurance$region)

# exploring relationships among features: correlation matrix
cor(insurance[c("age", "bmi", "children", "expenses")])

# visualing relationships among features: scatterplot matrix
pairs(insurance[c("age", "bmi", "children", "expenses")])

# more informative scatterplot matrix
install.packages("psych")
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])

############pairs.panel provides us with a line of fit and the scatterplot along with the rest that just "Pairs" can provide with!!!!!!

pairs(insurance[c("age", "bmi", "children", "expenses")])

## Step 3: Training a model on the data ----
ins_model <- lm(expenses ~ age + children + bmi + sex + smoker + region,
                data = insurance)
ins_model <- lm(expenses ~ ., data = insurance) # this is equivalent to above

# see the estimated beta coefficients
ins_model

predicted_insurance<- data.frame(predict(ins_model, insurance, type = "prob"))

str(predicted_insurance)
## Step 4: Evaluating model performance ----
# see more detail about the estimated beta coefficients
summary(ins_model)

## Step 5: Improving model performance ----

# add a higher-order "age" term
insurance$age2 <- insurance$age^2
?rlevel
# add an indicator for BMI >= 30
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

# create final model
ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex +
                   bmi30*smoker + region, data = insurance)
summary(ins_model)
summary(ins_model2)

#### Part 2: Regression Trees and Model Trees -------------------

## Understanding regression trees and model trees ----
## Example: Calculating SDR ----
# set up the data
tee <- c(1, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 7, 7, 7)
at1 <- c(1, 1, 1, 2, 2, 3, 4, 5, 5)
at2 <- c(6, 6, 7, 7, 7, 7)
bt1 <- c(1, 1, 1, 2, 2, 3, 4)
bt2 <- c(5, 5, 6, 6, 7, 7, 7, 7)

# compute the SDR
sdr_a <- sd(tee) - (length(at1) / length(tee) * sd(at1) + length(at2) / length(tee) * sd(at2))
sdr_b <- sd(tee) - (length(bt1) / length(tee) * sd(bt1) + length(bt2) / length(tee) * sd(bt2))

# compare the SDR for each split
sdr_a
sdr_b

## Example: Estimating Wine Quality ----
## Step 2: Exploring and preparing the data ----
wine <- read.csv("D:/Machine Learning/MLwR-master/Machine Learning with R (2nd Ed.)/Chapter 06/whitewines.csv")

# examine the wine data
str(wine)

# the distribution of quality ratings
hist(wine$quality)

# summary statistics of the wine data
summary(wine)

wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]
DataSet$QuoteConversion_Flag
## Step 3: Training a model on the data ----
# regression tree using rpart
str(DataSet)
str(DataSet, list.len=ncol(DataSet))
str(final_test, list.len=ncol(final_test))
library(rpart)
m.rpart <- rpart(DataSet$QuoteConversionFlag ~ ., data = DataSet)
m.rpart <- rpart(QuoteConversion_Flag ~ ., data = DataSet, control = rpart.control(cp = 0.0001))
# get basic information about the tree
m.rpart
install.packages("rpart.plot")
library("rpart.plot")
rpart_plot(m.rpart)
# get more detailed information about the tree
summary(m.rpart)


final_test$ GeographicField62A <-NULL
final_test$ GeographicField61A <-NULL
final_test$ GeographicField60A <-NULL
final_test$ GeographicField56A <-NULL
final_test$ GeographicField21A <-NULL
final_test$ GeographicField22A <-NULL
final_test$ GeographicField23A <-NULL
final_test$ GeographicField18A <-NULL
final_test$ GeographicField14A <-NULL
final_test$ GeographicField10A <-NULL
final_test$ GeographicField5A  <-NULL
final_test$ PropertyField29    <-NULL
final_test$ PropertyField11A   <-NULL
final_test$ PropertyField2A    <-NULL
final_test$ PersonalField84    <-NULL
final_test$ QuoteNumber        <-NULL
final_test$ Original_Quote_Date<-NULL





# use the rpart.plot package to create a visualization
library(rpart.plot)

# a basic decision tree diagram
rpart.plot(m.rpart, digits = 3)

# a few adjustments to the diagram
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101, )

?rpart.control

## Step 4: Evaluate model performance ----

# generate predictions for the testing dataset
X_test$Original_Quote_Date<- as.Date(X_test$Original_Quote_Date)
X_test$month <- as.integer(format(X_test$Original_Quote_Date, "%m"))
X_test$year <- as.integer(format(X_test$Original_Quote_Date, "%y"))
X_test$day <- weekdays(as.Date(X_test$Original_Quote_Date))
stacked1.0 <- read.csv('D:/Machine Learning/homesite/stack ensemble/finalone/stacked_v2.0.csv', header = TRUE)
str(stacked1.0)
table(kaggle$QuoteConversion_Flag)

write.csv(stacked1.0,'D:/Machine Learning/homesite/stack ensemble/test/stacked_test1.1.csv')
stacked1.0$ probab_b  <- as.numeric(stacked1.0$ probab_b  )   
stacked1.0$ probab_j  <- as.numeric(stacked1.0$ probab_j  )   
stacked1.0$ probabj1  <- as.numeric(stacked1.0$ probabj1  )   
stacked1.0$ probab_b2 <- as.numeric(stacked1.0$ probab_b2 )   
stacked1.0$ probab_b3 <- as.numeric(stacked1.0$ probab_b3 )   
stacked1.0$ probab_svm<- as.numeric(stacked1.0$ probab_svm)   
stacked1.0$ probab_f  <- as.numeric(stacked1.0$ probab_f  )   
write.csv(stacked1.0,'D:/Machine Learning/homesite/stack ensemble/finalone/stackedv4.0.csv')
na.fail(kaggle)
str(kaggle$Scored.Labels)
stacked1.0$QuoteConversion_Flag[is.na(stacked1.0$QuoteConversion_Flag)] <-1
table(kaggle$QuoteConversion_Flag)
na.fail(kaggle)
write.csv(kaggle,'D:/Machine Learning/homesite/shitpiece2.csv')
write.csv(kaggle,'D:/Machine Learning/homesite/kaggle_v2.csv')
p.rpart <- predict(m.rpart, test, type = c("vector", "prob", "class", "matrix"))
itrain <- read.csv('D:/Machine Learning/homesite/train1.csv', header = TRUE)
X_test <- read.csv('D:/Machine Learning/homesite/test/test.csv', header = TRUE)
write.csv(X_test,'D:/Machine Learning/homesite/test/testX.csv')
table(test$PersonalField16)
str(test$PersonalField16)
table(final_test$PersonalField16)
str(final_test$PersonalField16)
memory.limit(size = 50000)
predictions <- predict(m.rpart, final_test, type = "prob")
write.csv(predictions, 'D:/Machine Learning/homesite/predictions.csv')
levels(DataSet$PropertyField5)
levels(final_test$PropertyField5)
final_test$Field6<-as.factor(final_test$Field6)
final_test$Field6<-droplevels(final_test$Field6)
final_test$Field6<-as.factor(final_test$Field6)
droplevels.factor(final_test$Field6, exclude = if(anyNA(levels(final_test$Field6))) NULL else NA)
173827
173828
173832
173836
levels(DataSet$PersonalField16)
levels(itrain$PersonalField16)
levels(final_test$PersonalField16)
p.rpart <- predict(m.rpart, test, type = "prob")
install.packages("Amelia")

library(Amelia)
missmap(DataSet, main = "missing val test")

install.packages("ROCR")



library(ROCR)
# compare the distribution of predicted values vs. actual values
summary(p.rpart)
summary(test$QuoteConversion_Flag)

# compare the correlation
cor(p.rpart, test$QuoteConversion_Flag)

# function to calculate the mean absolute error
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))  
}

# mean absolute error between predicted and actual values
MAE(p.rpart, test$QuoteConversion_Flag)

# mean absolute error between actual values and mean value
mean(wine_train$quality) # result = 5.87
MAE(5.87, wine_test$quality)

## Step 5: Improving model performance ----
# train a M5' Model Tree
library(RWeka)
memory.limit(size = 120000)
options(java.parameters = "-Xmx4000m")
library(rJava)
m.m5p <- M5P(QuoteConversion_Flag ~ ., data = train)

p12<-read.csv('D:/p12.csv', header = TRUE)
install.packages("Metrics")
library(Metrics)
rmse(p12$Sales,p12$Predicted)
mape(p12$Sales,p12$Predicted)

# display the tree
m.m5p

# get a summary of the model's performance
summary(m.m5p)

# generate predictions for the model
p.m5p <- predict(m.m5p,test)

# summary statistics about the predictions
summary(p.m5p)

# correlation between the predicted and true values
cor(p.m5p, wine_test$quality)

# mean absolute error of predicted and true values
# (uses a custom function defined above)
MAE(wine_test$quality, p.m5p)
