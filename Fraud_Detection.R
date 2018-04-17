install.packages("regclass")
library("regclass")

insurancefraud<- read.csv('D:/Machine Learning/week-2/Insurance Fraud - TRAIN-3000(1)')
library(readr)
if1<- read_csv("D:/Machine Learning/week-2/Insurance Fraud - TRAIN-3000(1).csv")
table(if1$FRAUDFOUND)
pb_train <- read_csv("D:/Machine Learning/week-2/Portugese Bank Data - TRAIN(1).csv")
table(pb_train$y)

str(PB_train)
table(PB_train$y)
str(if1)
pb_train$y<- factor(ifelse(pb_train$y == "no", labels("no"),ifelse(pb_train$y == "yes",labels("yes"),NA)))

str(PB_train)
pb_train$y <- factor(pb_train$y, levels = c("no","yes"),
                                      labels = c("No", "Yes"))


str(insurancefraud)
table(insurancefraud$FRAUD_Binary)
insurancefraud$FRAUD_Binary <- factor(insurancefraud$FRAUDFOUND, levels = c(0, 1),
                         labels = c("NotFraud", "Fraud"))

if1$FRAUD_Binary<- factor(if1$FRAUDFOUND, levels = c('No','Yes'), labels = c('NotAFraud', 'Fraud'))
table(if1$FRAUD_Binary)
insurancefraud$FRAUD_Binary
table(insurancefraud$FRAUDFOUND)
table(insurancefraud$FRAUD_Binary)
round(prop.table(table(insurancefraud$FRAUD_Binary)) * 100, digits = 1)
#86.7% not fraud
#13.3% are fraudulent cases in all!!!

insurancefraud_1 <- as.data.frame(scale(insurancefraud))
str(insurancefraud_1)
#PB_train[, c(1:17)] <- sapply(PB_train[, c(1:17)], as.numeric)
table(insurancefraud$MONTH)

insurancefraud<- insurancefraud%>% 
  mutate(MONTHCLAIMED = ifelse(MONTHCLAIMED == 'Jan', 1, 
                        ifelse(MONTHCLAIMED == 'Feb',2,
                               ifelse(MONTHCLAIMED == 'Mar',3,
                                      ifelse(MONTHCLAIMED == 'Apr',4,
                                             ifelse(MONTHCLAIMED == 'May',5,
                                                    ifelse(MONTHCLAIMED == 'Jun',6,
                                                           ifelse(MONTHCLAIMED == 'Jul',7,
                                                                  ifelse(MONTHCLAIMED == 'Aug',8,
                                                                     ifelse(MONTHCLAIMED == 'Sep',9,
                                                                         ifelse(MONTHCLAIMED == 'Oct',10,
                                                                                ifelse(MONTHCLAIMED == 'Nov',11,12))))))))))))

insurancefraud<- insurancefraud%>%
  mutate(DAYOFWEEKCLAIMED = ifelse(DAYOFWEEKCLAIMED == 'Monday',1,
                            ifelse(DAYOFWEEKCLAIMED == 'Tuesday',2,
                                   ifelse(DAYOFWEEKCLAIMED == 'Wednesday',3,
                                          ifelse(DAYOFWEEKCLAIMED == 'Thursday',4,
                                                 ifelse(DAYOFWEEKCLAIMED == 'Friday',5,
                                                        ifelse(DAYOFWEEKCLAIMED == 'Saturday',6,7)))))))

table(insurancefraud$MAKE)

insurancefraud<- insurancefraud%>%
  mutate(MAKE = ifelse(MAKE == 'Accura',1,
                       ifelse(MAKE == 'BMW',2,
                              ifelse(MAKE == 'Chevrolet',3,
                                     ifelse(MAKE == 'Dodge',4,
                                            ifelse(MAKE == 'Ford',5,
                                                   ifelse(MAKE == 'Honda',6,
                                                          ifelse(MAKE == 'Mazda',7,
                                                                 ifelse(MAKE == 'Mecedes',8,
                                                                        ifelse(MAKE == 'Mercury',9,
                                                                               ifelse(MAKE == 'Nisson',10,
                                                                                      ifelse(MAKE == 'Pontiac',11,
                                                                                             ifelse(MAKE == 'Porche',12,
                                                                                                    ifelse(MAKE == 'Saab',13,
                                                                                                           ifelse(MAKE == 'Saturn',14,
                                                                                                                  ifelse(MAKE == 'Toyota',15,16))))))))))))))))
table(insurancefraud$BASEPOLICY)

insurancefraud<- insurancefraud%>%
  mutate(FAULT = ifelse(FAULT == 'Policy_Holder',1,2))
DAYOFWEEKCLAIMED
insurancefraud$MARITALSTATUS

insurancefraud<- insurancefraud%>%
  mutate(AGEOFPOLICYHOLDER = ifelse(AGEOFPOLICYHOLDER == '16_to_17',1,
                             ifelse(AGEOFPOLICYHOLDER == '18_to_20',2,
                                    ifelse(AGEOFPOLICYHOLDER == '21_to_25',3,
                                           ifelse(AGEOFPOLICYHOLDER == '26_to_30',4,
                                                  ifelse(AGEOFPOLICYHOLDER == '31_to_35',5,
                                                         ifelse(AGEOFPOLICYHOLDER == '36_to_40',6,
                                                                ifelse(AGEOFPOLICYHOLDER == '41_to_50',7,
                                                                       ifelse(AGEOFPOLICYHOLDER == '51_to_65',8,9)))))))))
insurancefraud<- insurancefraud%>%
  mutate(AGENTTYPE = ifelse(AGENTTYPE == 'External',1,2))


insurancefraud<- insurancefraud%>%
  mutate(BASEPOLICY = ifelse(BASEPOLICY == 'All_Perils',1,
                               ifelse(BASEPOLICY == 'Collision',2,3)))
        

table(PB_train$y)                              

table(if1$FRAUD_Binary)

str(insurancefraud)

set.seed(123)
train_sample <- sample(2999,2000)
insurance1_train <- if1[1:2000, ]
insurance1_test <- if1[2001:2999, ]
str(PB_train)
table(PB_train$y)
#insurance_train_labels <- if1[1:2500, 33]
#insurance_test_labels <- if1[2501:2999, 33]

train_sample <- sample(2999, 2000)
insurance1_train <- if2[1:2000, ]
insurance1_test <- if2[2001:2999, ]
str(if2)
table(if2$FRAUD_Binary)
insurance_train_labels <- if1[1:2000, 33]
insurance_test_labels <- if1[2001:2999, 33]

if2<- if1[-32]

library(class)
install.packages('gmodels')
library(gmodels)
insurance_test_pred <- knn(train = insurance1_train, test = insurance1_test,
                      cl = insurance_train_labels, k = 21)
str(if1)
set.seed(123)
train_sample <- sample(2999,2000)
insurance1_train <- if1[1:2000, ]
insurance1_test <- if1[2001:2999, ]

IF_train <- if1[train_sample, ]
IF_test  <- if1[-train_sample, ]
str(Bank_train)
str(Bank_test)
set.seed(123)
train_sample <- sample(2999,2000)
insurance1_train <- if1[1:2000, ]
insurance1_test <- if1[2001:2999, ]

IF_train <- if1[train_sample, ]
IF_test  <- if1[-train_sample, ]
round(prop.table(table(Bank_test$y)),3)

round(prop.table(table(Bank_train$y)),3)
prop.table(table(insurance_train$FRAUD_Binary))
prop.table(table(insurance_test$FRAUD_Binary))
install.packages('C50')
library(C50)
str(IF_train)
IF_train<- IF_train[-32]
str(IF_train)

IF_model <- C5.0(IF_train[-32], IF_train$FRAUD_Binary)


TREE1 <- rpart(FRAUD_Binary~.,data=IF_train,cp=0.0001)
TREE1

install.packages("tree")
library(tree)
summarize_tree(TREE1)

#POLICYTYPE                 YEAR                MONTH         MONTHCLAIMED           BASEPOLICY      VEHICLECATEGORY                FAULT                 MAKE 
#47.74208169          47.62326660          41.16364859          40.82756659          38.91887142          30.90891429          30.87688066          24.10039005 
#DAYOFWEEK

#fatorize the target var..
#

IF_model

credit_model
install.packages("RWeka")
library(RWeka)
install.packages('DMwR')
library(DMwR)
install.packages('rpart')
library(rpart)
library(pROC)
library(caret)
install.packages('tree')
library(tree)
summary(bank_model)
insurance_model

bank_pred <- predict(bank_model, Bank_test)



table(insurance_train$FRAUD_Binary)
library(gmodels)
CrossTable(Bank_test$default, bank_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual y', 'predicted y'))



















