hs.train <- read.csv("D:/Machine Learning/train.csv", header = TRUE)
p.test <-  read.csv("D:/Machine Learning/week 3/energy_renewed/test.csv", header = TRUE)
ps.smotetest<- read.csv("D:/Machine Learning/week 3/energy_renewed//testsmote/train.csv")

library(Amelia)

missmap(p.train, main = "Missing Data Finder")

install.packages("tidyverse")
library(tidyverse)
install.packages("reshape2")
library(reshape2)

table(hs.train$QuoteConversion_Flag)
table(hs.train1$QuoteConversion_Flag)
str(hs.train)

###to remove the ID column from the data ser::::
hs.train<- hs.train[-125]
hs.train<- hs.train[-161]
################################################
###to look at every column without having them truncated from the result set!
str(hs.train, list.len=ncol(hs.train))
#############################################################################
table(hs.train$QuoteConversion_Flag)
table(hs.train1$QuoteConversion_Flag)
#############################################################################

na.fail(train)
table(train$QuoteConversion_Flag)
trainX <- na.omit(train)
table(trainX$QuoteConversion_Flag)
p.train <- p.train[-57]

na.fail(hs.train)
hs.train3 <- na.omit(hs.train1)
na.fail(hs.train3)

library(caret)
library(DMwR)

hs.train1$QuoteConversion_Flag<- as.factor(hs.train1$QuoteConversion_Flag)

str(hs.train1)
hs.train<- hs.train[-1]
table(hs.train3$QuoteConversion_Flag)
hs.train3 <- SMOTE(QuoteConversion_Flag~., hs.train1, k=5, perc.over = 100,perc.under=50)
table(hs.train2$QuoteConversion_Flag)

write.csv(trainX, 'D:/Machine Learning/homesite/trainX.csv', row.names=T)

####################################NOT GONNA NORMALIZE THIS DATASET...cuz it sucks!
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
str(trainX)
trainZ<-trainX

trainZ<- trainZ[,-feature.names]

hs.train2 <- as.data.frame(lapply(trainX[-2], normalize))
str(pst.norm)
#########################################################################
pst.norm$target <- ps.train$target
  

write.csv(hs.train1,'D:/Machine Learning/homesite.train.csv', row.names=T)
write.csv(pst.norm, 'D:/Machine Learning/week 3/energy_renewed/pst.norm.csv', row.names=T)
table(pst.norm$target)
table(trainX$QuoteConversion_Flag)
##########split data for training and testing purpos::::::::
trainer <- trainX[1:200000,]
tester <- trainX[200001:260753,]
#############
table(trainer$QuoteConversion_Flag)
table(tester$QuoteConversion_Flag)
str(trainX)

abc <- pst.norm[sample(1:nrow(pst.norm), 175000,
                          replace=FALSE),]
install.packages("sqldf")
library(sqldf)
cba <- sqldf('select * from asd except select * from dsa')

asd<-as.data.table(pst.norm)
dsa<- as.data.table(abc)
table(abc$target)
table(cba$target)
trainX<-abc
testX<-cba
trainY<-abc
testY<-cba
cba <- pst.norm[sample(175001:nrow(pst.norm), 221737,
                       replace=FALSE),]
table(pst.norm$target)
table(abc$target)
table(cba$target)

#########################################################################################################
##Decision trees!!!!!!
str(trainer)
trainer$QuoteConversion_Flag<- as.factor(trainer$QuoteConversion_Flag)
tester$QuoteConversion_Flag<-as.factor(tester$QuoteConversion_Flag)
#First up...C5.0 algorithm!

train.c50 <- C50::C5.0(trainer[-2], trainer$QuoteConversion_Flag, trials = 10)

train.c50

Oracle1.0<- predict(train.c50, tester)
library(gmodels)
CrossTable(tester$QuoteConversion_Flag, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))


trained5.0 <- C50::C5.0(trainer[-2], trainer$QuoteConversion_Flag,
                       trials = 10)
trained5.0
summary(trained5.0)
library(C50)
Oracle1.0 <- predict.C5.0(trained5.0, tester)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))


## Next one is regression Tree

##

train.rpart <- rpart::rpart(target ~ ., data = trainX)


##

## Now showing... M5P

train.m5p <- RWeka::M5P(target ~ ., data = trainX)
  
##

## the star of the show is here...time for XGBoost guys!!!
install.packages("xgboost")
require(xgboost)

library(data.table)
library(mlr)

setDT(trainY)
setDT(testY)
table(is.na(trainY))
sapply(testY, function(x) sum(is.na(x))/length(x))*100
setDT(p.test)
table(is.na(p.test))

#using one hot encoding 
labels <- trainY$target 
ts_label <- testY$target
p.test$target <- 0
ts_label1 <- p.test$target
new_tr <- model.matrix(~.+0,data = trainY[,-c("target"),with=F]) 
new_ts <- model.matrix(~.+0,data = testY[,-c("target"),with=F])
str(testY$target)
table(testY$target)
table(p.test$target)
str(p.test$target)
p.test$target<- as.factor(p.test$target)
new_tst <- model.matrix(~.+0, data = p.test[,-c("target"), with  = F])

#convert factor to numeric 
labels <- as.numeric(labels)-1
ts_label <- as.numeric(ts_label)-1
tst_label <- as.numeric(ts_label1)-1
table(labels)
table(ts_label)
table(tst_label)
library(stringr)
dtrain <- xgb.DMatrix(data = new_tr,label = labels) 
dtest <- xgb.DMatrix(data = new_ts,label=ts_label)
Dtest <- xgb.DMatrix( data = new_tst, label = ts_label1)
#default parameteres
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

#using Cross Validation fuction of XGBoost algorithm

xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)

min(xgbcv$test.error.mean)
#first default - model training
xgb1 <- xgb.train (params = params, 
                   data = dtrain, 
                   nrounds = 79, 
                   watchlist = list(val=dtest,train=dtrain), 
                   print_every_n = 10, 
                   early_stop_round = 10, 
                   maximize = F , 
                   eval_metric = "error", type = "response")


#model prediction
xgbpred <- predict (xgb1,Dtest)

write.csv(xgbpred, 'D:/Machine Learning/week 3/energy_renewed/xgbprediction.csv', row.names=T)

xgbpred <- ifelse (xgbpred > 0.5,1,0)

xgbpredfinal <- predict(xgb1, p.test)

xgbpredictfinal <- ifelse(xgbpredictfinal > 0.5,1,0)
install.packages('e1071', dependencies=TRUE)
#confusion matrix
library(caret)
table(ts_label)
confusionMatrix (xgbpred, ts_label)
#Accuracy - 86.54%` 
str(ts_label)

#view variable importance plot
mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20]) 


#convert characters to factors
fact_col <- colnames(trainY)[sapply(trainY,is.character)]

for(i in fact_col) set(trainY,j=i,value = factor(trainY[[i]]))
for (i in fact_col) set(testY,j=i,value = factor(testY[[i]]))

#create tasks
traintask <- makeClassifTask (data = trainY,target = "target")
testtask <- makeClassifTask (data = testY,target = "target")

table(trainY$target)
#do one hot encoding`<br/> 
traintask <- createDummyFeatures (obj = traintask,target = "target") 
testtask <- createDummyFeatures (obj = testtask,target = "target")





train.xgb <- xgboost(data = trainX$., label = train$label,
                 nrounds = 2, objective = "binary:logistic")



trainY <- data.frame(trainX)
testY <- data.frame(testX)
str(trainY)
data(trainY, package = 'xgboost')
data(testY, package = 'xgboost')

#########################################################################################################

na.fail(p.train)
somethingelse<-!is.na.data.frame(p.train)
na.fail(somethingelse)

str(somethingelse)
na.fail(p.train)
summary(p.train)

install.packages("magrittr")
library(magrittr)
library(ggplot2)
ggplot_missing <- function(x){
  
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = X2,
               y = X1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}

ggplot_missing(p.train)

# sum(is.na(hs.train $ Original_Quote_Date ))
# sum(is.na(hs.train $ QuoteConversion_Flag))
# sum(is.na(hs.train $ Field6              ))
# sum(is.na(hs.train $ Field7              ))
# sum(is.na(hs.train $ Field8              ))
# sum(is.na(hs.train $ Field9              ))
# sum(is.na(hs.train $ Field10             ))
# sum(is.na(hs.train $ Field11             ))
# sum(is.na(hs.train $ Field12             ))
# sum(is.na(hs.train $ CoverageField1A     ))
# sum(is.na(hs.train $ CoverageField1B     ))
# sum(is.na(hs.train $ CoverageField2A     ))
# sum(is.na(hs.train $ CoverageField2B     ))
# sum(is.na(hs.train $ CoverageField3A     ))
# sum(is.na(hs.train $ CoverageField3B     ))
# sum(is.na(hs.train $ CoverageField4A     ))
# sum(is.na(hs.train $ CoverageField4B     ))
# sum(is.na(hs.train $ CoverageField5A     ))
# sum(is.na(hs.train $ CoverageField5B     ))
# sum(is.na(hs.train $ CoverageField6A     ))
# sum(is.na(hs.train $ CoverageField6B     ))
# sum(is.na(hs.train $ CoverageField8      ))
# sum(is.na(hs.train $ CoverageField9      ))
# sum(is.na(hs.train $ CoverageField11A    ))
# sum(is.na(hs.train $ CoverageField11B    ))
# sum(is.na(hs.train $ SalesField1A        ))
# sum(is.na(hs.train $ SalesField1B        ))
# sum(is.na(hs.train $ SalesField2A        ))
# sum(is.na(hs.train $ SalesField2B        ))
# sum(is.na(hs.train $ SalesField3         ))
# sum(is.na(hs.train $ SalesField4         ))
# sum(is.na(hs.train $ SalesField5         ))
# sum(is.na(hs.train $ SalesField6         ))
# sum(is.na(hs.train $ SalesField7         ))
# sum(is.na(hs.train $ SalesField8         ))
# sum(is.na(hs.train $ SalesField9         ))
# sum(is.na(hs.train $ SalesField10        ))
# sum(is.na(hs.train $ SalesField11        ))
# sum(is.na(hs.train $ SalesField12        ))
# sum(is.na(hs.train $ SalesField13        ))
# sum(is.na(hs.train $ SalesField14        ))
# sum(is.na(hs.train $ SalesField15        ))
# sum(is.na(hs.train $ PersonalField1      ))
# sum(is.na(hs.train $ PersonalField2      ))
# sum(is.na(hs.train $ PersonalField4A     ))
# sum(is.na(hs.train $ PersonalField4B     ))
# sum(is.na(hs.train $ PersonalField5      ))
# sum(is.na(hs.train $ PersonalField6      ))
# sum(is.na(hs.train $ PersonalField7      ))
# sum(is.na(hs.train$PersonalField7))
table(hs.train$PersonalField7)
str(hs.train$PersonalField7)


toBeRemoved <- which(hs.train1$PropertyField38=="")

toBeRemoved

hs.train1<-hs.train1[-toBeRemoved,]

hs.train2<- hs.train1

table(hs.train1$PropertyField38)

table(hs.train2$PropertyField38)



hs.train1$PersonalField7<-as.factor(hs.train1$PersonalField7)
table(hs.train1$PersonalField7)
levels(hs.train1$PersonalField7)
hs.train1$PersonalField7 <- as.factor(hs.train1$PersonalField7)

###
hs.train1$PropertyField38<-droplevels(hs.train1$PropertyField38)
table(hs.train1$PropertyField38)
str(hs.train1$PropertyField38)
###
xyz <- ifelse(hs.train$PersonalField7 == 'Y', 1, ifelse(hs.train$PersonalField7 == 'N', 0, ''))
table(xyz)
xyz1<-na.omit(xyz)
table(xyz1)
levels(xyz)
levels(hs.train$PersonaField7)





