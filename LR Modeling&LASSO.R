
#USING REGRESSION FOR ANALYSING IMPACT OF IndependentVariables on DependentVariable
#USING LASSO TO HELP FIND SELECT FEATURES FOR ANALYSIS
SalaryData <- read.csv("PATH/Salary.csv")
str(SalaryData)
summary(SalaryData)
Salary <- SalaryData[ , 2]
Age <- SalaryData[ , 3]
Gender <- as.factor(SalaryData[ , 4])
#dim(SalaryData)
res1 = lm(Salary ~ Age + Gender)
summary(res1)
## the adj. R Sq is quite well...it is at around 87%...

## working with the amusement park data now...
hist(sat.df$logdist)


m2 <- lm(overall ~ rides + games + wait + clean, data = sat.df)
summary(m2)

#install.packages("coefplot")
library(coefplot)

coefplot(res1, intercept = TRUE, ylab = "Features", xlab = "Relationships")
coefplot(m2, intercept = FALSE, ylab = "Features", xlab = "Relationships with DV")
# this will tell us what all factors are strongly related with the dependent variable...

#Standardizing predictors...to scale...
str(sat.df)
sat.std <- sat.df[ , -3]
sat.std[ , 3:8] <- scale(sat.std[ , 3:8]) # now normalizing...
str(sat.std)

m3 <- lm(overall ~ rides + games + wait + clean + weekend + logdist + num.child, data = sat.std)
summary(m3)
coefplot(m3, intercept = T, ylab = "Features", xlab = "Relationship with Dependent Variable")
# so, wait time, cleanliness, rides, and num.child are a bit more correlated with DV compared to the rest of the independent variables...

# Discrete Independent Variables...

factor(sat.std$num.child)

sat.std$num.child.factor <- factor(sat.std$num.child)

# if we want to change reference group in the dummy variables, otherwise reference = 0 as a default...

#sat.std <- within(sat.std, num.child.factor <- relevel(num.child.factor, ref = 5))

## MODELING WITH CHILD FACTOR>>>>


m4 <- lm(overall ~ rides + games + wait+ clean + weekend + logdist + num.child.factor , data = sat.std)
summary(m4)
coefplot(m4, intercept = T, ylab = "Features", xlab = "Relationship with Dependent Variable")
## with coefplot, you can also see as to how the Dependent Variable is getting affected by different factor values of an Independent Variable...

AIC(m2); AIC(m3); AIC(m4)
BIC(m2); BIC(m3); BIC(m4)
# Model m4 is way better compared to the last two....

sat.std$has.child <- factor(sat.std$num.child > 0)
table(sat.std$has.child)

m5 <- lm(overall ~ rides + games + wait+ clean + weekend + logdist + has.child , data = sat.std)
summary(m5)
coefplot(m5, intercept = T, ylab = "Features", xlab = "Relationship with Dependent Variable")
AIC(m2) ; AIC(m3) ; AIC(m4) ; AIC(m5)
BIC(m2) ; BIC(m3) ; BIC(m4) ; BIC(m5)
# the dependent variable is highly influenced by the factor HAS.CHILD = TRUE.....>>>>>>>>>>>>>
###
## MOVING ON WITH INTERACTION TERMS.....


m6_1 <- lm(overall ~ wait + has.child + wait:has.child, data = sat.std)
summary(m6_1)

m6 <- lm(overall ~ rides + games + wait + clean + has.child + rides:has.child + games:has.child + wait:has.child
         + clean:has.child + rides:weekend + games:weekend + wait:weekend + clean:weekend, data = sat.std)
summary(m6)
coefplot(m6, intercept = T, ylab = "Features", xlab = "Relationship with Dependent Variable")
AIC(m2) ; AIC(m3) ; AIC(m4) ; AIC(m5) ; AIC(m6)
BIC(m2) ; BIC(m3) ; BIC(m4) ; BIC(m5) ; BIC(m6)


m7 <- lm( overall ~ rides + games + wait + clean + has.child + wait:has.child, data = sat.std)
coefplot(m7, intercept = T, ylab = "Features", xlab = "Relationship with Dependent Variable")
AIC(m2) ; AIC(m3) ; AIC(m4) ; AIC(m5) ; AIC(m6) ; AIC(m7)
BIC(m2) ; BIC(m3) ; BIC(m4) ; BIC(m5) ; BIC(m6) ; BIC(m7)
## TO MEASURE THE INTERACTION TERM, YOU CANNOT MISS THE REAL TERM IN THE MODEL>>>>>>>>>>>>>>>>>>>>>>>>>
## BRUTE force is not always the best way ahead...
# though there are less interaction terms in m7, we are still having a better model with that in play...

## REGRESSION with VARIABLE Selection:

Discover <- read.csv("D:/Machine Learning/Marketing Research and Analytics/Discover_step.csv", head = T)
dim(Discover)

str(Discover)

Sat <- Discover[ , 2]

IVs <- as.matrix(Discover[ ,3:15])
IVs

cor(IVs)

library(Hmisc)
res_cor <- lm(Sat ~ IVs)
summary(res_cor)

library(MASS)
fit <- lm (q4 ~ ., data = Discover[ , -1]) # everything except the ID column....

summary(fit)

step_both <- stepAIC(fit, direction = "both")
# stepAIC can be done in forward / backward direction or in both directions...

###using salary data...

lm.ridge(Salary ~ Age + Gender, SalaryData, lambda = 10)
# lm.ridge is giving better +ve explanatory power compared to the normal lm model...
lm(Salary ~ Age + Gender)
plot(lm.ridge(Salary ~ Age + Gender, data = SalaryData, lambda = seq(0, 10, 0.001)))

### now working with the Discover Dataset...
lm(q4 ~ ., data = Discover[ , -1])
lm.ridge(q4 ~ ., data = Discover[ , -1], lambda = 1)

fit <- lm.ridge(q4 ~ ., data = Discover[ , -1], lambda = seq(0, 500, by = 1))
plot(fit$GCV)
########################
which.min(fit$GCV)
# this gives us the most optimal values for lambda for the given model...
#######################
#which.max(fit$GCV)
lm.ridge(q4 ~ ., data = Discover, lambda = 120)

# Weak Shrinkage...
plot(lm.ridge(q4 ~., data = Discover[ , -1], lambda = seq(0, 10, 0.1)))

# strong shrinkage...
plot(lm.ridge(q4 ~., data = Discover[ , -1], lambda = seq(0, 100, 0.1)))


#####################
###LASSO###########
#################
install.packages("lars")
library(lars)

res.lasso <- lars(IVs, Sat, type = "lasso")
plot(res.lasso)

## coefficients are shrinking into zero values depending on alpha parameter in the Lasso formula...
dim(res.lasso$beta)
res.lasso$lambda

res.lasso$beta[1,]
res.lasso$lambda[1] # estimated coefficients with lambda values...

res.lasso$beta[6,] # five significant variables
res.lasso$beta[5,] # four significant variables
res.lasso$beta[4,] # three significant variables












