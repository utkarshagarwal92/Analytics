###################################
######## BASS model in R #########
#################################


#### 1. iPhone Example ####
## Data Loading
iphone_data=read.csv("sample_iphone_sales.csv", header=TRUE)
Sales=ts(iphone_data$Sales, start=c(2007,3), freq=4)


#### 2. Samsung Example ####
Samsung_data <- read.csv("D:/Machine Learning/Marketing Research and Analytics/Gallaxy.csv", header = T)
Sales=ts(Samsung_data$Sales, start=c(2007,1), freq=4)

#### 3. in-class Sales_Excel ####
sales_excel=read.csv("Sales_excel_vary.csv", header=TRUE)
Sales=ts(sales_excel$Sales, start=c(2004))

###########################
###BASS model from here###
#########################

# plot the data
plot(Sales, type="l", lty=2, col="red")
points(Sales,pch=20,col="blue")
title("Quarterly iPhone Sales(millions)")

# plot cumulative sales
Y=cumsum(Sales)
Y=ts(Y,start=c(2007,3),freq=4)#you have to check this for each example
plot(Y,type="l",lty=2,col="red")
points(Y,pch=20,col="blue")
title("Cumulative iPhone Sales(millions)")

##############################################
### fit BASS regression and compute m,p,q ###
############################################

# Estimating a, b and c using linear regression:

Y=c(0,Y[1:(length(Y)-1)]) 
#Y_t-1 is needed instead of Y_t (Cumulative Sales) to match with Sales.
Ysq=Y^2
out=lm(Sales~Y+Ysq) ##S(T)=a+bY(T)+c(Y(T))^2
summary(out)

a=out$coef[1]
b=out$coef[2]
c=out$coef[3]

mplus=(-b+sqrt(b^2-4*a*c))/(2*c) #m plus
mminus=(-b-sqrt(b^2-4*a*c))/(2*c) #m minus

#selecting mminus as m since mplus is minus in the equation 
m=mminus; 
p=a/m
q=b+p
####### BASS estimation code by here ##########

####### External Effect ##########

Ext=NULL

for (t in 1:length(Y)){
  Ext<-c(Ext,p*(m-Y[t])) 
} #External effects

plot(Ext,type='l')

####### Internal Effect ##########
Int=NULL

for (t in 1:length(Y)){
  Int<-c(Int,q*(Y[t]/m)*(m-Y[t])) 
}

plot(Int,type='l')

#########################################
### prediction model by using p, q, m ###
#########################################

Bass_Model=function(p,q,m,T=50){
  
  S=double(T) # total T=50; Creating a double-precision vector 
  Y=double(T+1) 
  Y[1]=0 # starting value
  
  for(t in 1:T){
    S[t]=p*m+(q-p)*Y[t]-(q/m)*Y[t]^2 # Estiamted Sales
    Y[t+1]=Y[t]+S[t]  # Cumulative Sales at t+1
  } #for t by here
  
  return(list(sales=S, cumSales=cumsum(S)))
  
}

#######################################
########### predictions & plots ######
####### per each datasets ###########
####################################

### 1. for iPhone case ###
Spred=Bass_Model(p,q,m, T=25)$sales 
Spred=ts(Spred,start=c(2007,3),freq=4)
ts.plot(Sales,Spred,col=c("blue","red")) #actual sales and estimated sales(spread)
legend("topleft", legend=c("actual","Bass Model"), fill=c("blue","red"))

## for cumulative sales

Spred=Bass_Model(p,q,m)$sales
CumSpred=ts(cumsum(Spred),start=c(2007,3),freq=4)
CumSales=ts(cumsum(Sales),start=c(2007,3),freq=4)
ts.plot(CumSales,CumSpred,col=c("blue","red"))
legend("topleft", legend=c("actual","Bass Model"),fill=c("blue","red"))
title("Predicted Cumulative iPhone Sales")

### 2. Samsung ###

Spred=Bass_Model(p,q,m, T=15)$sales
Spred=ts(Spred,start=c(2007,1),freq=4)
ts.plot(Sales,Spred,col=c("blue","red")) #actual sales and estimated sales(spread)
legend("topleft", legend=c("actual","Bass Model"), fill=c("blue","red"))

## for cumulative sales

Spred=Bass_Model(p,q,m)$sales
CumSpred=ts(cumsum(Spred),start=c(2007,1),freq=4)
CumSales=ts(cumsum(Sales),start=c(2007,1),freq=4)
ts.plot(CumSales,CumSpred,col=c("blue","red"))
legend("topleft", legend=c("actual","Bass Model"),fill=c("blue","red"))
title("Predicted Cumulative Samsung Sales")
