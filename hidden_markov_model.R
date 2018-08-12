#Using hidden markov chains for predicting the next phase of the financial market

install.packages("depmixS4")
library("depmixS4") 
library(lubridate)
library(xts)
library(TTR)

market=read.csv("/Users/jmc/Downloads/EURUSD1d.csv")

#create a time series object (atr is a volatility indicator)
Date=as.character(market[,1])
Date_ts=as.POSIXlt(Date,format="%Y.%m.%d %H:%M:%S")
data_ts=data.frame(market[,2:5],row.names=Date_ts)
data_ts=as.xts(data_ts)

#calculate the atr indicator
atr_indicator=ATR(data_ts[,2:4],n=14)
atr_indicator
atr=atr_indicator[,2]

log_returns=log(market$Close)-log(market$Open) #log returns 
model_data=data.frame(log_returns,atr)
model_data=model_data[-c(1:14),]
names(model_data)

#building the hidden markov model (moving from one state to the next state)
HMM<-depmix(list(log_returns~1,atr~1),data=model_data,nstates=3,family=list(gaussian(),gaussian())) 
HMM_Fit=fit(HMM,verbose=FALSE)
summary(HMM_Fit)

