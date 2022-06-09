#include the required libraries
library(dplyr)
library(tidyverse)
library(lubridate,magrittr)
library(ggfortify)
library(tseries)
library('vars')
library('quantmod')
library(LINselect)
library(forecast)

#obtain dataset 
#change directory if needed
#getwd()
#setwd('C:/Users/Acer/OneDrive/Documents/GitHub/wqd7004-group-project')
data = read.csv("data/data_ts.csv")
head(data)

# data types
str(data)

#convert date from char into date type
data$date=as.Date(data$date, "%Y-%m-%d")

#create a separate df for state Madhya_pradesh
x = unique(data['state'])
Madhya_Pradesh = data[which (data$state == 'Madhya Pradesh'), ] 
#remove location and type
Madhya_Pradesh = subset(data, select = -c(state,location,type))
#group the date by mean
Madhya_Pradesh$day <- floor_date(Madhya_Pradesh$date, "day")
Madhya_Pradesh = aggregate(cbind(so2,no2,rspm,pm2_5) ~ day, data = Madhya_Pradesh, FUN = mean, na.rm = TRUE)

#create train and test set(test set for 30 days)
MP_train= Madhya_Pradesh[1:697, ]
MP_train=subset(MP_train,select = -c(day))
MP_test= Madhya_Pradesh[698:727, ]

#obtain day in number for ts()
#as.numeric(as.Date("2014-01-01") - as.Date("2014-01-01"))
#as.numeric(as.Date("2015-08-09") - as.Date("2015-01-01"))

MP_train=ts(MP_train,start = c(2014,0),frequency = 365)
#MP_test= ts(MP_test,start = c(2015,220),frequency = 365)

# plot the ts graph
autoplot(MP_train) +
  ggtitle("Time Series Plot of the Madhaya Pradesh Time-Series") +
  theme(plot.title = element_text(hjust = 0.5))

# test for stationarity
apply(MP_train, 2, adf.test)

#since all data is stationary proceed to modelling
#select the n var VAR(n)
VARselect(MP_train, 
          type = "none", #type of deterministic regressors to include. We use none becasue the time series was made stationary using differencing above. 
          lag.max = 20) #highest lag order
#n= 10 
var.a <- vars::VAR(MP_train,
                   lag.max = 10, #highest lag order for lag length selection according to the choosen ic
                   ic = "AIC", #information criterion
                   type = "none") #type of deterministic regressors to include

summary(var.a)
#Portmanteau Test 
#null hypothesis of no autocorrelation is rejected 
serial.test(var.a)


# Granger test for causality
# Rejected the null hypothesis for the test is that lagged x-values do not explain the variation in y. In other words, it assumes that x(t) doesn't Granger-cause y(t). 
#for causality function to give reliable results we need all the variables of the multivariate time series to be stationary. 
causality(var.a, #VAR model
          cause = c("pm2_5")) #cause variable. If not specified then first column of x is used. Multiple variables can be used. 

fcast = predict(var.a, n.ahead =30) # we forecast over a month
par(mar = c(2.5,2.5,2.5,2.5))
plot(fcast)
#evaluation, requires numeric vector 
f.val= fcast$fcst$pm2_5[,1]
a.val= MP_test[['pm2_5']]
accuracy(f.val,a.val)


#https://bookdown.org/singh_pratap_tejendra/intro_time_series_r/multivariate-ts-analysis.html






#https://stackoverflow.com/questions/65047250/running-a-loop-in-separate-time-series-in-r
#statewise = subset(data, select = -c(location,type))
#data%>% group_by(state)
#data_by_state <- split(statewise, data$state)
#ly <- lapply(data_by_plot, function(df){
#  })
#result <- do.call(rbind, ly)
#Odisha=data[which (data$state == 'Odisha'), ] 
#Telangana=data[which (data$state == 'Telangana'), ] 
#Gujarat=data[which (data$state == 'Gujarat'), ] 
#Delhi=data[which (data$state == 'Delhi'), ] 
#Goa=data[which (data$state == 'Goa'), ] 
#West_Bengal=data[which (data$state == 'West Bengal'), ] 
#Tamil_Nadu=data[which (data$state == 'Tamil Nadu'), ] 
#Dadra_NagarHaveli=data[which (data$state == 'Dadra & Nagar Haveli'), ] 
#Daman_Diu=data[which (data$state == 'Daman & Diu'), ] 