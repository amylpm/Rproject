library(dplyr)
library(tidyverse)
library(lubridate,magrittr)
library(ggfortify)
library(tseries)
library('vars')
library('quantmod')
library(LINselect)
#obtain dataset 
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
Madhya_Pradesh = subset(data, select = -c(state,location,type))
Madhya_Pradesh$day <- floor_date(Madhya_Pradesh$date, "day")
Madhya_Pradesh = aggregate(cbind(so2,no2,rspm,pm2_5) ~ day, data = Madhya_Pradesh, FUN = mean, na.rm = TRUE)
Madhya_Pradesh =  ts(Madhya_Pradesh)

autoplot(Madhya_Pradesh) +
  ggtitle("Time Series Plot of the Madhaya Pradesh Time-Series") +
  theme(plot.title = element_text(hjust = 0.5))
apply(Madhya_Pradesh, 2, adf.test)

VARselect(Madhya_Pradesh, 
          type = "none", #type of deterministic regressors to include. We use none becasue the time series was made stationary using differencing above. 
          lag.max = 10) #highest lag order

var.a <- vars::VAR(Madhya_Pradesh,
                   lag.max = 10, #highest lag order for lag length selection according to the choosen ic
                   ic = "AIC", #information criterion
                   type = "none") #type of deterministic regressors to include
summary(var.a)
serial.test(var.a)
#selecting the variables
# Granger test for causality
#for causality function to give reliable results we need all the variables of the multivariate time series to be stationary. 
causality(var.a, #VAR model
          cause = c("pm2_5")) #cause variable. If not specified then first column of x is used. Multiple variables can be used. 
fcast = predict(var.a, n.ahead = 25) # we forecast over a short horizon because beyond short horizon prediction becomes unreliable or uniform
par(mar = c(2.5,2.5,2.5,2.5))
plot(fcast)

f.val= fcast$fcst[5]; f.val # type list
plot(DAXinv)
#https://bookdown.org/singh_pratap_tejendra/intro_time_series_r/multivariate-ts-analysis.html

#https://stackoverflow.com/questions/65047250/running-a-loop-in-separate-time-series-in-r
#statewise = subset(data, select = -c(location,type))
#data%>% group_by(state)
#data

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