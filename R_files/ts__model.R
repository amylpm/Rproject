#obtain dataset 
#getwd()
#setwd('C:/Users/Acer/OneDrive/Documents/GitHub/wqd7004-group-project')
data = read.csv("data/data_ts.csv")
head(data)

# data types
str(data)

#convert date from char into date type
data$date=as.Date(data$date, "%Y-%m-%d")

#create a seperate 
unique(data['state'])
Madhya_Pradesh =data[which (data$state == 'Madhya Pradesh'), ] 
Odisha=data[which (data$state == 'Odisha'), ] 
Telangana=data[which (data$state == 'Telangana'), ] 
Gujarat=data[which (data$state == 'Gujarat'), ] 
Delhi=data[which (data$state == 'Delhi'), ] 
Goa=data[which (data$state == 'Goa'), ] 
West_Bengal=data[which (data$state == 'West Bengal'), ] 
Tamil_Nadu=data[which (data$state == 'Tamil Nadu'), ] 
Dadra_NagarHaveli=data[which (data$state == 'Dadra & Nagar Haveli'), ] 
Daman_Diu=data[which (data$state == 'Daman & Diu'), ] 




