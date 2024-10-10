install.packages(c("lubridate","dplyr","ggplot2"))
library(lubridate)
library(dplyr)
library(ggplot2)

weather <- read.csv("/cloud/project/campus_weather.csv",
                    na.strings= "#N/A")

weather$datef = mdy_hm(weather$Date)

interval = weather$datef[-length(weather$datef)] %--% weather$datef[1]
interval

#set up time intervals in a vector of dates
timeInterval = function(x){
  x[-length(x)] %--%x[-1]
}

timeInterval(weather$datef)

#Curly brackets mean creating a new function or for loop
#could use 1:6 instead of seqex
seqex = c(1,4,6)

for(i in seqex){
  print(paste("example", i))
}

chEx = character()
for(i in 1:6){
  chEx[i] = paste("example", i)
}
chEx

numEx = numeric()
for(i in 1:6){
  numEx[i]= 6*i
  print(numEx[i])
}

numEx2 = numeric()
for(i in 2:6){
  numEx2[i]= 6*i
}
numEx2

#Calculate a rolling average of air temperatures over eight 15 min measurements (2 hours) for January of 2022 using a for loop. Make a plot of the 15 minute air temperature and the rolling average.

#Start by extracting just January using lubridate functions from past tutorials (January is consistent with no issues)
#Isolate Jan 2022
weather$month = month(weather$datef)
weather$year = year(weather$datef)

Jan22 = weather%>%
  filter(month == 1 & year == 2022)

rollAveTemp = numeric()
mean(Jan22$AirTemp[1:8])
for(i in 8:nrow(Jan22)){
  rollAveTemp[i] = mean(Jan22$AirTemp[(i-7):i])
}

Jan22$rollAveTemp = rollAveTemp
View(Jan22)
