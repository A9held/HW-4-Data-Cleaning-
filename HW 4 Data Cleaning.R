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

# parse date
weather$dateF <- mdy_hm(weather$Date)
# create a month column
weather$doy <- yday(weather$dateF)
# create a year column
weather$year <- year(weather$dateF)

# examine precipitation using a bar plot
ggplot(data=weather[weather$doy > 121 & weather$doy < 274 ,],
       aes(x=dateF,
           y=Precip))+
  geom_col(color="royalblue4")+
  theme_classic()

# add a column to weather:
weather$precip.QC <- ifelse(weather$doy >= 121 & weather$doy <= 188 & weather$year == 2021, 
                            # evaluate if the doy is between May 1 and July 7 2021
                            NA, # value if true
                            weather$Precip) # value if false: uses original precipitation observation
# Create a flag 
weather$FreezeFlag <- ifelse(weather$AirTemp <= 0, # check if at or below zero
                             1, # if true: set flag to 1
                             0) # if false: set flag to zero

#interval data: look at first 2 observations as interval
# Time 1 %--% Time 2
weather$dateF[1] %--% weather$dateF[2]

# look at the interval length from the first to the second observation:
int_length(weather$dateF[1] %--% weather$dateF[2])

# set up intervals
intervals <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1] # start date %--% end date
# interval starts with row 1, and row 2
# and ends on second to last row and final row


# calculate interval times
interval_times <- int_length(intervals)
# check interval times
intervals[interval_times != 900]

#create function for checking for irregular intervals that
# deviate from 900 seconds
# only argument is x, a vector of POSIXct formatted dates to be checked
# with an expected interval of 900 seconds
# the output is all observations that deviate from 900 seconds
timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
  
}
# run on weather data
timeCheck900(weather$dateF)


###HOMEWORK 
#Question 1
weather$Precip <- ifelse(weather$Precip <= 0 & XLevel >= 2 & YLevel >=2, 
                                        1, 
                                        0) 
weather

#38 Missing Precipitation Values
#It looks like all of the precipitation values that are printed are 1, which seems way too consistent to be true. It would be wise to cross reference this with other data and determine if the sensor was functioning properly. 


#Question 2: Create a data flag that warns a user if the battery voltage falls below 8.5 Volts
weather$BatVolt <- ifelse(weather$BatVolt <= 8.5, # check if at or below 8.5 volts
                             1, # if true: set flag to 1
                             0) # if false: set flag to zero

#Question 3 Question 3 You should also create a function that checks for observations that are in unrealistic data ranges in air temperature and solar radiation. Explain how your function works.
#-30 C to 45C

weather$AirTemp <- ifelse(weather$AirTemp >= 45 & weather$AirTemp <= -30, # check if at or above 45C or below -30C
                          1, # if true: set flag to 1
                          0) # if false: set flag to zero

weather$SolRad <- ifelse(weather$SolRad >= 1000, # check if at or above 1000w/m2
                          1, # if true: set flag to 1
                          0) # if false: set flag to zero

#Question 4: Make a plot of winter air temperatures in Jan - Mar of 2021. 

filtered_data = weather%>% 
  filter(month %in% 1:3)

ggplot(filtered_data,
       aes(x = Date, y=AirTemp, group = month))+
  geom_line()+
  labs(x= "Date", y="Air Temperatures (Degrees Celcius)")+
  theme_classic()


# Question 5 You are asked for total daily precipitation in March and April of 2021. Use a for loop to exclude (convert to NA) any days that include temperatures less than 35 degrees F on that day or the day prior to ensure that measurements are not likely to be affected by snow accumulation on the sensor.
MA = weather%>% 
  filter(month %in% 3:4)

for (Precip in MA)
  if(AirTemp <= 1.67
     1,
     0)
    print(paste(NA))
      
  
  
