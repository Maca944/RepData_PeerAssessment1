wd <- "c:/maca/Rdata/RepRes1" #the directory where I unzipped the datafiles
setwd(wd) #is now my working directory

Sys.setlocale("LC_TIME", "C") # change timesettings to English, otherwise the names of the days appeared in Dutch

library(knitr) #load knitr, assuming the package is installed
library(markdown) # dito for markdown
library(plyr)
library(lubridate)
library(ggplot2)

activity <- read.csv("activity.csv") 
act_cc <- activity[complete.cases(activity),] #only use the rows without missing values

totals <- ddply(act_cc, .(date), summarize, steps=sum(steps))
hist(totals$steps)
ddply(act_cc, .(date), summarize, steps=mean(steps))
ddply(act_cc, .(date), summarize, steps=median(steps))
summary(act_cc) 

interval <- ddply(act_cc, ~interval, summarise, mean = mean(steps)) #get the averages per interval
plot(ddply(act_cc, ~interval, summarise, mean = mean(steps)), type = "l") #time series plot of 5-minute interval

interval$interval[interval$mean==max(interval$mean)] #which interval contains the max number of steps

nrow(activity) - nrow(act_cc) #the number of rows with NAs

## I want to use the average for that 5-minute interval to replace NAs 
act_all <- merge(activity, interval, by ="interval")
act_all$steps[is.na(act_all$steps)] <- act_all$mean[is.na(act_all$steps)]
act_all$mean <- NULL
#order(act_all[,3], act_all[,2])

hist(act_all$steps)
summary(act_all)

act_all$date <- as.Date(as.character(act_all$date), format='%Y-%m-%d')
act_all$weekday <- weekdays(act_all$date)
act_all$weekday[act_all$weekday == "Saturday" | act_all$weekday =="Sunday"] <- "Weekend"
act_all$weekday[act_all$weekday != "Weekend"] <- "Weekday"

weekdays <- act_all[act_all$weekday=="Weekday",]
weekends <- act_all[act_all$weekday=="Weekend",]

par(mfrow=c(2,1))
plot(ddply(weekdays, ~interval, summarise, mean = mean(steps)), type = "l")
plot(ddply(weekends, ~interval, summarise, mean = mean(steps)), type = "l")

        