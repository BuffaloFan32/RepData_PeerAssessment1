


#Clear variables
rm(list = ls())

#Reference libraries


 
#Download and unzip files
urll<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destfile<-paste0(getwd(),"/","dataweek2.zip")
download.file(urll,destfile)
unzip("dataweek2.zip",exdir = getwd(), list = FALSE, overwrite = TRUE)
path<-"./"

#Read the activities
activity<-read.csv(paste0(path, "activity.csv"), header=TRUE)
str(activity)

#Histogram, median and mean of total daily steps
activityAggDate<-aggregate(. ~ date, data = activity, FUN=sum, na.rm=TRUE)
hist(activityAggDate$steps)
median(activityAggDate$steps, na.rm=TRUE)
mean(activityAggDate$steps, na.rm=TRUE)

#Data by interval
activityAggInt<-aggregate(. ~ interval, data = activity, FUN=mean, na.rm=TRUE)
plot(x=activityAggInt$interval, y=activityAggInt$steps, type="l")
maxSteps<-max(activityAggInt$steps)
pos<-which(activityAggInt$steps==maxSteps)
activityAggInt$interval[pos]

#Dealing with NAs
sum(is.na(activity$steps))
#Fill in NAs with average for each interval
activityNA<-activity
pos<-match(activityNA$interval[is.na(activityNA$steps)],activityAggInt$interval)
activityNA$steps[is.na(activityNA$steps)]<-activityAggInt$steps[pos]
sum(is.na(activityNA$steps))
activityAggDateNA<-aggregate(. ~ date, data = activityNA, FUN=sum, na.rm=TRUE)
hist(activityAggDateNA$steps)
median(activityAggDateNA$steps, na.rm=TRUE)
mean(activityAggDateNA$steps, na.rm=TRUE)

#Add weekdays to the data
weekday<-weekdays(as.Date(activity$date))
weekday[which(weekday=="Saturday")]<-"Weekend"
weekday[which(weekday=="Sunday")]<-"Weekend"
weekday[which(weekday!="Weekend")]<-"Weekday"
activity<-cbind(activity, weekday)
par(mfrow=c(2,1))
activityAggWD<-aggregate(. ~ interval, data = activity, FUN=mean, na.rm=TRUE)




