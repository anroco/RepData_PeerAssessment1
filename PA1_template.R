#Read file activity.csv
activity_data <- read.csv(unz("activity.zip", "activity.csv"), nrows = 17568,
                            colClasses = c("numeric", "Date", "numeric"))

#total number of steps taken per day.
steps_day <- tapply(activity_data$steps, activity_data$date, sum, na.rm = T)

#Create a histogram of the total number of steps taken each day.
hist(steps_day,  xlab="total number of steps taken each day", col="red", 
                            main="Histogram number of steps by day")

#Calculate the mean and median of the total number of steps taken per day
mean(steps_day, na.rm=TRUE)
median(steps_day, na.rm=TRUE)

#Calculate the average number of steps taken by 5-minute interval across all days.
steps_interval <- tapply(activity_data$steps, activity_data$interval, mean, na.rm = T)

#Plot of Mean Steps vs Intervals
plot(names(steps_interval), steps_interval, type= "l", ylab="mean steps", 
     xlab= "5-minute intervals", main="Mean Steps vs 5-minute intervals")

#Obtain the 5-minute interval containing the maximum number of steps
max_interval <- which.max(steps_interval)
names(max_interval)

#the total number of missing values in activity_data
sum(is.na(activity_data$steps))

#Filling in all of the missing values with the mean for that 5-minute interval specific.
data_fill_NA <- activity_data
for(i in 1:nrow(data_fill_NA)){
    if(is.na(data_fill_NA$steps[i]))
        data_fill_NA$steps[i]<- steps_interval[as.character(data_fill_NA$interval[i])]
}

#Calculate and report the mean and median total number of steps taken per day 
#with respect to the new dataset obtained in the previous step.
steps_day_2 <- tapply(data_fill_NA$steps, data_fill_NA$date, sum, na.rm = T)
hist(steps_day_2,  xlab="total number of steps taken each day", col="red", 
     main="Histogram number of steps by day")
mean(steps_day_2, na.rm=TRUE)
median(steps_day_2, na.rm=TRUE)

#Define whether a given date is a weekday or weekend day.
data_fill_NA$day <- sapply(data_fill_NA$date, 
                           function(date){
                                day <- weekdays(date)
                                if (day %in% c("Monday", "Tuesday", "Wednesday",
                                               "Thursday", "Friday"))
                                    return("weekday")
                                else (day %in% c("Saturday", "Sunday"))
                                    return("weekend")
                            })

#plot of the 5-minute interval vs the average number of steps taken, across all 
#weekday days or weekend days.
averages <- aggregate(steps ~ interval + day, data_fill_NA, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("number of steps")
