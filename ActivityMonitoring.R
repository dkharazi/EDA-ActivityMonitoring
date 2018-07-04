## Import libraries
library(knitr)
library(dplyr)
library(ggplot2)

## Load the raw data
activity <- read.csv("/Users/dariuskharazi/Downloads/activity.csv", colClasses = c("numeric", "Date", "numeric"))

## Calculate the total number of steps taken per day
dailySteps <- activity %>% group_by(date) %>% summarise(steps = sum(steps))

## Create a histrogram
hist(dailySteps$steps, col = "blue", main = "Steps Taken per Day", xlab = "Steps")

## Calculate the mean and median
c(mean = mean(dailySteps$steps, na.rm = TRUE), median = median(dailySteps$steps, na.rm = TRUE))

## Calculate the average steps at each 5-minute interval
meanSteps <- activity %>% group_by(interval) %>% summarise(steps = mean(steps, na.rm = TRUE))

## Create a time series plot
plot(meanSteps$interval, meanSteps$steps, type = "l", col = "blue", lwd = 2, main = "Average Steps", xlab = "Time Interval (min.)", ylab = "Steps")

## Find the interval with the max number of steps
meanSteps[meanSteps$steps == max(meanSteps$steps) ,1]

## Calculate the total number of missing values
colSums(is.na(activity))

## Replace missing values with median
activity <- inner_join(activity, meanSteps, by = "interval")
activity$steps.x <- with(activity, ifelse(is.na(steps.x), steps.y, steps.x))
activity <- activity[,c(1:3)]
names(activity) <- c("steps", "date", "interval")

## Calculate the total number of steps taken per day
dailySteps <- activity %>% group_by(date) %>% summarise(steps = sum(steps))

## Create a histogram
hist(dailySteps$steps, col = "blue", main = "Steps Taken per Day", xlab = "Steps")

## Create a new factor variable
activity$weekday <- ifelse(weekdays(activity$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

## Calculate the average number of steps taken
meanSteps <- activity %>% group_by(weekday, interval) %>% summarise(steps = mean(steps))

## Create a panel plot
ggplot(meanSteps, aes(interval, steps)) + geom_line(color = "blue") + labs(x = "Time (min.)", y = "Steps") + facet_grid(weekday~., labeller = as_labeller(c("weekday" = "Weekday", "weekend" = "Weekend")))



