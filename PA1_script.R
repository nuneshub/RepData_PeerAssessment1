################################################################################
# Reproducible Research:
# Peer Assessment 1
################################################################################
### Loading and preprocessing the data

# Show any code that is needed to
# 1. Load the data (i.e. read.csv())
# 2. Process/transform the data (if necessary) into a format suitable for your 
#    analysis.

zipped <- file.path(getwd(), "activity.zip")
unzipped <- file.path(getwd(), "activity.csv")
if (!file.exists(unzipped)) { unzip(zipped) }

activity <- read.csv(file=unzipped,na.strings="NA",stringsAsFactors=FALSE)

# Preprocessing the data

hour <- activity$interval %/% 100
min <- activity$interval - hour * 100
datetime <- paste(activity$date, hour, min, sep=" ")

activity$datetime <- as.POSIXct(strptime(datetime,format="%Y-%m-%d %H %M"))
activity$date <- as.Date(activity$date,format="%Y-%m-%d")

################################################################################
### What is mean total number of steps taken per day?

# For this part of the assignment, you can ignore the missing values in the dataset.
# 1. Make a histogram of the total number of steps taken each day
# 2. Calculate and report the mean and median total number of steps taken per day

stepsDay <- aggregate(steps ~ date, data=activity, FUN=sum, na.action=na.omit)

library(ggplot2)
gHist <- ggplot(data=stepsDay, aes(x=stepsDay$steps)) +
        geom_histogram(colour="black",fill="orange",binwidth=500) +
        labs(title="Histogram") +
        xlab("Total number of steps per day") +
        ylab("Frequency") +
        theme_bw()
print(gHist)

stepsDayMean <- format(mean(stepsDay$steps), scientific=FALSE)
stepsDayMedian <- format(median(stepsDay$steps), scientific=FALSE)

################################################################################
### What is the average daily activity pattern?

# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all days (y-axis)
# 2. Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?

# Extract time from datetime column and create time column (with present date)

activity$time <- strftime(activity$datetime, format="%H:%M:%S")
activity$time <- as.POSIXct(activity$time, format="%H:%M:%S")

# Time series plot

stepsTime <- aggregate(steps ~ time, data=activity, FUN=mean, na.action=na.omit)

library(scales)
gTs <- ggplot(data=stepsTime, aes(x=time, y=steps)) +
        geom_line(colour="red",size=1.05) + 
        labs(title="Time Series Plot") +
        xlab("Time (5-minute interval)") +
        ylab("Average number of steps (averaged across all days)") +
        scale_x_datetime(labels=date_format("%H:%M"), breaks="2 hour") +
        theme_bw()
print(gTs)

# 5-minute interval, on average across all the days in the dataset, that
# contains the maximum number of steps (interval: interval1 - interval2)

maxStepsIdx <- which.max(stepsTime$steps)
interval1 <- stepsTime[maxStepsIdx, "time"]
interval1 <- format(interval1, format="%H:%M")
interval2 <- stepsTime[maxStepsIdx+1, "time"]
interval2 <- format(interval2, format="%H:%M")

################################################################################
### Imputing missing values

# Note that there are a number of days/intervals where there are missing values
# (coded as NA). The presence of missing days may introduce bias into some 
# calculations or summaries of the data.
# 1. Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)
# 2. Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use the 
# mean/median for that day, or the mean for that 5-minute interval, etc.
# 3. Create a new dataset that is equal to the original dataset but with the 
# missing data filled in.
# 4. Make a histogram of the total number of steps taken each day and Calculate 
# and report the mean and median total number of steps taken per day. Do these 
# values differ from the estimates from the first part of the assignment? What 
# is the impact of imputing missing data on the estimates of the total daily 
# number of steps?

# Total number of missing values

Nas <- sum(is.na(activity$steps))

# Filling in all of the missing values
# Strategy to input missing values: mean for that 5-minute interval

# New dataset

activityNew <- activity
activityNew$steps[is.na(activityNew$steps)] <- 
        round(tapply(activity$steps,activity$interval,FUN=mean,na.rm=TRUE),
              digits=0)

# Histogram of the total number of steps taken each day

stepsDayNew <- aggregate(steps ~ date, data=activityNew, FUN=sum)

gHisNew <- ggplot(data=stepsDayNew, aes(x=stepsDayNew$steps)) + 
        geom_histogram(colour="black",fill="orange",binwidth=500) +
        labs(title="Histogram (missing values imputed)") +
        xlab("Total number of steps per day") +
        ylab("Frequency") +
        theme_bw()
print(gHisNew)

# mean and median total number of steps taken per day

stepsDayMeanNew <- format(mean(stepsDayNew$steps), scientific=FALSE)
stepsDayMedianNew <- format(median(stepsDayNew$steps), scientific=FALSE)

# Do these values differ from the estimates from the first part of the assignment?
# What is the impact of imputing missing data on the estimates of the total daily 
# number of steps?

# They differ from the estimates from the first part of the assignment, but not 
# significantly. In fact, the differences are very small.
# This should be due to the fact that the missing values correspond to complete 
# days where no information was provided. Those days (eight in total) were 
# accross the two-months period, in October (8-Oct and 10-Oct) and in November 
# (1-Nov, 4, 9, 10, 14 and 30-Nov).
# The simplistic strategy used of filling in all of the missing values with the 
# mean for the corresponding 5-minute interval, resulted in more 8 days going 
# directly to the bar in the histogram that corresponds to the mean (from a 
# frequency of 3 in the original data to a frequancy of 11 in the new data). 
# This does not affect significantly the values of the mean or the median.

################################################################################
### Are there differences in activity patterns between weekdays and weekends?
        
# For this part the weekdays() function may be of some help here. Use the 
# dataset with the filled-in missing values for this part.
# 1. Create a new factor variable in the dataset with two levels - "weekday" and
#    "weekend" indicating whether a given date is a weekday or weekend day.
# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 
# 5-minute interval (x-axis) and the average number of steps taken, averaged 
# across all weekday days or weekend days (y-axis). See the README file in the 
# GitHub repository to see an example of what this plot should look like using 
# simulated data.

# New factor variable with two levels - "weekday" and "weekend" 

dayType <- function(x) {
        ifelse(weekdays(x) %in% c("Saturday", "Sunday"), "weekend", "weekday")
}

activityNew$dayType <- as.factor(sapply(activityNew$date, dayType))

# Time series plot (panel)

panelDat <- aggregate(steps ~ time + dayType, data=activityNew, FUN=mean)

gPanel <- ggplot(data=panelDat, aes(x=time, y=steps)) +
        geom_line(colour="red",size=1.05) + 
        facet_grid(dayType ~ .) +
        labs(title="Time Series Plot") +
        xlab("Time (5-minute interval)") +
        ylab("Average number of steps") +
        scale_x_datetime(labels=date_format("%H:%M"), breaks="2 hour") +
        theme_bw()
print(gPanel)



