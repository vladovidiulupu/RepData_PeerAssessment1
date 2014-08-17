# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

This script assumes that the "repdata-data-activity.zip" file is present in the working directory.


```r
library(ggplot2)
options(scipen = 6)

if (!file.exists("activity.csv"))
        unzip("repdata-data-activity.zip")

data <- read.csv("activity.csv")
totalStepsPerDay <- aggregate(data$steps, by=list(data$date), FUN=sum)
totalStepsPerDay <- totalStepsPerDay[complete.cases(totalStepsPerDay),]
names(totalStepsPerDay) <- c("date", "steps")
```

## What is mean total number of steps taken per day?

Histogram of the total number of steps taken each day:


```r
hist(totalStepsPerDay$steps, breaks=20, col="palegreen3",
     main="Histogram of steps taken each day", 
     xlab="Steps taken per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

Computing the mean and median:


```r
totalStepsPerDayMean <- round(mean(totalStepsPerDay$steps))
```

The mean total number of steps taken per day is 10766.


```r
totalStepsPerDayMedian <- median(totalStepsPerDay$steps)
```

The median total number of steps taken per day is 10765.


## What is the average daily activity pattern?

Computing the average daily pattern:


```r
averageStepsByInterval <- tapply(data$steps, data$interval, mean, na.rm=T)
intervals <- data$interval[data$date == "2012-10-01"]
averageDailyPattern <- data.frame(intervals, averageStepsByInterval)
names(averageDailyPattern) <- c("interval", "steps")
```

Time series plot:


```r
ggplot(averageDailyPattern, aes(interval, steps)) + 
    geom_line(colour="blue") +
    xlab("Interval") + 
    ylab("Average number of steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 



```r
maxStepsInterval <- averageDailyPattern$interval[which.max(averageDailyPattern$steps)]
```

The interval with the maximum number of steps is 835.

## Imputing missing values


```r
rowsWithNA <- sum(!complete.cases(data))
```

The number of rows with NAs is 2304.

To fill the missing values in the dataset, I will use the means for each of the 5-minute
intervals:


```r
data.imputed <- data
for (interval in intervals) {
    #get the average number of steps for the current interval
    averageSteps <- 
        round(averageDailyPattern[averageDailyPattern$interval==interval,]$steps)
    #fill in the missing data where NA is present
    incompleteRows <- data$interval == interval & is.na(data$steps)
    data.imputed[incompleteRows,]$steps <- averageSteps
}

totalStepsPerDay.imputed <- 
    aggregate(data.imputed$steps, by=list(data.imputed$date), FUN=sum)
names(totalStepsPerDay.imputed) <- c("date", "steps")
```

Histogram of the total number of steps taken each day:


```r
hist(totalStepsPerDay.imputed$steps, breaks=20, col="palegreen4",
     main="Histogram of steps taken each day", 
     xlab="Steps taken per day")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

Computing the mean and median for the imputed dataset:


```r
totalStepsPerDayMean.imputed <- round(mean(totalStepsPerDay.imputed$steps))
```

The mean total number of steps taken per day is 10766.


```r
totalStepsPerDayMedian.imputed <- median(totalStepsPerDay.imputed$steps)
```

The median total number of steps taken per day is 10762.

Because all the NAs were replaced with average values, the number of days with the mean number of steps was drastically increased. The average dominates in the histogram and the mean and median are close.

## Are there differences in activity patterns between weekdays and weekends?

Creating the weekday factor and adding it to the imputed data frame:


```r
dateToFactor <- function(day) {
    if (day == 0 || day == 6)
        "weekend"
    else
        "weekday"
}
weekdays <- sapply(as.POSIXlt(data.imputed$date)$wday, dateToFactor)
weekdaysFactor <- as.factor(weekdays)
data.imputed <- cbind(data.imputed, weekdaysFactor)
names(data.imputed) <- c("steps", "date", "interval", "weekday")

dailyPatternByWeekday <- 
    aggregate(steps ~ interval + weekday, data=data.imputed, mean)
```

Plotting the average number of steps for each interval for weekday days and
weekend days:


```r
ggplot(dailyPatternByWeekday, aes(interval, steps)) + 
    facet_grid(. ~ weekday) +
    geom_line(colour="blue") +
    xlab("Interval") + 
    ylab("Average number of steps")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 

We can see that the weekdays have a period in which the average number of steps taken is very high, which could be the morning. The mean number of steps per interval is more evenly distributed in the weekend. In the weekend, activities start and end later than in weekdays.

These results may not be precise because I have used a simple method for replacing NAs that used the average for all days and did not take into account this difference of patterns. 
