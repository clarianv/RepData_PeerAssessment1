---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---




## Loading and preprocessing the data

```r
data <- read.table(unzip("activity.zip"), header = TRUE, sep = ',')
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?
##### 1. Calculate the total number of steps taken per day

```r
stepsPerDay <- aggregate(data$steps, by=list(data$date), sum)
colnames(stepsPerDay) <- c("date","steps")
```
##### 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
hist(stepsPerDay$steps, xlab = "Steps taken each day", main = "Histogram of Steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

##### 3. Calculate and report the mean and median of the total number of steps taken per day
(Ignoring the missing values in the dataset)

```r
meanStepsPerDay <- as.integer(mean(stepsPerDay$steps, na.rm=TRUE))
medianStepsPerDay <- median(stepsPerDay$steps, na.rm=TRUE)
```
Mean of total number of steps per day is 10766, median is 10765


## What is the average daily activity pattern?
1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
library(lubridate)
stepsByInterval <- aggregate(steps ~ interval, data, mean)
names(stepsByInterval) <- c("interval", "steps")
with (stepsByInterval, plot(steps ~ interval, type = "l", main = "Avg steps by interval", ylab = "Avg steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
stepsByInterval[which.max(stepsByInterval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
length(which(is.na(data$steps)))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset -- using the mean for that 5-minute interval

```r
overallMean <- as.integer(mean(data$steps, na.rm=TRUE))
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
newData <- data
newData[is.na(newData$steps),1] <- overallMean
head(newData)
```

```
##   steps       date interval
## 1     0 2012-10-01        0
## 2     0 2012-10-01        5
## 3     0 2012-10-01       10
## 4     0 2012-10-01       15
## 5     0 2012-10-01       20
## 6     0 2012-10-01       25
```
4.Make a histogram of the total number of steps taken each day 

```r
newStepsPerDay <- aggregate(newData$steps, by=list(newData$date), sum)
colnames(newStepsPerDay) <- c("date","steps")
hist(newStepsPerDay$steps, xlab = "Steps taken each day", main = "Histogram of Steps taken each day (With Filled In Missing Values)")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

and Calculate and report the mean and median total number of steps taken per day. 

```r
newMeanStepsPerDay <- as.integer(mean(newStepsPerDay$steps, na.rm=TRUE))
newMedianStepsPerDay <- median(newStepsPerDay$steps, na.rm=TRUE)

newMeanStepsPerDay
```

```
## [1] 9354
```

```r
newMedianStepsPerDay
```

```
## [1] 10395
```
Do these values differ from the estimates from the first part of the assignment? 
Yes, a little.

```r
meanStepsPerDay - newMeanStepsPerDay
```

```
## [1] 1412
```

```r
medianStepsPerDay - newMedianStepsPerDay
```

```
## [1] 370
```



What is the impact of imputing missing data on the estimates of the total daily number of steps?
The mean much more distributed.




## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
newData$datetime <- as.POSIXct(newData$date) + minutes(newData$interval)
newData$weekday <- weekdays(newData$datetime)

weekendData <- newData[newData$weekday == 'Sunday'|newData$weekday == 'Saturday', ]
weekdayData <- newData[newData$weekday != 'Sunday'|newData$weekday != 'Saturday', ]

weekdayStepsByInterval <- aggregate(steps ~ interval, weekdayData, mean)
names(weekdayStepsByInterval) <- c("interval", "steps")

weekendStepsByInterval <- aggregate(steps ~ interval, weekendData, mean)
names(weekendStepsByInterval) <- c("interval", "steps")
```
2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



```r
par(mfcol= c(2,1))
with (weekdayStepsByInterval, plot(steps ~ interval, type = "l", main = "Weekday", ylab = "Avg steps"))
with (weekendStepsByInterval, plot(steps ~ interval, type = "l", main = "Weekend", ylab = "Avg steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
