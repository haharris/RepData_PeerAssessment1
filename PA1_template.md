---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
First, the data gets loaded and assigned to a variable.  

```r
activity_data <- read.csv("activity.csv")
```

Next, the values in the date column are converted from character type into date type.  

```r
activity_data$date <- as.Date(activity_data$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?  
A histogram of the total steps taken each day helps to understand the data before making calculations.  

```r
totalSteps <- aggregate(activity_data$steps, by=list(activity_data$date), sum, na.rm=TRUE)

names(totalSteps) <- c("date", "steps")

hist(totalSteps$steps, xlab = "Total steps per day", ylab = "Count", main = "Total number of steps each day (NA's removed)", col = 4)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
meanSteps <- mean(totalSteps$steps, na.rm = TRUE)

medianSteps <- median(totalSteps$steps, na.rm = TRUE)
```
The mean number of steps per day is 9354.2295082 and the median is 10395.  


## What is the average daily activity pattern?
The first step in answering this question is to make a time series plot of the 5-minuite interval and the average steps taken across all days.  

```r
meanStepsbyInterval <- aggregate(activity_data$steps, by=list(activity_data$interval), mean, na.rm=TRUE)

names(meanStepsbyInterval) <- c("interval", "steps")

plot(meanStepsbyInterval$interval, meanStepsbyInterval$steps, type = "l", xlab = "Interval", ylab = "Average steps taken", main = "Average steps per 5 minute interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
maxInterval <- meanStepsbyInterval$interval[which.max(meanStepsbyInterval$steps)]
```
Now, a calculation can be run to determine which 5 minute interval, on average across all days, contains the maximum number of steps.     

So, the most steps on average occur 835.  

## Imputing missing values
Before changing any missing values, first the total number of missing values should be found.  

```r
missing <- nrow(activity_data[!complete.cases(activity_data),])
```
It looks like there are 2304 rows with NA values.  

Now, we will fill in those missing values with the mean value for that day.  

```r
completeActivityData <- activity_data
for (i in 1:nrow(completeActivityData)){
  if (is.na(completeActivityData$steps[i])) {
    idx <- which(completeActivityData$interval[i] == meanStepsbyInterval$interval)
    completeActivityData$steps[i] <- meanStepsbyInterval[idx,]$steps
  }
}
```
Now that we have a complete data set, we can create a histogram of total steps taken each day.  

```r
completeStepsperDay <- aggregate(completeActivityData$steps, by=list(completeActivityData$date), mean)

names(completeStepsperDay) <- c("interval", "steps")

hist(completeStepsperDay$steps, xlab = "Total steps per day", ylab = "Count", main = "Total steps per day (NA's filled in)", col = 4)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
completemeanSteps <- mean(completeStepsperDay$steps)

completemedianSteps <- median(completeStepsperDay$steps)
```
Using the corrected data set, the mean steps per day is 37.3825996 and the median is 37.3825996.  

  
## Are there differences in activity patterns between weekdays and weekends?
To answer this question, first a new factor variable needs to be created with two levels: "weekday" and "weekend".  

```r
completeActivityData$day <- weekdays(completeActivityData$date)

completeActivityData$daytype <- "weekday"

completeActivityData$daytype[completeActivityData$day %in% c("Saturday", "Sunday")] <- "weekend"
```
Finally, we will create an panel plot comparing weekday and weekend average steps taken of the 5 minuite intervals.  

```r
intervalAveragebyDay <- aggregate(completeActivityData$steps, by=list(completeActivityData$daytype, completeActivityData$interval), mean)

names(intervalAveragebyDay) <- c("daytype", "interval", "steps")

library(ggplot2)

qplot(interval, steps, data=intervalAveragebyDay, type="l",
      geom="line",
      xlab = "Interval",
      ylab = "Average number of steps",
      main = "Average steps taken Weekdays Vs Weekends",
      facets = daytype ~ .)
```

```
## Warning: Ignoring unknown parameters: type
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

The main visible difference between weekends and weekdays seems to be the higher spike in activity in the morning on weekdays.  
