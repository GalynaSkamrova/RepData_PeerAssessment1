---
title: "Reproducible Research Peer Assessment 1"
output: html_document
---

### Loading and preprocessing the data

* Loading the dataset:


```r
library(dplyr)
data <- read.csv("activity.csv")
```


### What is mean total number of steps taken per day?

* Calculating number of steps per each day:


```r
stepSum <- summarise(group_by(data,date), steps = sum(steps))
```

* Making a histogram of the total numbers of steps per day:


```r
library(ggplot2)
g <- ggplot(stepSum, aes(x = steps)) 
g <- g + geom_histogram(colour = "black", fill = "#FF0000", binwidth = 2000)
g <- g + labs(title = "Total number of steps taken each day")
g <- g + labs(x = "Number of steps per day")
g <- g + labs(y = "Frequency")
g
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

* Calculating the mean and median of the total number of steps taken per day:


```r
stepMean <- mean(stepSum$steps, na.rm = TRUE)
stepMedian <- median(stepSum$steps, na.rm = TRUE)
```

The mean and median values of the total number of steps taken per day are 1.0766189 &times; 10<sup>4</sup> and 10765 respectively

 
### What is the average daily activity pattern?
 
* Making a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days:
 

```r
intSum <- summarise(group_by(data,interval), steps = sum(steps, na.rm = TRUE))
with(intSum, plot(interval, steps,xlab = "Time interval", ylab = "Average number of steps taken, averaged across all days ", type = "l"))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
maxSteps <- max(intSum$steps)
maxStepInt <- intSum$interval[which(intSum$steps == maxSteps)]
```

5-munute interval number 835, on average across all the days in the dataset, contains the maximum number of steps (10927 steps)


### Imputing missing values

* Calculating the total number of missing values in the dataset:


```r
nas <- length(data$steps[is.na(data$steps)])
```

There are 2304 missing values in the dataset

* Creating a new dataset that is equal to the original dataset but with the missing data filled in:


```r
newdata <-  data
for (i in seq_along(newdata$steps)) {
  if (is.na(newdata$steps[i])) {
    newdata$steps[i] <- mean(newdata$steps[which(newdata$interval == newdata$interval[i])], na.rm = TRUE)
  }
}
```

* Making a histogram of the total numbers of steps per day with imputing missing data:


```r
newstepSum <- summarise(group_by(newdata,date), steps = sum(steps))
g <- ggplot(newstepSum, aes(x = steps)) 
g <- g + geom_histogram(colour = "black", fill = "#990000", binwidth = 2000)
g <- g + labs(title = "Total number of steps taken each day (with imputing missing data)")
g <- g + labs(x = "Number of steps per day")
g <- g + labs(y = "Frequency")
g
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

* Calculating the mean and median of the total number of steps taken per day with imputing missing data:


```r
newstepMean <- mean(newstepSum$steps)
newstepMedian <- median(newstepSum$steps)
```

The mean and median values of the total number of steps taken per day with imputing missing data are 1.0766189 &times; 10<sup>4</sup> and 1.0766189 &times; 10<sup>4</sup> respectively

Mean value is the same as in the previous case (with missing value). Since missing values were replaced with the means for corresponding 5-minute interval, it makes sence. Median value slightly increased and became equal to mean value.

To describe the impact of imputing missing data on the estimates of the total daily number of steps, let's compare two histograms:


```r
g <- ggplot(newstepSum, aes(x = steps)) 
g <- g + geom_histogram(colour = "black", fill = "#990000", binwidth = 2000)
g <- g + geom_histogram(data = stepSum, colour = "black", fill = "#FF0000", binwidth = 2000)
g <- g + labs(title = "Total number of steps taken each day (with and without missing data)")
g <- g + labs(x = "Number of steps per day")
g <- g + labs(y = "Frequency")
g
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 


### Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day:


```r
data <- mutate(data,wday = factor(1 * (weekdays(as.Date(date))  %in% c('Saturday','Sunday')), labels = c("weekday", "weekend")))
```

* Making a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days:


```r
intSumWdays <- filter(data, wday == "weekday")
intSumWends <- filter(data, wday == "weekend")
intSumWeekdays <- summarise(group_by(intSumWdays,interval), steps = sum(steps, na.rm = TRUE))
intSumWeekends <- summarise(group_by(intSumWends,interval), steps = sum(steps, na.rm = TRUE))
par(mfrow = c(2,1), mar = c(4,4,2,1))
with(intSumWeekends, plot(interval, steps, main = "Weekend", xlab = "Time interval", ylab = "Number of steps", type = "l"))
with(intSumWeekdays, plot(interval, steps, main = "Weekday", xlab = "Time interval", ylab = "Number of steps", type = "l"))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 
