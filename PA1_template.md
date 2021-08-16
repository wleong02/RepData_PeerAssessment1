---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

First, we will load the data and preview it.

```r
options("scipen" = 10)
unzip('activity.zip')
data <- read.csv('activity.csv')
str(data[1:5,])
```

```
## 'data.frame':	5 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20
```
Next, we will transform the data variable into the appropriate format.

```r
data$date <- as.Date(data$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?


```r
# Calculate the mean no. of steps per day
totalsteps <- aggregate(steps ~ date, data = data, sum)

# Calculate median across the means
mediansteps <- round(median(totalsteps$steps),0)

# Calculate the mean across the means
meansteps <- round(mean(totalsteps$steps),0)


# Plot histogram and overlay the mean/median
library(ggplot2)
ggplot(totalsteps, aes(x=steps)) +
  geom_histogram(binwidth = 1000, color = "darkblue", fill = "lightblue") +
  geom_vline(xintercept = meansteps, col = "black") +
  geom_text(aes(x=meansteps, label = "Mean/Median", y = 8), angle = 90, vjust = 1.2) +
  ggtitle("Figure 1: Distribution of total number of steps taken each day") +
  labs(x = "Total number of steps taken per day", y = "Frequency")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)
The mean number of steps per day is 10766 and the median number of steps per day is 10765.

## What is the average daily activity pattern?

```r
# Calculate the mean number of steps for each interval
avg <- aggregate(steps ~ interval, data = data, FUN = mean)

# Identify the maximum point
maxpoint <- avg[which.max(avg$steps),]
  
# Plot the time series
ggplot(avg, aes(x=interval, y=steps)) +
  geom_line() +
  geom_point(data = maxpoint, aes(x=interval, y=steps), color = "red", size = 3) +
  geom_text(data = maxpoint, aes(x=interval, y = steps, label = paste0("Maximum (",interval,",",round(steps,0),")"))) +
  ggtitle("Figure 2: Average daily activity across time intervals") +
  labs(x = "Interval", y = "Average daily activity (no. of steps)")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)
The maximum number of steps is contained within the 835 interval.

## Imputing missing values

We will inspect the number of NAs for each variable.

```r
na <- with(data, sapply(data, function(x) {mean(is.na(x))}))
na
```

```
##     steps      date  interval 
## 0.1311475 0.0000000 0.0000000
```
About 13% of the data are missing values. We will proceed to impute them by taking the mean for that 5-minute interval, which we had earlier computed.


```r
library(dplyr)

# Replace missing data with mean of that interval
imputeddata <- data %>%
                mutate(steps = ifelse(is.na(steps), avg[avg$interval == interval,]$steps, steps))
```

Now we update the histogram with the imputed data.

```r
# Re-calculate the mean number of steps per day
totalsteps2 <- aggregate(steps ~ date, data = imputeddata, sum)

# Re-calculate the median of the means
mediansteps2 <- round(median(totalsteps2$steps),0)

# Re-calculate the mean of the means
meansteps2 <- round(mean(totalsteps2$steps),0)

# Plot histogram and overlay with the mean/median
ggplot(totalsteps2, aes(x=steps)) +
  geom_histogram(binwidth = 1000, color = "darkblue", fill = "lightblue") +
  geom_vline(xintercept = meansteps2, col = "black") +
  geom_text(aes(x=meansteps2, label = "Mean/Median", y = 8), angle = 90, vjust = 1.2) +
  ggtitle("Figure 3: Distribution of total number of steps taken each day") +
  labs(x = "Total number of steps taken per day", y = "Frequency")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)
The updated mean number of steps per day is 10766 and the median number of steps per day is 10766, which remains generally unchanged as our imputation method applies the mean value and therefore preserves the overall mean.

## Are there differences in activity patterns between weekdays and weekends?


```r
# Create factor variable indicating whether the day is a weekday or weekend
imputeddata$dayofweek <- weekdays(imputeddata$date)
imputeddata$weekdayindicator <- as.factor(ifelse(imputeddata$dayofweek %in% c("Saturday", "Sunday"), "weekend", "weekday"))

# Calculate average steps by interval and weekday/weekend
avgbydayofweek <- aggregate(steps ~ interval + weekdayindicator, data = imputeddata, mean)

# Plot time series
ggplot(avgbydayofweek, aes(x=interval, y=steps)) +
  geom_line() +
  facet_grid(weekdayindicator~.) +
  ggtitle("Figure 4: Average daily activity across time intervals") +
  labs(x = "Interval", y = "Average daily activity (no. of steps)")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)
There is a higher peak in activity during the weekdays than weekends. However, during the weekends, activity seems to be generally higher across the day.
