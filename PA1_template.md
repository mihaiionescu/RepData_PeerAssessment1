---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

**Note:** we assume that the activity.csv file is in the working directory.

Loading the data in a varable: 

```r
activity_data <- read.csv("activity.csv")
```

Formating the dates: 

```r
activity_data$date <- as.Date(activity_data$date)
```

## What is mean total number of steps taken per day?

#### Ignoring the missing values in the dataset

```r
x <- complete.cases(activity_data)
existing_activity<- activity_data[x,]
```

### Make a histogram of the total number of steps taken each day

```r
activity_per_day <- aggregate(existing_activity[,1],list(date= existing_activity$date),sum)
hist(activity_per_day$x, xlab="Steps", main="Steps Taken Each Day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

### Calculate and report the **mean** and **median** total number of steps taken per day
Calculate mean:

```r
total_steps_mean <- mean(activity_per_day$x,na.rm=TRUE)
total_steps_mean
```

```
## [1] 10766.19
```


Calculate mean:

```r
total_steps_median <- median(activity_per_day$x, na.rm=TRUE)
total_steps_median
```

```
## [1] 10765
```

## What is the average daily activity pattern?

### Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
agg_int <-  aggregate(existing_activity[,1],list(interval= existing_activity$interval),mean)
with(agg_int,plot(interval,x,type="l", ylab="daily average for interval"))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_step_interval <- agg_int[agg_int$x== max(agg_int[,2]),1]
max_step_interval
```

```
## [1] 835
```

## Imputing missing values

### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NAs`)

```r
empty_rows <- nrow(activity_data[!x,])
empty_rows
```

```
## [1] 2304
```

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
extended_activity_data <- merge(activity_data, agg_int)
extended_activity_data[!complete.cases(extended_activity_data),2]<- extended_activity_data[!complete.cases(extended_activity_data),4]
head(extended_activity_data, 10)
```

```
##    interval    steps       date        x
## 1         0 1.716981 2012-10-01 1.716981
## 2         0 0.000000 2012-11-23 1.716981
## 3         0 0.000000 2012-10-28 1.716981
## 4         0 0.000000 2012-11-06 1.716981
## 5         0 0.000000 2012-11-24 1.716981
## 6         0 0.000000 2012-11-15 1.716981
## 7         0 0.000000 2012-10-20 1.716981
## 8         0 0.000000 2012-11-16 1.716981
## 9         0 0.000000 2012-11-07 1.716981
## 10        0 0.000000 2012-11-25 1.716981
```

### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
new_activity_data <- extended_activity_data[,1:3]
head(new_activity_data, 10)
```

```
##    interval    steps       date
## 1         0 1.716981 2012-10-01
## 2         0 0.000000 2012-11-23
## 3         0 0.000000 2012-10-28
## 4         0 0.000000 2012-11-06
## 5         0 0.000000 2012-11-24
## 6         0 0.000000 2012-11-15
## 7         0 0.000000 2012-10-20
## 8         0 0.000000 2012-11-16
## 9         0 0.000000 2012-11-07
## 10        0 0.000000 2012-11-25
```

### Make a histogram of the total number of steps taken each day. 

```r
new_activity_per_day <- aggregate(new_activity_data[,2],list(date= new_activity_data$date),sum)
hist(new_activity_per_day$x, xlab="total steps per day", main="Steps per day for New Activity Data")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

### Calculate and report the mean and median total number of steps taken per day.

```r
new_total_steps_mean <-  mean(new_activity_per_day$x,na.rm=TRUE)
new_total_steps_mean
```

```
## [1] 10766.19
```

```r
new_total_steps_median <- median(new_activity_per_day$x, na.rm=TRUE)
new_total_steps_median
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
set_day_type <- function(x) {

    if (weekdays(as.Date(x)) == "Sunday" | weekdays(as.Date(x)) == "Saturday") 
       day_type <- "weekend"
    else
        day_type <- "weekday"

    day_type    
}
new_activity_data$day_type <- as.factor(sapply(new_activity_per_day[,1],set_day_type))
head(new_activity_data, 10)
```

```
##    interval    steps       date day_type
## 1         0 1.716981 2012-10-01  weekday
## 2         0 0.000000 2012-11-23  weekday
## 3         0 0.000000 2012-10-28  weekday
## 4         0 0.000000 2012-11-06  weekday
## 5         0 0.000000 2012-11-24  weekday
## 6         0 0.000000 2012-11-15  weekend
## 7         0 0.000000 2012-10-20  weekend
## 8         0 0.000000 2012-11-16  weekday
## 9         0 0.000000 2012-11-07  weekday
## 10        0 0.000000 2012-11-25  weekday
```

### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
par(mfrow = c(2, 1))

wkday <-  aggregate(steps~interval,new_activity_data,mean, subset= new_activity_data$wkday=="weekday")
```

```
## Error in aggregate.data.frame(mf[1L], mf[-1L], FUN = FUN, ...): no rows to aggregate
```

```r
with(wkday,plot(interval,steps,type="l", main="weekday average for 5 min interval"))

wkend <-  aggregate(steps~interval,new_activity_data,mean, subset= new_activity_data$wkday=="weekend")
```

```
## Error in aggregate.data.frame(mf[1L], mf[-1L], FUN = FUN, ...): no rows to aggregate
```

```r
with(wkend,plot(interval,steps,type="l", main="weekend average for 5 min interval"))
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 
