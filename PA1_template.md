# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
activity <- read.csv("activity.csv", stringsAsFactors = F)
### After loading data we can check the name of the variables and number of observations by using names and str function 
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(activity, 10)
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
```

```r
###  Process/transform the data (if necessary) 
### Since there are  lot of data missing, we can subset data frame to values without na for later use
complete_days_only <- activity[complete.cases(activity),]
### What is mean total number of steps taken per day?
### For this part, you can ignore the missing values in the dataset.
### Make a histogram of the (total number of (steps taken each day))
### (total number of (steps taken per day))
total <- aggregate(steps ~ date, complete_days_only, sum)

### add descriptive variable names
names(total)[2] <- "sum_steps"
### check out new data frame
head(total, 5)
```

```
##         date sum_steps
## 1 2012-10-02       126
## 2 2012-10-03     11352
## 3 2012-10-04     12116
## 4 2012-10-05     13294
## 5 2012-10-06     15420
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(lattice)
ggplot(total, aes(sum_steps)) + geom_histogram(breaks=seq(0,24000, by = 3000), col="white",
fill="blue",) +
 labs(title="Histogram: Steps taken per day", x="Total number of steps per day", y= "Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->




## What is mean total number of steps taken per day?


```r
### mean
mean(total$sum_steps)
```

```
## [1] 10766.19
```

```r
### median
median(total$sum_steps)
```

```
## [1] 10765
```

```r
### Average daily activity pattern
### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
### the average number of steps taken, averaged across all days for each 5-minute
### interval

### What is the average daily activity pattern?
### Average daily activity pattern
### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
### the average number of steps taken, averaged across all days for each 5-minute
### interval
interval <- aggregate(steps ~ interval, complete_days_only, mean)

### add descriptive variable names
names(interval)[2] <- "mean_steps"

### check out new data frame
head(interval, 5)
```

```
##   interval mean_steps
## 1        0  1.7169811
## 2        5  0.3396226
## 3       10  0.1320755
## 4       15  0.1509434
## 5       20  0.0754717
```

```r
###   interval mean_steps

## format plot margins to accommodate long text labels.
par(mai = c(1.2,1.5,1,1))

### plot time series
plot(
    x = interval$interval,
    y = interval$mean_steps,
    type = "l",
    main = "Time Series Plot of the 5-Minute Interval\n and the Average Number of Steps Taken\n Averaged Across All Days",
    xlab = "5-Minute Interval",
          ylab = "Average Number of Steps Taken,\n Averaged Across All Days"
)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
interval[interval$mean_steps==max(interval$mean_steps),]
```

```
##     interval mean_steps
## 104      835   206.1698
```

## What is the average daily activity pattern?

```r
x  <- complete_days_only %>% 
    group_by(interval) %>% 
    summarise(avg_interval = mean(steps))
##


## which interval had the highest number of average steps? 
x[which.max(x$avg_interval), ]
```

```
## # A tibble: 1 × 2
##   interval avg_interval
##      <int>        <dbl>
## 1      835     206.1698
```
## Imputing missing values

```r
### Calculate and report the total number of missing values (coded as NA)
nrow(activity[is.na(activity$steps),])
```

```
## [1] 2304
```

```r
### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
###We will use the mean for the 5-minute interval to populate NA values for a given interval.
### Create a new dataset that is equal to the original dataset but with the missing data filled in.
### merge original activity data frame with interval data frame
newactivity <- merge(activity, interval, by = 'interval', all.y = F)

### merge NA values with averages rounding up for integers
newactivity$steps[is.na(newactivity$steps)] <- as.integer(
    round(newactivity$mean_steps[is.na(newactivity$steps)]))

### drop and reorder columns to match original activity data frame

keeps <- names(activity)
newactivity <- newactivity[keeps]
### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
### (total number of (steps taken per day))
newtotal <- aggregate(steps ~ date, newactivity, sum)

### add descriptive variable names
names(newtotal)[2] <- "sum_steps"

### check out new data frame
head(newtotal, 5)
```

```
##         date sum_steps
## 1 2012-10-01     10762
## 2 2012-10-02       126
## 3 2012-10-03     11352
## 4 2012-10-04     12116
## 5 2012-10-05     13294
```

```r
### plot histogram, using breaks purely for better visuals.
hist(
    newtotal$sum_steps,
    col = "blue",
    main = "Histogram of the (Total Number of (Steps Taken Each Day))",
    xlab = "(Total Number of (Steps Taken Each Day))",
    breaks = 20
)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
### mean
mean(newtotal$sum_steps)
```

```
## [1] 10765.64
```

```r
### median
median(newtotal$sum_steps)
```

```
## [1] 10762
```


## Are there differences in activity patterns between weekdays and weekends?


```r
###They do differ, but ever so slightly.
###mean(total) = 10766.19, while mean(newtotal) = 10765.64. Rounding produces the same value.
###median(total) = 10765, while median(newtotal) = 10762. 3 step difference.
###What is the impact of imputing missing data on the estimates of the total daily number of steps?
###This seems to highly depend on how you impute the missing data. Since I used the average for #a given interval, there was practically no difference because we basically pulled the averages #closer to the inserted average value.
###Are there differences in activity patterns between weekdays and weekends?
### Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
### create new data frame
newnewactivity <- newactivity

### set up logical/test vector

weekend <- weekdays(as.Date(newnewactivity$date)) %in% c("Saturday", "Sunday")

### Fill in weekday column

newnewactivity$daytype <- "weekday"

### replace "weekday" with "weekend" where day == Sat/Sun
newnewactivity$daytype[weekend == TRUE] <- "weekend"

### convert new character column to factor
newnewactivity$daytype <- as.factor(newnewactivity$daytype)

### Check out new data frame
str(newnewactivity)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : int  2 0 0 0 0 0 0 0 0 0 ...
##  $ date    : chr  "2012-10-01" "2012-11-23" "2012-10-28" "2012-11-06" ...
##  $ interval: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ daytype : Factor w/ 2 levels "weekday","weekend": 1 1 2 1 2 1 2 1 1 2 ...
```

```r
head(newnewactivity, 5)
```

```
##   steps       date interval daytype
## 1     2 2012-10-01        0 weekday
## 2     0 2012-11-23        0 weekday
## 3     0 2012-10-28        0 weekend
## 4     0 2012-11-06        0 weekday
## 5     0 2012-11-24        0 weekend
```

```r
weekdays(as.Date(newnewactivity$date[3]))
```

```
## [1] "Sunday"
```

```r
### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
### the average number of steps taken, averaged across all days for each 5-minute
### interval
newinterval <- aggregate(steps ~ interval + daytype, newnewactivity, mean)

### add descriptive variable names
names(newinterval)[3] <- "mean_steps"

### check out new data frame
head(newinterval, 5)
```

```
##   interval daytype mean_steps
## 1        0 weekday 2.28888889
## 2        5 weekday 0.40000000
## 3       10 weekday 0.15555556
## 4       15 weekday 0.17777778
## 5       20 weekday 0.08888889
```

```r
### plot time series
library(lattice)
xyplot(
    mean_steps ~ interval | daytype,
    newinterval,
    type = "l",
    layout = c(1,2),
    main = "Time Series Plot of the 5-Minute Interval\nand the Average Number of Steps Taken,\nAveraged Across All Weekday Days or Weekend Days",
    xlab = "5-Minute Interval",
    ylab = "Average Number of Steps Taken"
)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
