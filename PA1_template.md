# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
  activity <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?

Subset the data by steps and dates, getting the sum of steps per date.
Create histogram.


```r
  steps <- aggregate(steps ~ date, data = activity, na.rm = TRUE, sum)    
  hist(steps$steps, xlab = "Steps per Day", ylab = "Number of Days Steps        Achieved", main = "Number of Steps per Day", col = 5)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Mean and median of total steps taken per day.


```r
  mean(steps$steps)
```

```
## [1] 10766.19
```

```r
  median(steps$steps)
```

```
## [1] 10765
```



## What is the average daily activity pattern?

Subset the data and make line plot.


```r
  y <- aggregate(steps ~ interval, data = activity, na.rm = TRUE, mean)
  plot(steps ~ interval, data = y, type = "l", col = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Get the interval with the highest average.


```r
  y[which.max(y$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```


## Imputing missing values

Calculate the number of rows with missing values.


```r
  sum(rowSums(is.na(activity)))
```

```
## [1] 2304
```

Replace the NA's with average steps from earlier.


```r
  for(i in 1:length(activity$steps)){
    if(is.na(activity$steps[i])){
      n <- which(activity$interval[i] == y$interval)
      activity$steps[i] <- y[n,]$steps
    }
  }
```

Histogram of new data, repeating Step 2.


```r
  steps <- aggregate(steps ~ date, data = activity, na.rm = TRUE, sum)    
  hist(steps$steps, xlab = "Steps per Day", ylab = "Number of Days Steps        Achieved", main = "Number of Steps per Day", col = 5)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Mean and median of new dataset. Only the median difference slightly.


```r
  mean(steps$steps)
```

```
## [1] 10766.19
```

```r
  median(steps$steps)
```

```
## [1] 10766.19
```



## Are there differences in activity patterns between weekdays and weekends?

Make sure the date data is in the correct format, then create column with the weekday.


```r
  activity$date <- as.Date(activity$date)
  activity$DayofWeek <- weekdays(activity$date)
  
  for(i in 1:length(activity$DayofWeek)){
    if(activity$DayofWeek[i] == "Saturday" | activity$DayofWeek[i] == "Sunday"){
      activity$type[i] <- "Weekend" 
    }
    else
      activity$type[i] <- "Weekday"
  }
  activity$type <- as.factor(activity$type)
  head(activity)
```

```
##       steps       date interval DayofWeek    type
## 1 1.7169811 2012-10-01        0    Monday Weekday
## 2 0.3396226 2012-10-01        5    Monday Weekday
## 3 0.1320755 2012-10-01       10    Monday Weekday
## 4 0.1509434 2012-10-01       15    Monday Weekday
## 5 0.0754717 2012-10-01       20    Monday Weekday
## 6 2.0943396 2012-10-01       25    Monday Weekday
```

Time series plot.


```r
  library(ggplot2)
  y <- aggregate(steps ~ interval + type, data = activity, na.rm = TRUE, mean)
  p <- ggplot(y, aes(x = interval, y = steps)) +
    geom_line() +
    facet_grid(type ~ .)
  print(p)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->






