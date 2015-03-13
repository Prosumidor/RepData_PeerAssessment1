# Reproducible Research: Peer Assessment 1
##*by Javier Cantón*##

## Loading and preprocessing the data

**1. Loading the data**

```r
unzip("activity.zip")
act <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

**2. Calculate the total number of steps taken per day and make a histogram of the total number of steps taken each day**

```r
sd <- aggregate(steps ~ date, data=act, FUN=sum)
barplot(sd$steps, names.arg=sd$date, xlab="Fecha", ylab="Pasos", col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png) 

**3. Calculate and report the mean and median of the total number of steps taken per day**

```r
mean(sd$steps)
```

```
## [1] 10766.19
```

```r
median(sd$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
**4. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**

```r
sint <- aggregate(steps ~ interval, data=act, FUN=mean)
plot(sint, type="l", xlab="Actividad media", ylab="Pasos")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

**5. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**

```r
sint$interval[which.max(sint$steps)]
```

```
## [1] 835
```


## Imputing missing values
**6. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**

```r
sum(is.na(act))
```

```
## [1] 2304
```

**7. Devise a strategy for filling in all of the missing values in the dataset: using means for the 5-minute intervals as fillers for NAs. Create a new dataset (act2) equal to the original dataset but with the missing data filled in**

```r
act2 <- merge(act, sint, by = "interval", suffixes = c("", 
    ".y"))
nas <- is.na(act2$steps)
act2$steps[nas] <- act2$steps.y[nas]
act2 <- act2[, c(1:3)]
```

**8. Make an histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**

```r
sda <- aggregate(steps ~ date, data = act2, FUN = sum)
barplot(sda$steps, names.arg = sda$date, xlab = "Fechas", ylab = "Pasos", col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
mean(sda$steps)
```

```
## [1] 10766.19
```

```r
median(sda$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
**9. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day**

```r
day <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
act2$day <- as.factor(sapply(act2$date, day))
```

par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
  steps.type <- aggregate(steps ~ interval, 
                          data = act, 
                          subset = act2$daytype == type, 
                          FUN = mean)
    plot(steps.type, type = "l", main = type)
}
