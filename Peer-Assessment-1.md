---
title: "Reproducible Research: Peer Assessment 1"
output: 
      html_document:
          keep_md: true 
---



## Load and preprocesssing the data



```r
# load the data
raw_data <- read.csv('activity.csv', stringsAsFactors = FALSE)
# trasform date into character and interval into factor
raw_data$date <- as.Date.character(raw_data$date)
#raw_data$interval <- as.factor(raw_data$interval)
```

## What is mean total number of steps taken per day?

```r
# calculate total number of steps taken per day
gr_data.d <- group_by(raw_data, date)
sum_data.d <- summarise(gr_data.d, steps = sum(steps, na.rm = TRUE))

# mean and median steps taken per day
mean.steps <- mean(sum_data.d$steps, na.rm = TRUE)
median.steps <- median(sum_data.d$steps, na.rm = TRUE)

# plot a histogramm of the total number steps taken per day
hist(sum_data.d$steps, main = "Number of steps taken per day", xlab = "Steps")
abline(v = mean.steps, col = "red")
abline(v = median.steps, col = "blue ")
```

![](Peer-Assessment-1_files/figure-html/steps taken per day-1.png)<!-- -->

The mean (red line) of steps taken per day is about 9354.

The median (blue line) of steps taken per day is 10395.

## What is the average daily activity pattern?


```r
# group data by interval and calculate average number of steps per interval
gr_data.i <-  group_by(raw_data, interval)
sum_data.i <- summarise(gr_data.i, steps = mean(steps, na.rm = TRUE))

# time series plot 
plot(sum_data.i, type = "l", xlab = "5-minute Interval", main = "Average Dayly Steps per 5-minute Interval")
```

![](Peer-Assessment-1_files/figure-html/daily activity pattern-1.png)<!-- -->

```r
max.act.interval <- sum_data.i$interval[sum_data.i$steps == max(sum_data.i$steps)]
max.steps <- max(sum_data.i$steps)
```

5-minute interval, on average across all the days in the dataset, containing on average across all the days the maximum number of steps (206 steps), is 835. 

## Imputing missing value.
Number of missing values:

```r
# calculate number of missing values in dataset
colSums(is.na(raw_data))
```

```
##    steps     date interval 
##     2304        0        0
```


```r
# vector of indexes of missing values
index_mis.steps <-  which(is.na(raw_data$steps))
# copy of raw dataset for transformation
full_data <- raw_data
# replace every NA value with mean for relevant 5-minute interval
for (i in index_mis.steps){
    mis.interval <- full_data[i,3]
    mean_mis.interval <- sum_data.i$steps[sum_data.i$interval == mis.interval]
    full_data$steps[i] <- mean_mis.interval
}

# calculate total number of steps taken per day after fiiling of missing values
gr_fdata.d <- group_by(full_data, date)
sum_fdata.d <- summarise(gr_fdata.d, steps = sum(steps, na.rm = TRUE))

# mean and median number of steps taken per day after filling of missing values
f.mean.steps <- mean(sum_fdata.d$steps)
f.median.steps <- median(sum_fdata.d$steps)

# plot a histogramm of the total number steps taken per day after filling of missing values with lines showing mean and median
hist(sum_data.d$steps, main = "Number of steps taken per day after filling of missing values", xlab = "Steps")
abline(v = f.mean.steps, col = "red")
abline(v = f.median.steps, col = "blue ")
```

![](Peer-Assessment-1_files/figure-html/filling of missing values-1.png)<!-- -->

The mean and median of steps taken per day after filling missing data is about 10766.

Both statistics nearly the same after imputing of missing data.

## Are there differences in activity patterns between weekdays and weekends?

```r
# add one column for weekday
full_data$weekdays <- weekdays(full_data$date) 
# transform weekday to factor with two levels
full_data$weekdays  <- factor(full_data$weekdays, levels = c("Monday", "Tuesday",
        "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), labels = c("weekday",
        "weekday", "weekday", "weekday", "weekday", "weekend", "weekend"))
# calculate average number of steps per interval for weekdays and weekends
gr.fdata.w <- group_by(full_data, interval, weekdays)
sum.fdata.w <- summarise(gr.fdata.w, steps = mean(steps))
# plot 
ggplot(sum.fdata.w, aes(x = interval, y = steps)) +
    geom_line() +
    facet_wrap(~ weekdays, nrow = 2) +
    theme_classic()
```

![](Peer-Assessment-1_files/figure-html/weekdays & weekends-1.png)<!-- -->

As we can see range for a number of steps in weekends is smaller, there is no high peak in the morning.






