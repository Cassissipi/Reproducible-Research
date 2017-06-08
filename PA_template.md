Week 2 Assignment
================

### This is an analysis looks at activity data from several brands of activity trackers. Results include the mean number of steps taken per day, daily activity patterns and differences between weekday and weekend activity.

Reading in Data
---------------

``` r
dat<-read.csv("activity.csv")
```

Converting to date format
-------------------------

``` r
dat$date <- as.POSIXct(dat$date, format = "%Y-%m-%d")
```

Histogram of steps per day
--------------------------

``` r
daysteps <- aggregate(steps~date, dat, sum)
hist(daysteps$steps, xlab = "Steps per Day", main = "Frequency of Steps per Day")
```

![](PA_template_files/figure-markdown_github/unnamed-chunk-3-1.png)

Mean of steps per day
---------------------

``` r
mean(daysteps$steps, na.rm = TRUE)
```

    ## [1] 10766.19

Median of steps
---------------

``` r
median(daysteps$steps, na.rm = TRUE)
```

    ## [1] 10765

Plot of Steps per Interval
--------------------------

``` r
intervalsteps <- aggregate(steps~interval, dat, mean)
plot(intervalsteps$interval, intervalsteps$steps, type = "l", xlab = "Interval", ylab = "Mean Steps", main = "Mean Steps per Interval")
```

![](PA_template_files/figure-markdown_github/unnamed-chunk-6-1.png)

Max 5 Minute Interval
---------------------

``` r
max <- intervalsteps[which.max(intervalsteps$steps), 1]
max
```

    ## [1] 835

Number of rows is NAs
---------------------

``` r
sum(is.na(dat$steps))
```

    ## [1] 2304

Imputing missing values
-----------------------

``` r
datcopy <- dat
mergy <- merge(datcopy, 
               daysteps, 
               by= "date", 
               suffixes = c(".dat", ".spd"))
NAs <- which(is.na(datcopy$steps))
datcopy[NAs, "steps"] <- mergy[NAs, "steps.spd"]
```

Calculating and plotting steps per day
--------------------------------------

``` r
datcopybyday<-aggregate(steps~date, datcopy, sum)
plot(datcopybyday$date, 
     datcopybyday$steps, 
     type = "h", 
     main = "Histogram of Daily Steps (New Nas)", 
     xlab = "Date", 
     ylab = "Steps", 
     col="blue", 
     lwd =8)
```

![](PA_template_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
median(datcopybyday$steps)
```

    ## [1] 11405

``` r
mean(datcopybyday$steps)
```

    ## [1] 279514.9

Separating weekends from weekdays
---------------------------------

### Package "chron" required

``` r
library(chron)
datcopy$weekend <- is.weekend(datcopy$date)

datcopy.with.weekday <- subset(datcopy, weekend == "FALSE")
datcopy.with.weekend <- subset(datcopy, weekend =="TRUE")
```

Calculating and plotting weekday and weekend step per interval
--------------------------------------------------------------

``` r
weekdayint <- aggregate(steps~interval, datcopy.with.weekday, mean)
weekendint <- aggregate(steps~interval, datcopy.with.weekend, mean)
par(mfrow=c(2,1))
plot(weekdayint$interval, 
     weekdayint$steps, 
     type = "l", 
     xlab = "Weekday Interval", 
     ylab = "Steps")
plot(weekendint$interval, 
     weekendint$steps, 
     type = "l", 
     xlab = "Weekend Interval", 
     ylab = "Steps")
```

![](PA_template_files/figure-markdown_github/unnamed-chunk-12-1.png)
