---
title: "Week 2 Assignment"
output:
  pdf_document: default
  html_document: default
---


### This is an analysis looks at activity data from several brands of activity trackers. Results include the mean number of steps taken per day, daily activity patterns and differences between weekday and weekend activity. 

## Reading in Data
```{r, echo = TRUE}
dat<-read.csv("activity.csv")
```

## Converting to date format
```{r, echo = TRUE}
dat$date <- as.POSIXct(dat$date, format = "%Y-%m-%d")
```

## Histogram of steps per day
``` {r, echo = TRUE}
daysteps <- aggregate(steps~date, dat, sum)
hist(daysteps$steps, xlab = "Steps per Day", main = "Frequency of Steps per Day")
```

## Mean of steps per day
```{r, echo = TRUE}
mean(daysteps$steps, na.rm = TRUE)
```

## Median of steps
```{r, echo = TRUE}
median(daysteps$steps, na.rm = TRUE)
```

## Plot of Steps per Interval
```{r, echo = TRUE}
intervalsteps <- aggregate(steps~interval, dat, mean)
plot(intervalsteps$interval, intervalsteps$steps, type = "l", xlab = "Interval", ylab = "Mean Steps", main = "Mean Steps per Interval")
```

## Max 5 Minute Interval
```{r, echo = TRUE}
max <- intervalsteps[which.max(intervalsteps$steps), 1]
max
```

## Number of rows is NAs 
```{r, echo = TRUE}
sum(is.na(dat$steps))
```

## Imputing missing values 
```{r, echo = TRUE}
datcopy <- dat
mergy <- merge(datcopy, 
               daysteps, 
               by= "date", 
               suffixes = c(".dat", ".spd"))
NAs <- which(is.na(datcopy$steps))
datcopy[NAs, "steps"] <- mergy[NAs, "steps.spd"]

```

## Calculating and plotting steps per day
```{r, echo = TRUE}
datcopybyday<-aggregate(steps~date, datcopy, sum)
plot(datcopybyday$date, 
     datcopybyday$steps, 
     type = "h", 
     main = "Histogram of Daily Steps (New Nas)", 
     xlab = "Date", 
     ylab = "Steps", 
     col="blue", 
     lwd =8)
median(datcopybyday$steps)
mean(datcopybyday$steps)
```


## Separating weekends from weekdays
### Package "chron" required
```{r, echo = TRUE}
library(chron)
datcopy$weekend <- is.weekend(datcopy$date)

datcopy.with.weekday <- subset(datcopy, weekend == "FALSE")
datcopy.with.weekend <- subset(datcopy, weekend =="TRUE")
```

## Calculating and plotting weekday and weekend step per interval
```{r, echo = TRUE}
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
