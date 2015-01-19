# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
# require R 3.1.2 for newes dplyr functions
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
# setwd("/Volumes/Transcend/coursera/statinf/courses/05_ReproducibleResearch/mywork/RepData_PeerAssessment1")
activity.df<- read.csv(unz("activity.zip","activity.csv"))
activity.df$bdate<-as.POSIXct(activity.df$date,fomat="%Y-%m-%d",tz="")
```


## What is mean total number of steps taken per day?

```r
# using "piping operator" %>% from the package "dplyr""
act.by.day<-activity.df %>% 
   group_by (bdate) %>%
      summarise(total.steps=sum(steps,na.rm=TRUE))

mean.steps.per.day <- mean(act.by.day$total.steps)
median.steps.per.day<- median(act.by.day$total.steps)

hist(act.by.day$total.steps,main = "Histogram: Daily walking activity", xlab="#steps",ylab="frequency (#days)")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

**Note:** Observations with *NA* values not taken into account in this histogram 

Average activity per day: 

**median= 10395 steps **

**mean = 9354.2295082   steps  **

## What is the average daily activity pattern?


```r
# using "piping operator" %>% from the package "dplyr""
act.by.interval <-activity.df %>% 
   group_by (interval) %>%
      summarise(mean.steps=mean(steps,na.rm=TRUE))

plot(x=act.by.interval$interval,y=act.by.interval$mean.steps,type="l",main="Average day activity pattern",xlab="daytime",ylab="steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

**Note:** Observations with *NA* values not taken into account in this histogram 

#### 5-minute interval with maximum average activity

```r
max_interval<-act.by.interval[match(max(act.by.interval$mean.steps),act.by.interval$mean.steps),]
```


Interval with most activity is [id,#steps] : [835, 206.1698113]

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)

```r
  sum(is.na(activity.df$steps))
```

```
## [1] 2304
```

or another way:


```r
nrow(activity.df[which(is.na(activity.df)),])
```

```
## [1] 2304
```

(total number of rows is 17568)

2. Devise a strategy for filling in all of the missing values in the dataset. The
strategy does not need to be sophisticated. For example, you could use
the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
global_mean<-mean(activity.df$steps, na.rm=TRUE)
mean_interval_activity.by.day<-activity.df %>% 
   group_by (bdate) %>%
      summarise(mean.steps=mean(steps,na.rm=TRUE))
day_means<-mutate(mean_interval_activity.by.day,mean.steps=ifelse(is.nan(mean.steps),global_mean,mean.steps))
join_with_means=inner_join(activity.df,day_means)
```

```
## Joining by: "bdate"
```

3. Create a new dataset that is equal to the original dataset but with the
missing data filled in.

```r
activity.wo.na <- mutate(.data = join_with_means, steps = ifelse(is.na(steps),mean.steps,steps))
glimpse(activity.wo.na)
```

```
## Observations: 17568
## Variables:
## $ steps      (dbl) 37.3826, 37.3826, 37.3826, 37.3826, 37.3826, 37.382...
## $ date       (fctr) 2012-10-01, 2012-10-01, 2012-10-01, 2012-10-01, 20...
## $ interval   (int) 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 100, ...
## $ bdate      (time) 2012-10-01, 2012-10-01, 2012-10-01, 2012-10-01, 20...
## $ mean.steps (dbl) 37.3826, 37.3826, 37.3826, 37.3826, 37.3826, 37.382...
```

4. Make a histogram of the total number of steps taken each day and Calculate
and report the mean and median total number of steps taken per day. Do
these values differ from the estimates from the first part of the assignment?
What is the impact of imputing missing data on the estimates of the total
daily number of steps?

```r
act.by.day.wo.na<-activity.wo.na %>% 
   group_by (bdate) %>%
      summarise(total.steps=sum(steps,na.rm=FALSE))

mean.steps.per.day.wo.na <- mean(act.by.day.wo.na$total.steps)
median.steps.per.day.wo.na<- median(act.by.day.wo.na$total.steps)

hist(act.by.day.wo.na$total.steps,main = "Histogram: Daily walking activity after filling NA values", xlab="#steps",ylab="frequency (#days)")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

mean value: **1.0766189\times 10^{4} **

median value: **1.0766189\times 10^{4}**



## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset
with the filled-in missing values for this part.



1. Create a new factor variable in the dataset with two levels <U+2013> <U+201C>weekday<U+201D>
and <U+201C>weekend<U+201D> indicating whether a given date is a weekend or weekend
day.


```r
# for English weekday names
Sys.setlocale("LC_ALL", "C") 
```

```
## [1] "C/C/C/C/C/ru_RU.UTF-8"
```

```r
Sys.setlocale("LC_TIME", "C") 
```

```
## [1] "C"
```

```r
activity.wo.na <- mutate(activity.wo.na,weekday=weekdays(bdate,abbreviate = TRUE))

activity.wo.na <- mutate(activity.wo.na,is.weekend=factor(weekday %in% c("Sat","Sun"),labels=c("weekday","weekend")))

# weekday activity pattern
weekend.act.by.interval <-activity.wo.na %>% 
  filter(is.weekend=="weekend") %>%
  group_by (interval) %>%
      summarise(mean.steps=mean(steps))

weekday.act.by.interval <-activity.wo.na %>% 
  filter(is.weekend=="weekday") %>%
  group_by (interval) %>%
      summarise(mean.steps=mean(steps))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 
5-minute interval (x-axis) and the average number of steps taken, averaged
across all weekday days or weekend days (y-axis). The plot should look
something like the following, which was creating using simulated data:
Your plot will look different from the one above because you will be using
the activity monitor data. Note that the above plot was made using the lattice
system but you can make the same version of the plot using any plotting system
you choose.

```r
par(mfrow = c(2, 1))
plot(weekday.act.by.interval$interval, weekday.act.by.interval$mean.steps, 
     type="l", col="blue", lwd=3,
     main="Daily activity pattern on weekdays", 
     xlab="Interval (hhmm)", ylab="Average number of steps")

plot(weekday.act.by.interval$interval, weekend.act.by.interval$mean.steps, 
     type="l", col="red", lwd=3, 
     main="Daily activity pattern at weekend",
     xlab="Interval (hhmm)", ylab="Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
