Personal activity project
==========================
##following is code for Loading and preprocessing the data

```r
setwd("C:/Users/u292859/Desktop/Reproducible Research")
url="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url,destfile="C:/Users/u292859/Desktop/Reproducible Research/AMD.zip")
# then unzip the zip files
unzip ("AMD.zip", exdir = "C:/Users/u292859/Desktop/Reproducible Research")

# list the files that unziped
list.files(path="C:/Users/u292859/Desktop/Reproducible Research")
```

```
##  [1] " reproducible research project 1.md"               
##  [2] "activity.csv"                                      
##  [3] "AMD.zip"                                           
##  [4] "average daily activity pattern"                    
##  [5] "average daily activity pattern.png"                
##  [6] "average number of steps by weekday and weekend.png"
##  [7] "figure"                                            
##  [8] "project 1.Rmd"                                     
##  [9] "reproducible research project 1.html"              
## [10] "reproducible research project 1.md"                
## [11] "reproducible research project 1.Rmd"               
## [12] "total number of steps with new data.png"           
## [13] "total number of steps.png"
```

```r
data <- read.csv("C:/Users/u292859/Desktop/Reproducible Research/activity.csv")
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
## What is mean total number of steps taken per day?
* follwoing is code for Calculate the total number of steps taken per day

```r
sum<-aggregate(data$steps, by = list(date=data$date), FUN = "sum", na.rm = TRUE)
head(sum)
```

```
##         date     x
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
colnames(sum)<-c("date","total_steps")
```
* following is code of make a histogram of the total number of steps taken each day

```r
hist(sum$total_steps,col="red",main="total steps per day",xlab="total steps per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
dev.copy(png,file="total number of steps.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

* following is code for Calculate and report the mean and median of the total number of steps taken per day

```r
mean(sum$total_steps)
```

```
## [1] 9354.23
```

```r
median(sum$total_steps)
```

```
## [1] 10395
```
## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
 

```r
average<-aggregate(data$steps, by=list(interval=data$interval),FUN="mean",na.rm=TRUE)
colnames(average)<-c("interval","average_steps")
head(average)
```

```
##   interval average_steps
## 1        0     1.7169811
## 2        5     0.3396226
## 3       10     0.1320755
## 4       15     0.1509434
## 5       20     0.0754717
## 6       25     2.0943396
```

```r
plot(average$interval,average$average_steps,type="l", xlab="5 minute interval", ylab="average number of steps",
main ="average daily activity pattern")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

```r
dev.copy(png,file="average daily activity pattern")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```
* following is code of Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max(average$average_steps, na.rm = TRUE)
```

```
## [1] 206.1698
```

```r
order<-average[rev(order(average$average_steps)),]

head(order)
```

```
##     interval average_steps
## 104      835      206.1698
## 105      840      195.9245
## 107      850      183.3962
## 106      845      179.5660
## 103      830      177.3019
## 101      820      171.1509
```
## Imputing missing values
*follwoing is code of Calculate and report the total number of missing values in the dataset 

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```
*follwoign is a strategy for filling in all of the missing values with  the mean for that 5-minute intervalin the dataset. 
*Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
total <- merge(data,average,by="interval")
head(total)
```

```
##   interval steps       date average_steps
## 1        0    NA 2012-10-01      1.716981
## 2        0     0 2012-11-23      1.716981
## 3        0     0 2012-10-28      1.716981
## 4        0     0 2012-11-06      1.716981
## 5        0     0 2012-11-24      1.716981
## 6        0     0 2012-11-15      1.716981
```

```r
total$steps[is.na(total$steps)] <- total$average_steps[is.na(total$steps)]
newdata <- subset(total, select = -average_steps )
head(newdata)
```

```
##   interval    steps       date
## 1        0 1.716981 2012-10-01
## 2        0 0.000000 2012-11-23
## 3        0 0.000000 2012-10-28
## 4        0 0.000000 2012-11-06
## 5        0 0.000000 2012-11-24
## 6        0 0.000000 2012-11-15
```
*Make a histogram of the total number of steps taken each day 

```r
newsum<-aggregate(newdata$steps, by = list(date=newdata$date), FUN = "sum")
head(newsum)
```

```
##         date        x
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

```r
colnames(newsum)<-c("date","total_steps")
hist(newsum$total_steps,col="red",main="total steps per day  with  missing value replaced with average steps",xlab="total steps per day")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

```r
dev.copy(png,file="total number of steps with new data.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```
* follwoing are codes of  Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
mean(newsum$total_steps)
```

```
## [1] 10766.19
```

```r
median(newsum$total_steps)
```

```
## [1] 10766.19
```
##Are there differences in activity patterns between weekdays and weekends?


1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
newdata$date<-as.Date(newdata$date)
str(newdata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ interval: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ steps   : num  1.72 0 0 0 0 ...
##  $ date    : Date, format: "2012-10-01" "2012-11-23" ...
```

```r
newdata$weekdays<-weekdays(newdata$date)
```

```r
head(newdata)
```

```
##   interval    steps       date weekdays
## 1        0 1.716981 2012-10-01   Monday
## 2        0 0.000000 2012-11-23   Friday
## 3        0 0.000000 2012-10-28   Sunday
## 4        0 0.000000 2012-11-06  Tuesday
## 5        0 0.000000 2012-11-24 Saturday
## 6        0 0.000000 2012-11-15 Thursday
```

```r
newdata$weekdays<-weekdays(newdata$date)
head(newdata)
```

```
##   interval    steps       date weekdays
## 1        0 1.716981 2012-10-01   Monday
## 2        0 0.000000 2012-11-23   Friday
## 3        0 0.000000 2012-10-28   Sunday
## 4        0 0.000000 2012-11-06  Tuesday
## 5        0 0.000000 2012-11-24 Saturday
## 6        0 0.000000 2012-11-15 Thursday
```

```r
weekend <- c('Saturday', 'Sunday')
newdata$level<-ifelse(newdata$weekdays %in% weekend, 'weekend','weekday')
head(newdata)
```

```
##   interval    steps       date weekdays   level
## 1        0 1.716981 2012-10-01   Monday weekday
## 2        0 0.000000 2012-11-23   Friday weekday
## 3        0 0.000000 2012-10-28   Sunday weekend
## 4        0 0.000000 2012-11-06  Tuesday weekday
## 5        0 0.000000 2012-11-24 Saturday weekend
## 6        0 0.000000 2012-11-15 Thursday weekday
```

2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
weekdaysaverage<-aggregate(newdata$steps, by=list(level=newdata$level, interval=newdata$interval),FUN="mean")
colnames(weekdaysaverage)<-c("level", "interval", "average_steps")
head(weekdaysaverage)
```

```
##     level interval average_steps
## 1 weekday        0    2.25115304
## 2 weekend        0    0.21462264
## 3 weekday        5    0.44528302
## 4 weekend        5    0.04245283
## 5 weekday       10    0.17316562
## 6 weekend       10    0.01650943
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.3
```

```r
ggplot(data=weekdaysaverage, aes(x=interval, y=average_steps)) + geom_line(stat="identity", aes(fill=interval))  +facet_grid(level~., scales="free")+  ggtitle("average number of steps by weekday and weekend") + xlab("interval")+ylab("average steps") 
```

```
## Warning: Ignoring unknown aesthetics: fill
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png)

```r
dev.copy(png,file="average number of steps by weekday and weekend.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```
