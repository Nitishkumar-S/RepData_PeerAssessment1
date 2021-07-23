---
output: 
  html_document: 
    keep_md: yes
---
# Report on Activity dataset


## Loading the data and/or processing it
Data is stored in local directory and is loaded into r environment using following command

```r
library(ggplot2)
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
data<-read.csv("activity.csv")
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

## Total number of steps taken per day
This is calculated by applying sum on "steps" variable using "date" as splitting variable.

```r
sum <- aggregate(data$steps,list(Date=data$date),FUN=sum,simplify=TRUE)
g <- ggplot(sum, aes(x)) +geom_histogram(na.rm=TRUE) +ylab("No. of steps taken")+ xlab("days")
print(g)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/total-1.png)<!-- -->

Mean of the number of steps taken is

```
## [1] 10766.19
```
Median is g

```
## [1] 10765
```

## Average daily activity
The average of five minute intervals across all days is calculated using

```r
interval_mean <- aggregate(data$steps,list(interval=data$interval),FUN=mean, na.rm=T)
g<-ggplot(interval_mean,aes(interval,x))+geom_line()+ylab("steps")
print(g)
```

![](PA1_template_files/figure-html/interval-1.png)<!-- -->

The five minute interval with highest average steps taken is

```
## [1] 835
```

## Imputing the missing values
First let's see how many missing values are present,
total number of missing values are

```
## [1] 2304
```
Following code hepls in replacing the missing values with mean of that interval over al days which is present in interval_mean

```r
df<-data
df<-inner_join(df, interval_mean, by = "interval")
df[is.na(df$steps),1] <- df[is.na(df$steps),4]
df_sum<-aggregate(df$steps,list(Date=df$date),FUN=sum,simplify=TRUE)
g<-ggplot(df_sum,aes(x))+geom_histogram()+ylab("No. of steps")+xlab("days")
print(g)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/impute-1.png)<!-- -->

Mean of the number of steps taken is

```
## [1] 10766.19
```
Median is g

```
## [1] 10766.19
```
The new values dont differ much from the previous values, so there is not great impact of imputing the missing values

## Activity patterns between weekdays and weekends
Here we create new columns which hold details regarding the day of every observation, then we compare the activity pattern between weekdays and weekends

```r
df$date<-as.Date(df$date)
df$day <- weekdays(df$date, c(TRUE))
df$weekend <- sapply(df$day,function(x) { 
                                  if(x == "Sat" | x == "Sun") 
                                  { 
                                    "Weekend" 
                                  } else { 
                                    "Weekday"
                                  }
                              },simplify = TRUE)
g<-ggplot(df, aes(interval,steps))+geom_line()+facet_grid(weekend~.)
print(g)
```

![](PA1_template_files/figure-html/weekends-1.png)<!-- -->

Clearly there are more steps on weekdays than on weekends.
