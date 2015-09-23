---
title: 'Reproducible Research: Peer Assessment 1'
author: "Fernando Martínez Plumed"
date: "23th of September,2015"
output: html_document
---

#Loading and preprocessing the data

```{r global_options, include=FALSE}
knitr::opts_chunk$set(echo=TRUE, warning=FALSE, message=FALSE)
```


Load libraries

```{r}

 # List of packages for session
.packages <- c("dplyr","reshape2","lubridate","ggplot2", "xtable")

installed <- function(pack){pack %in% installed.packages()[,1]}

load.pack <- function(packs){
  for (p in packs){
    if (!installed(p)){
      install.packages(p)
    }
    require(p, character.only=TRUE)
    
  }
}

load.pack(.packages)


```


Load the data (i.e. read.csv())


```{r}
Activity <- tbl_df(read.csv("activity.csv"))
Activity
```


Process/transform the data (if necessary) into a format suitable for your analysis: Date from factor to Date

```{r}
Activity$date <- ymd(Activity$date)
#Activity <- Activity[complete.cases(Activity)]
```



#What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

Make a histogram of the total number of steps taken each day



```{r}
Activity.day <- group_by(Activity, date) 
Activity.day
Activity.day.sum <- summarise(Activity.day, total= sum(steps,na.rm = T))

g <- ggplot(select(Activity.day.sum, date, total), aes(x=total))
g + geom_histogram(binwidth=5000) + ggtitle("Histogram of total number of steps per day")+ xlab("Total steps per day")

```



```{r}
mean <- round(mean(Activity.day.sum$total, na.rm = T),2)
median <- round(median(Activity.day.sum$total, na.rm = T),2)

```

Mean: `r mean` and median: `r median` 



#What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
Activity.interval <- group_by(Activity, interval) 
Activity.interval.avg <- summarise(Activity.interval, avg = mean(steps, na.rm=T))
  
g <- ggplot(Activity.interval.avg, aes(interval, avg))
g + geom_line() + ggtitle("Average number of steps over all days") + xlab("Interval") + ylab("Average number of steps")
```



```{r}
max <- Activity.interval.avg[which.max(Activity.interval.avg$avg),]$interval
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  `r max`


#Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.



```{r}
rowsNA <- sum(is.na(Activity))
```

The total number of missing values in the dataset (i.e. the total number of rows with NAs) : `r rowsNA`


Replacing NA's with the mean for that 5-minute interval. Creating a new dataset that is equal to the original dataset but with the missing data filled in.



```{r}
Activity.clean <- Activity
for(i in 1:nrow(Activity.clean)){
  if (is.na(Activity.clean$steps[i])){
    thisInterval <- Activity.clean$interval[i]
    AvgValue <- Activity.interval.avg[Activity.interval.avg$interval == thisInterval,]$avg
    Activity.clean$steps[i]<- AvgValue
  }
}
```



Histogram of the total number of steps taken each day and the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```{r}
Activity.clean.day <- group_by(Activity.clean, date) 
Activity.clean.day
Activity.clean.day.sum <- summarise(Activity.clean.day, total= sum(steps))

g <- ggplot(Activity.clean.day.sum, aes(total))
g + geom_histogram(binwidth = 5000) + ggtitle("Histogram of total number of steps per day") + xlab("Total steps per day")

```

```{r}
mean <- round(mean(Activity.clean.day.sum$total, na.rm = T), 2)
median <- round(median(Activity.clean.day.sum$total, na.rm = T),2)

```


Mean: `r mean` and median: `r median` 

Mean and median show slight differences between both datasets.


#Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r}

#New factor variable 
Activity.clean$wd <- ifelse(weekdays(Activity.clean$date) %in% c("sábado", "domingo"), "weekend", "weekday")
#Activity.clean$wd <- ifelse((wday(Activity.clean$date) == 1 || wday(Activity.clean$date) == 7),"weekend","weekday")

Activity.clean$wd<- as.factor(Activity.clean$wd)


Activity.clean.wd <- group_by(Activity.clean, interval, wd)
Activity.clean.wd.avg <- summarise(Activity.clean.wd, avg = mean(steps))

g <- ggplot(Activity.clean.wd.avg, aes(interval, avg))
g + geom_line() + facet_grid(wd ~ .) 

#

```


Yes, it seems there are a lot of differences between weekdays and weekends. People tend to wake up later. During weekdays the activity peak is at 8:35 am whereas in the weekend the peaks are around 10:00 am and 4:00 pm