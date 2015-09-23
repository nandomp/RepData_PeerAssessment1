---
title: 'Reproducible Research: Peer Assessment 1'
author: "Fernando Martínez Plumed"
date: "23th of September,2015"
---
Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.
Data

The data for this assignment can be downloaded from the course web site:

    Dataset: Activity monitoring data [52K]

The variables included in this dataset are:

    steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

    date: The date on which the measurement was taken in YYYY-MM-DD format

    interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.
Assignment

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

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
![plot of chunk plot1](instructions_fig/plot1.png) 


```{r}
trunc(mean(Activity.day.sum$total, na.rm = T))
trunc(median(Activity.day.sum$total, na.rm = T))

```

MEAN: 10766
MEDIAN: 10765


#What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
Activity.interval <- group_by(Activity, interval) 
Activity.interval.avg <- summarise(Activity.interval, avg = mean(steps, na.rm=T))
  

g <- ggplot(Activity.interval.avg, aes(interval, avg))
g + geom_line() + ggtitle("Average number of steps over all days") + xlab("Interval") + ylab("Average number of steps")


```
![plot of chunk plot2](instructions_fig/plot2.png) 


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
![plot of chunk plot3](instructions_fig/plot3.png) 


```{r}
trunc(mean(Activity.clean.day.sum$total, na.rm = T))
trunc(median(Activity.clean.day.sum$total, na.rm = T))

```
MEAN: 10766
MEDIAN: 10765

Comparing original and modified data There is no significant change in the results: the number of valid samples in the modified data is greater, so the total steps is also greater, but the histogram is very similar in shape, and the mean and median are practily the same

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



```
![plot of chunk plot4](instructions_fig/plot4.png) 

Yes, it seems there are a lot of differences between weekdays and weekends. People tend to wake up later. During weekdays the activity peak is at 8:35 am whereas in the weekend the peaks are around 10:00 am and 4:00 pm