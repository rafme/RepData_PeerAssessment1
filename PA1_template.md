---
title: "RepData_PeerAssessment1"
author: "Rafael Mendez"
date: "August 7, 2015"
output: html_document
---

#Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

##Data

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

date: The date on which the measurement was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

This Markdown File will now go through the steps required for this assignement including operations and plotting

Before Anything: Prepare Global Enviornment 

```r
library(knitr)
library(data.table)
```

```
## data.table 1.9.4  For help type: ?data.table
## *** NB: by=.EACHI is now explicit. See README to restore previous behaviour.
```

```r
library(ggplot2)
```

```
## Need help? Try the ggplot2 mailing list: http://groups.google.com/group/ggplot2.
```

```r
library(lattice)
opts_chunk$set(echo = TRUE, results = 'hold')       
#Define Text results. Echo all results and hold output pieces and push them                             to end of chunk
```

1. Loading and preprocessing the data

    A) Load in the activity.csv Data and Read into a dataframe

```r
url <- "hf bcfttps://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip "
download.file(url, destfile = activity.csv )
```

```
## Error in download.file(url, destfile = activity.csv): object 'activity.csv' not found
```

```r
activitydata<-read.csv("activity.csv", header = TRUE, sep = ",", 
colClasses = c("numeric","character","numeric"))
```

    B) Check the given data frame with str()

```r
    str(activitydata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...
```
    
    C) Tidy the data frames date and interval field 

```r
    #Make the date field a date object.  
    activitydata$date <- as.POSIXlt(activitydata$date)
```

2. What is mean total number of steps taken per day?

    A) Calculate the sum of the total steps per day

```r
    #First aggregate the steps by data in the data, with the function sum
    #Make new data frame with steps per day 
    Total_steps_day <- aggregate(activitydata$steps, by=list(activitydata$date), FUN=sum, na.rm = TRUE)
```

```
## Error in complete.cases(by): invalid 'type' (list) of argument
```

```r
    colnames(Total_steps_day ) <- c("date","steps")
```

```
## Error in colnames(Total_steps_day) <- c("date", "steps"): object 'Total_steps_day' not found
```

```r
    head(Total_steps_day,10)
```

```
## Error in head(Total_steps_day, 10): object 'Total_steps_day' not found
```
    
    B) Histogram: Total Number of Steps per day with missing values included  

```r
    ggplot(Total_steps_day, aes(x = steps)) +
    geom_histogram(col= "black",bindwidth = 1000) +
    ggtitle("Activity: Steps per Day") + 
    xlab("Number of Steps per Day") + 
    ylab("Number of times in day(Count)")
```

```
## Error in ggplot(Total_steps_day, aes(x = steps)): object 'Total_steps_day' not found
```

hist(Total_steps_day$steps,
     breaks=seq(from=0, to=25000, by=2500),
     col="black", 
     xlab="Number of Steps Per Day", 
     ylab="Number of times in day(Count)",
     ylim=c(0, 20), 
     main="Histogram of the total number of steps taken each day\n(NA removed)")
  
    C) Report the mean and median of the total number of steps taken per day

```r
    Total_steps_mean <- mean(Total_steps_day$steps, na.rm = TRUE)
```

```
## Error in mean(Total_steps_day$steps, na.rm = TRUE): object 'Total_steps_day' not found
```

```r
    Total_steps_median <- median(Total_steps_day$steps, na.rm = TRUE)
```

```
## Error in median(Total_steps_day$steps, na.rm = TRUE): object 'Total_steps_day' not found
```

```r
    Total_steps_mean    
```

```
## Error in eval(expr, envir, enclos): object 'Total_steps_mean' not found
```

```r
    Total_steps_median
```

```
## Error in eval(expr, envir, enclos): object 'Total_steps_median' not found
```

3. What is the average daily activity pattern?

    A) TimeSeries Plot: Average Daily Activity Pattern of Steps Per Day

```r
    #Can use tapply or aggregate, choose aggregate to make into a data frame 
    #which is easier to use than a matrix. 
    
    #Steps per interval
    time_series <- aggregate(activitydata$steps, by = list(interval = activitydata$interval), 
    FUN=mean, na.rm = TRUE)
    colnames(time_series) <- c("interval", "steps") #steps is x if this is not done
    
    #Use xyplot for bivariate time-seris plots
    xyplot(steps~interval, data=time_series, type="l", grid=TRUE, col ="red", xlab="5-min Intervals", 
    ylab="Average Steps on all Days", main="Average Daily Activity Patterns:Steps", aspect="fill", 
    size=5, border=TRUE)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 
    
    B) Report which 5-minute interval has the max average of steps

```r
    #Find the row which has the max number of steps use [] to access elements in data frame and the 'which.max()' function to find index
    max_avg_interval <- time_series[which.max(time_series$steps),]
    max_avg_interval #shows what interval has max number of steps and what steps 
```

```
##     interval    steps
## 104      835 206.1698
```

4. Imputing missing values - Missing days may Introduce Bias. Show this

    A) Report Number of Missing Values 

```r
    #Report the total number of missing values in the dataset 
    missingvalue <- sum(is.na(activitydata$steps))
    missingvalue
```

```
## [1] 2304
```

    B) Make new dataset with NA filled in
       Solution:Replace missing values with average steps per interval from time_seris plot

```r
    #Make a new data frame for the Activity with NA
Filled_Data <- activitydata
head(Filled_Data,10) #Compare Data with NA 
for(i in 1:nrow(Filled_Data)) {
    if(is.na(Filled_Data$steps[i])) {
        replace <- time_series$steps[time_series$interval == Filled_Data$interval[i]];
        Filled_Data$steps[i] <- replace;
    }
}
    
head(Filled_Data,10)           #Compare Data without NA
sum(is.na(Filled_Data$steps))  #Check if any NA left in Dataset
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
##        steps       date interval
## 1  1.7169811 2012-10-01        0
## 2  0.3396226 2012-10-01        5
## 3  0.1320755 2012-10-01       10
## 4  0.1509434 2012-10-01       15
## 5  0.0754717 2012-10-01       20
## 6  2.0943396 2012-10-01       25
## 7  0.5283019 2012-10-01       30
## 8  0.8679245 2012-10-01       35
## 9  0.0000000 2012-10-01       40
## 10 1.4716981 2012-10-01       45
## [1] 0
```

    C) Histogram: Total Number of Steps per day with missing values filled (replaced) 

```r
    steps_day_NA <- aggregate(Filled_Data$steps, by=list(Filled_Data$date), FUN = sum)
```

```
## Error in complete.cases(by): invalid 'type' (list) of argument
```

```r
    colnames(steps_day_NA) <- c("date","steps")
```

```
## Error in colnames(steps_day_NA) <- c("date", "steps"): object 'steps_day_NA' not found
```

```r
    ggplot(steps_day_NA, aes(x = steps)) + 
    geom_histogram(color = "black",bindwidth = x, fill= "blue") + 
    ggtitle("Activity: Steps per Day (Data with missing values filled/replaced)") + 
    xlab("Number of Steps per Day") + 
    ylab("Number of times in day(Count)")
```

```
## Error in ggplot(steps_day_NA, aes(x = steps)): object 'steps_day_NA' not found
```
    
    D) Report the Mean and Median Total: Total Steps Per Day

```r
Total_steps_mean_NA <- mean(steps_day_NA$steps, na.rm = TRUE)
```

```
## Error in mean(steps_day_NA$steps, na.rm = TRUE): object 'steps_day_NA' not found
```

```r
Total_steps_median_NA <- median(steps_day_NA$steps, na.rm = TRUE)
```

```
## Error in median(steps_day_NA$steps, na.rm = TRUE): object 'steps_day_NA' not found
```

```r
Total_steps_mean_NA
```

```
## Error in eval(expr, envir, enclos): object 'Total_steps_mean_NA' not found
```

```r
Total_steps_median_NA
```

```
## Error in eval(expr, envir, enclos): object 'Total_steps_median_NA' not found
```

    E) Report if these values differ from the estimates from having missing values included in the first dataset and report the impact of imputting missing data on estimates of the total daily number of steps
    
    Result: Yes the values in means and median changed. Imputing missing values have more data, thus the     greater mean and median.

```r
#Before Missing Values are Imputted
#Mean
Total_steps_mean
```

```
## Error in eval(expr, envir, enclos): object 'Total_steps_mean' not found
```

```r
#Median
Total_steps_median
```

```
## Error in eval(expr, envir, enclos): object 'Total_steps_median' not found
```

```r
#Imputted Missing Values
#Mean
Total_steps_mean_NA
```

```
## Error in eval(expr, envir, enclos): object 'Total_steps_mean_NA' not found
```

```r
#Median
Total_steps_median_NA
```

```
## Error in eval(expr, envir, enclos): object 'Total_steps_median_NA' not found
```

5. Are there differences in activity patterns between weekdays and weekends?

    A) Create a new factor variable in the dataset with two levels- "weekday" and "weekend" incdicating
    whether a given date is a weekday or weekend day
    

```r
# Compute the weekdays from the date attribute
Filled_Data <- data.frame(date=Filled_Data$date, 
                           weekday=tolower(weekdays(Filled_Data$date)), 
                           steps=Filled_Data$steps, 
                           interval=Filled_Data$interval)

# Create field daytype to identify weekdend and weekdays
Filled_Data <- cbind(Filled_Data, 
                      daytype=ifelse(Filled_Data$weekday == "saturday" |  #test is saturday or sunda 
                                     Filled_Data$weekday == "sunday", 
                                     "weekend",                           #if either then weekend
                                     "weekday"))                          #else is a weekday

# Create the final data.frame
Filled_Data_Weeks <- data.frame(date=Filled_Data$date, 
                       weekday=Filled_Data$weekday, 
                       daytype=Filled_Data$daytype, 
                       interval=Filled_Data$interval,
                       steps=Filled_Data$steps)
```
    
    B) Panel Plot Containing TIme_Series of 5-minute interval (x-axis) and average number of steps 
    taken, averaged accross all weekday days or weekend days (y-axis). 
    

```r
#Find Avg Number of steps taken averaged accross all weekday days or weenend days 
avg_steps_week <- aggregate(Filled_Data_Weeks$steps, 
                            by=list(Filled_Data_Weeks$daytype, 
                            Filled_Data_Weeks$weekday, Filled_Data_Weeks$interval), mean)
colnames(avg_steps_week) <- c("daytype", "weekday", "interval", "Avg_steps")
```

Create the Time_Series Plot 

```r
xyplot(Avg_steps ~ interval | daytype, data = avg_steps_week,
       type = "l",
       lwd = 1,
       xlab= "Interval",
       ylab = "Number of Steps Per Interval",
       layout=c(1,2))
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png) 
