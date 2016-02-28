---
title: "Activity Analysis"
author: "Sarathy Iyer"
date: "February 28, 2016"
output: html_document
---



# R Markdown

# Header 1 Assignment practice Reproducible Research using Knitr 
# Load Libraries required for processing


```r
library(nlme)
library(lattice)
library(ggplot2)
library(dplyr)
```
## Loading and preprocessing the data


```r
 activityData<-read.csv("./data/ReportPublish1/activity.csv")
 head(activityData)
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
 activityData1 <- activityData[ with (activityData, { !(is.na(steps)) } ), ]
```

## Summary of the 2 data sets to see the difference

```r
summary(activityData)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
summary(activityData1)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-02:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-03:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-04:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-05:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-06:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-07:  288   Max.   :2355.0  
##                   (Other)   :13536
```

```r
str(activityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
str(activityData1)
```

```
## 'data.frame':	15264 obs. of  3 variables:
##  $ steps   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## Activity 1.0: Make a histogram of the total number of steps taken each day
### Activity 1.1:Group the activity data by date and sum the steps. 
### Display the steps_by_day
### Draw the histogram 

```r
by_day <- group_by(activityData1, date)
steps_by_day <- summarise(by_day, total = sum(steps))
steps_by_day
```

```
## Source: local data frame [53 x 2]
## 
##          date total
##        (fctr) (int)
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## ..        ...   ...
```

```r
hist(steps_by_day$total, main="Histogram - total number of steps per day", 
     xlab="Total number of steps in a day")
```

![plot of chunk histogram1](figure/histogram1-1.png)
### Activity 1.2. Calculate and report the mean and median total number of steps taken per day


```r
summary(steps_by_day)
```

```
##          date        total      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```

## Activity 2.0: What is the average daily activity pattern?
### Activity 2.1: Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
	avgsteps<-aggregate(steps ~ interval, data = activityData1, mean)
	plot(avgsteps$interval,avgsteps$steps,type="l",main="Average Steps in the interval",xlab="Interval",ylab="Average Steps")
```

![plot of chunk timeseries5mininterval](figure/timeseries5mininterval-1.png)

### Activity 2.2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
  avgstepsdesc<-arrange(avgsteps,desc(steps))
	head(avgstepsdesc)
```

```
##   interval    steps
## 1      835 206.1698
## 2      840 195.9245
## 3      850 183.3962
## 4      845 179.5660
## 5      830 177.3019
## 6      820 171.1509
```

```r
	maxinterval<-avgstepsdesc[1,1]
  maxinterval
```

```
## [1] 835
```

## Activity 3.0 Imputing missing values
### Activity 3.1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
xy1<-activityData[!complete.cases(activityData),]
	nrow(xy1)
```

```
## [1] 2304
```

### Activity 3.2 Devise a strategy for filling in all of the missing values in the dataset.

```r
activityDataNoNA<-activityData
	for(i in 1:nrow(activityDataNoNA)) {
	  if ( is.na(activityDataNoNA$steps[i]) ){
		activityDataNoNA$steps[i] = avgsteps[activityDataNoNA$interval[i] == avgsteps$interval,2]
	  }
	}
```

### Activity 3.3 Create a new dataset that is equal to the original dataset but with the missing data filled in. 
activityDataNoNA is the new data set and activityData is the original 

### Activity 3.4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
by_day_nona <- group_by(activityDataNoNA, date)
	   steps_by_day_nona <- summarise(by_day_nona, total = sum(steps))
	   steps_by_day_nona
```

```
## Source: local data frame [61 x 2]
## 
##          date    total
##        (fctr)    (dbl)
## 1  2012-10-01 10766.19
## 2  2012-10-02   126.00
## 3  2012-10-03 11352.00
## 4  2012-10-04 12116.00
## 5  2012-10-05 13294.00
## 6  2012-10-06 15420.00
## 7  2012-10-07 11015.00
## 8  2012-10-08 10766.19
## 9  2012-10-09 12811.00
## 10 2012-10-10  9900.00
## ..        ...      ...
```

```r
		hist(steps_by_day_nona$total, main="Histogram - total number of steps per day", xlab="Total number of steps in a day")
```

![plot of chunk NewHistogramwithImputValues](figure/NewHistogramwithImputValues-1.png)

### Activity 3.5 What is the impact of imputing missing data on the estimates of the total daily number of steps?
Not much of difference in mean and median there is very mild difference of one
		
Summary of Imput Value 

```r
summary(steps_by_day_nona)
```

```
##          date        total      
##  2012-10-01: 1   Min.   :   41  
##  2012-10-02: 1   1st Qu.: 9819  
##  2012-10-03: 1   Median :10766  
##  2012-10-04: 1   Mean   :10766  
##  2012-10-05: 1   3rd Qu.:12811  
##  2012-10-06: 1   Max.   :21194  
##  (Other)   :55
```

```r
summary(steps_by_day)
```

```
##          date        total      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```

Here is the comparison the totals	with mean input vs original	1.0766189 &times; 10<sup>4</sup>, 126, 1.1352 &times; 10<sup>4</sup>, 1.2116 &times; 10<sup>4</sup>, 1.3294 &times; 10<sup>4</sup>, 1.542 &times; 10<sup>4</sup>, 1.1015 &times; 10<sup>4</sup>, 1.0766189 &times; 10<sup>4</sup>, 1.2811 &times; 10<sup>4</sup>, 9900, 1.0304 &times; 10<sup>4</sup>, 1.7382 &times; 10<sup>4</sup>, 1.2426 &times; 10<sup>4</sup>, 1.5098 &times; 10<sup>4</sup>, 1.0139 &times; 10<sup>4</sup>, 1.5084 &times; 10<sup>4</sup>, 1.3452 &times; 10<sup>4</sup>, 1.0056 &times; 10<sup>4</sup>, 1.1829 &times; 10<sup>4</sup>, 1.0395 &times; 10<sup>4</sup>, 8821, 1.346 &times; 10<sup>4</sup>, 8918, 8355, 2492, 6778, 1.0119 &times; 10<sup>4</sup>, 1.1458 &times; 10<sup>4</sup>, 5018, 9819, 1.5414 &times; 10<sup>4</sup>, 1.0766189 &times; 10<sup>4</sup>, 1.06 &times; 10<sup>4</sup>, 1.0571 &times; 10<sup>4</sup>, 1.0766189 &times; 10<sup>4</sup>, 1.0439 &times; 10<sup>4</sup>, 8334, 1.2883 &times; 10<sup>4</sup>, 3219, 1.0766189 &times; 10<sup>4</sup>, 1.0766189 &times; 10<sup>4</sup>, 1.2608 &times; 10<sup>4</sup>, 1.0765 &times; 10<sup>4</sup>, 7336, 1.0766189 &times; 10<sup>4</sup>, 41, 5441, 1.4339 &times; 10<sup>4</sup>, 1.511 &times; 10<sup>4</sup>, 8841, 4472, 1.2787 &times; 10<sup>4</sup>, 2.0427 &times; 10<sup>4</sup>, 2.1194 &times; 10<sup>4</sup>, 1.4478 &times; 10<sup>4</sup>, 1.1834 &times; 10<sup>4</sup>, 1.1162 &times; 10<sup>4</sup>, 1.3646 &times; 10<sup>4</sup>, 1.0183 &times; 10<sup>4</sup>, 7047, 1.0766189 &times; 10<sup>4</sup>
vs 
126, 11352, 12116, 13294, 15420, 11015, 12811, 9900, 10304, 17382, 12426, 15098, 10139, 15084, 13452, 10056, 11829, 10395, 8821, 13460, 8918, 8355, 2492, 6778, 10119, 11458, 5018, 9819, 15414, 10600, 10571, 10439, 8334, 12883, 3219, 12608, 10765, 7336, 41, 5441, 14339, 15110, 8841, 4472, 12787, 20427, 21194, 14478, 11834, 11162, 13646, 10183, 7047
more number of totals are displayed. But it did not affect the histogram by viewing it


### Activity 4.0 Are there differences in activity patterns between weekdays and weekends?
### Activity 4.1 For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.


```r
#weekdays(as.Date(activityDataNoNA$date,"%Y-%m-%d"))
```


### Activity 4.2

```r
for(i in 1:nrow(activityDataNoNA)) {
	    x=weekdays(as.Date(activityDataNoNA$date[i],"%Y-%m-%d") )
	  if ( x == "Saturday" || x=="Sunday") {
		activityDataNoNA$weekDayType[i] = "weekend"
	  }
	  else {
	     activityDataNoNA$weekDayType[i] = "weekday"
	  }
	 }
```


### Activity 4.3 Make a panel plot containing a time series plot based on the weekday weekend and steps
	

```r
	avgstepsNew<-aggregate(steps ~ interval+weekDayType, data = activityDataNoNA, mean)
    xyplot(steps ~ interval|weekDayType,activityDataNoNA, type="l", aspect = "iso")
```

![plot of chunk FinalPlot](figure/FinalPlot-1.png)

