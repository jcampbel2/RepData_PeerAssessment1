Reproducible Research - Peer Assessment 1
========================================================

## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

###  Set environment for script and load data

This markdown document assumes that
- The base working folder is C:/Users/james_000/DataScPrj/RepResearch/Assign1/RepData_PeerAssessment1
- The data is held in a sub-folder called .\data and is called activity.csv

The dataset used was originally downloaded from [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken


```r
##load Libs
library("plyr")
library("knitr")
library("ggplot2")
library("xtable")
##set workign directory
setwd("C:/Users/james_000/DataScPrj/RepResearch/Assign1/RepData_PeerAssessment1")
```


Data was loaded usign standard csv read functions with the "date" column converted to date type.


```r
actdata <- read.csv("./data/activity.csv")
actdata$date <- as.POSIXct(actdata$date)
```

## Question 1 - What is mean total number of steps taken per day?

By plotting the average number of steps in a given day (figure 1) we can see a string peak around 10,000 to 11,000 steps per day.  The shape appears broadly normally distributed around this peak with tails from 0 to 23,000.  The mean and median (figure 2) are very close to each other which implies distribution is likely to be symmetrical.

*Figure 1 - Histogram of Total Number of Steps by Day*

```r
##question 1
## create summary data frame by date
stepsbyday <- ddply(actdata[!is.na(actdata$steps),],.(date), 
           summarize,totsteps=sum(steps))

##histogram plot
p1 <- ggplot(stepsbyday,aes(totsteps))
p1 <- p1 +labs(title="Histogram of number of steps per day", y="Counts Days", x= "Total Steps in 1 day")
p1 <- p1 + geom_histogram(binwidth=1000, fill="red", color="black")
p1 <- p1 + scale_y_continuous(breaks=c(2,4,6,8,10,12))
p1 <- p1 + scale_x_continuous(breaks=seq(0,26000,2000))
p1
```

![plot of chunk plot1](figure/plot1.png) 

```r
stepmean <- mean(stepsbyday$totsteps)
stepmedian <- median(stepsbyday$totsteps)
rowname <- c("Mean","Median")
Values <- c(stepmean,stepmedian)
xtab1 <-xtable(as.data.frame(x=Values, row.names=rowname ) )                        
```

*Figure 2 - Summary Statistics for number of steps per day*

```r
print(xtab1, type="html")
```

<!-- html table generated in R 3.0.3 by xtable 1.7-3 package -->
<!-- Sun Jun 15 17:32:54 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Values </TH>  </TR>
  <TR> <TD align="right"> Mean </TD> <TD align="right"> 10766.19 </TD> </TR>
  <TR> <TD align="right"> Median </TD> <TD align="right"> 10765.00 </TD> </TR>
   </TABLE>

*Missing values have been ignored from above analysis.*

## Question 2 - What is the average daily activity pattern?



```r
avgdailyactivity <- ddply(actdata, .(interval), summarize, 
           avgsteps=mean(steps, na.rm=TRUE))
maxact <- avgdailyactivity[avgdailyactivity$avgsteps==max(avgdailyactivity$avgsteps),1]
```


If we then look at the average daily acivity in 5 minute intervals throughout the day we can see a strong peak between 8am and 9am with a daily average up to 200 steps per 5 minutes (figure 3).  The actual observed peak was at 835.  There was then a steadly level of around 100 steps per 5 minutes until early evening.  AS expected activity is very low between midnight and 6am.

*Figure 3 - Average daily activity.*

```r
p2 <- ggplot(avgdailyactivity,aes(interval,avgsteps))
p2 <- p2 +labs(title="Average daily activity", y="Average number of steps", x= "Time")
p2 <- p2 + geom_line(colour="blue")
p2 <- p2 + scale_x_continuous(breaks=c(0,300,600,900,1200,1500,1800,2100,2355),
                              labels=c("12am","3am","6am","9am","12pm","3pm","6pm","9pm","11:55pm"))
p2
```

![plot of chunk plotavgDaily](figure/plotavgDaily.png) 



## Question 3 - Imputing missing values



```r
numNA <- length(actdata[is.na(actdata$steps),1])
```
The dataset contains 2304 rows where the number of steps is recorded as a NA.  As an method for imputing values for these rows I have decided to use the average number of steps for each 5 minute interval as an estimator for the missing values.  The actual table for this is shown in the document appendices.



```r
actdata2 <- merge(x=actdata, y=avgdailyactivity, by="interval")
adjSteps <- with(actdata2,ifelse(is.na(steps),avgsteps,steps))
actdata2 <- cbind(actdata2,adjSteps)
```

*Figure 4 - As figure 2 but with missing values imputed*

```r
##question 1
## create summary data frame by date
stepsbyday2 <- ddply(actdata2,.(date), 
           summarize,totsteps=sum(adjSteps))

##histogram plot
p3 <- ggplot(stepsbyday2,aes(totsteps, fill="red"))
p3 <- p3 +labs(title="Histogram of Number of steps per day (missing values imputed)", y="Counts Days", x= "Total Steps in 1 day")
p3 <- p3 + geom_histogram(binwidth=1000, fill="red", color="black")
p3 <- p3 + scale_y_continuous(breaks=c(2,4,6,8,10,12))
p3 <- p3 + scale_x_continuous(breaks=seq(0,26000,2000))
p3
```

![plot of chunk plotstepday](figure/plotstepday.png) 

```r
stepmean2 <- mean(stepsbyday2$totsteps)
stepmedian2 <- median(stepsbyday2$totsteps)
rowname2 <- c("Mean","Mean (Imputed)","Median","Median (Imputed)")
Values2 <- c(stepmean,stepmean2,stepmedian,stepmedian2)
xtab3 <-xtable(as.data.frame(x=Values2, row.names=rowname2 ) )                        
```

### Summary Statistics for number of steps per day

By adopting the approach of imputing missing values with the average number of steps for that time slice we have created an interesting impact where the mean and median are EXACTLY the same.  This is the result of us having entire days with missing values.  These have now been imputed with exactly an average day.  This logically becomes the median or middle value.

*Figure 5 Summary statistics for average steps per day*

```r
print(xtab3, type="html")
```

<!-- html table generated in R 3.0.3 by xtable 1.7-3 package -->
<!-- Sun Jun 15 17:32:59 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Values2 </TH>  </TR>
  <TR> <TD align="right"> Mean </TD> <TD align="right"> 10766.19 </TD> </TR>
  <TR> <TD align="right"> Mean (Imputed) </TD> <TD align="right"> 10766.19 </TD> </TR>
  <TR> <TD align="right"> Median </TD> <TD align="right"> 10765.00 </TD> </TR>
  <TR> <TD align="right"> Median (Imputed) </TD> <TD align="right"> 10766.19 </TD> </TR>
   </TABLE>

## Question 4. Are there differences in activity patterns between weekdays and weekends?



```r
##calcuate if weekend based on abbreated day containing "S" and add as new column
DOW <- as.factor(ifelse(grepl("S",weekdays(actdata2$date, abbreviate=TRUE)),"Weekend","Weekday"))
actdata2 <- cbind(actdata2,DOW)

avgDOWactivity <- ddply(actdata, .(DOW,interval), summarize, 
           avgsteps=mean(steps, na.rm=TRUE))
```

If we split the datasets now into 2 subsets. One for weekdays (Monday to Friday) and the other for weekend days (Saturday and Sunday) and the plot the average number of steps in each 5 minute interval we can see the following pattern (figure 6).  This seems to show higher activity in the afternoon durng weekend days rather than weekdays. 


*Figure 6 Weekdays v's Weekends*

```r
p4 <- ggplot(avgDOWactivity,aes(interval,avgsteps))
p4 <- p4 + geom_line()
p4 <- p4 +labs(title="Average daily activity split between weekdays and weekends", y="Average number of steps", x= "Time")
p4 <- p4 + geom_line(colour="blue")

p4 <- p4 + scale_x_continuous(breaks=c(0,300,600,900,1200,1500,1800,2100,2355),
                              labels=c("12am","3am","6am","9am","12pm","3pm","6pm","9pm","11:55pm"))
p4 <- p4 + facet_grid( DOW ~ .)
p4
```

![plot of chunk plotDOW](figure/plotDOW.png) 

## Appendices
      
*Table 1*


```r
xtab2 <-xtable(avgdailyactivity )                        
print(xtab2, type="html")
```

<!-- html table generated in R 3.0.3 by xtable 1.7-3 package -->
<!-- Sun Jun 15 17:33:03 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> interval </TH> <TH> avgsteps </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD align="right">   0 </TD> <TD align="right"> 1.72 </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD align="right">   5 </TD> <TD align="right"> 0.34 </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD align="right">  10 </TD> <TD align="right"> 0.13 </TD> </TR>
  <TR> <TD align="right"> 4 </TD> <TD align="right">  15 </TD> <TD align="right"> 0.15 </TD> </TR>
  <TR> <TD align="right"> 5 </TD> <TD align="right">  20 </TD> <TD align="right"> 0.08 </TD> </TR>
  <TR> <TD align="right"> 6 </TD> <TD align="right">  25 </TD> <TD align="right"> 2.09 </TD> </TR>
  <TR> <TD align="right"> 7 </TD> <TD align="right">  30 </TD> <TD align="right"> 0.53 </TD> </TR>
  <TR> <TD align="right"> 8 </TD> <TD align="right">  35 </TD> <TD align="right"> 0.87 </TD> </TR>
  <TR> <TD align="right"> 9 </TD> <TD align="right">  40 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 10 </TD> <TD align="right">  45 </TD> <TD align="right"> 1.47 </TD> </TR>
  <TR> <TD align="right"> 11 </TD> <TD align="right">  50 </TD> <TD align="right"> 0.30 </TD> </TR>
  <TR> <TD align="right"> 12 </TD> <TD align="right">  55 </TD> <TD align="right"> 0.13 </TD> </TR>
  <TR> <TD align="right"> 13 </TD> <TD align="right"> 100 </TD> <TD align="right"> 0.32 </TD> </TR>
  <TR> <TD align="right"> 14 </TD> <TD align="right"> 105 </TD> <TD align="right"> 0.68 </TD> </TR>
  <TR> <TD align="right"> 15 </TD> <TD align="right"> 110 </TD> <TD align="right"> 0.15 </TD> </TR>
  <TR> <TD align="right"> 16 </TD> <TD align="right"> 115 </TD> <TD align="right"> 0.34 </TD> </TR>
  <TR> <TD align="right"> 17 </TD> <TD align="right"> 120 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 18 </TD> <TD align="right"> 125 </TD> <TD align="right"> 1.11 </TD> </TR>
  <TR> <TD align="right"> 19 </TD> <TD align="right"> 130 </TD> <TD align="right"> 1.83 </TD> </TR>
  <TR> <TD align="right"> 20 </TD> <TD align="right"> 135 </TD> <TD align="right"> 0.17 </TD> </TR>
  <TR> <TD align="right"> 21 </TD> <TD align="right"> 140 </TD> <TD align="right"> 0.17 </TD> </TR>
  <TR> <TD align="right"> 22 </TD> <TD align="right"> 145 </TD> <TD align="right"> 0.38 </TD> </TR>
  <TR> <TD align="right"> 23 </TD> <TD align="right"> 150 </TD> <TD align="right"> 0.26 </TD> </TR>
  <TR> <TD align="right"> 24 </TD> <TD align="right"> 155 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 25 </TD> <TD align="right"> 200 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 26 </TD> <TD align="right"> 205 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 27 </TD> <TD align="right"> 210 </TD> <TD align="right"> 1.13 </TD> </TR>
  <TR> <TD align="right"> 28 </TD> <TD align="right"> 215 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 29 </TD> <TD align="right"> 220 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 30 </TD> <TD align="right"> 225 </TD> <TD align="right"> 0.13 </TD> </TR>
  <TR> <TD align="right"> 31 </TD> <TD align="right"> 230 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 32 </TD> <TD align="right"> 235 </TD> <TD align="right"> 0.23 </TD> </TR>
  <TR> <TD align="right"> 33 </TD> <TD align="right"> 240 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 34 </TD> <TD align="right"> 245 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 35 </TD> <TD align="right"> 250 </TD> <TD align="right"> 1.55 </TD> </TR>
  <TR> <TD align="right"> 36 </TD> <TD align="right"> 255 </TD> <TD align="right"> 0.94 </TD> </TR>
  <TR> <TD align="right"> 37 </TD> <TD align="right"> 300 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 38 </TD> <TD align="right"> 305 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 39 </TD> <TD align="right"> 310 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 40 </TD> <TD align="right"> 315 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 41 </TD> <TD align="right"> 320 </TD> <TD align="right"> 0.21 </TD> </TR>
  <TR> <TD align="right"> 42 </TD> <TD align="right"> 325 </TD> <TD align="right"> 0.62 </TD> </TR>
  <TR> <TD align="right"> 43 </TD> <TD align="right"> 330 </TD> <TD align="right"> 1.62 </TD> </TR>
  <TR> <TD align="right"> 44 </TD> <TD align="right"> 335 </TD> <TD align="right"> 0.58 </TD> </TR>
  <TR> <TD align="right"> 45 </TD> <TD align="right"> 340 </TD> <TD align="right"> 0.49 </TD> </TR>
  <TR> <TD align="right"> 46 </TD> <TD align="right"> 345 </TD> <TD align="right"> 0.08 </TD> </TR>
  <TR> <TD align="right"> 47 </TD> <TD align="right"> 350 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 48 </TD> <TD align="right"> 355 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 49 </TD> <TD align="right"> 400 </TD> <TD align="right"> 1.19 </TD> </TR>
  <TR> <TD align="right"> 50 </TD> <TD align="right"> 405 </TD> <TD align="right"> 0.94 </TD> </TR>
  <TR> <TD align="right"> 51 </TD> <TD align="right"> 410 </TD> <TD align="right"> 2.57 </TD> </TR>
  <TR> <TD align="right"> 52 </TD> <TD align="right"> 415 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 53 </TD> <TD align="right"> 420 </TD> <TD align="right"> 0.34 </TD> </TR>
  <TR> <TD align="right"> 54 </TD> <TD align="right"> 425 </TD> <TD align="right"> 0.36 </TD> </TR>
  <TR> <TD align="right"> 55 </TD> <TD align="right"> 430 </TD> <TD align="right"> 4.11 </TD> </TR>
  <TR> <TD align="right"> 56 </TD> <TD align="right"> 435 </TD> <TD align="right"> 0.66 </TD> </TR>
  <TR> <TD align="right"> 57 </TD> <TD align="right"> 440 </TD> <TD align="right"> 3.49 </TD> </TR>
  <TR> <TD align="right"> 58 </TD> <TD align="right"> 445 </TD> <TD align="right"> 0.83 </TD> </TR>
  <TR> <TD align="right"> 59 </TD> <TD align="right"> 450 </TD> <TD align="right"> 3.11 </TD> </TR>
  <TR> <TD align="right"> 60 </TD> <TD align="right"> 455 </TD> <TD align="right"> 1.11 </TD> </TR>
  <TR> <TD align="right"> 61 </TD> <TD align="right"> 500 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 62 </TD> <TD align="right"> 505 </TD> <TD align="right"> 1.57 </TD> </TR>
  <TR> <TD align="right"> 63 </TD> <TD align="right"> 510 </TD> <TD align="right"> 3.00 </TD> </TR>
  <TR> <TD align="right"> 64 </TD> <TD align="right"> 515 </TD> <TD align="right"> 2.25 </TD> </TR>
  <TR> <TD align="right"> 65 </TD> <TD align="right"> 520 </TD> <TD align="right"> 3.32 </TD> </TR>
  <TR> <TD align="right"> 66 </TD> <TD align="right"> 525 </TD> <TD align="right"> 2.96 </TD> </TR>
  <TR> <TD align="right"> 67 </TD> <TD align="right"> 530 </TD> <TD align="right"> 2.09 </TD> </TR>
  <TR> <TD align="right"> 68 </TD> <TD align="right"> 535 </TD> <TD align="right"> 6.06 </TD> </TR>
  <TR> <TD align="right"> 69 </TD> <TD align="right"> 540 </TD> <TD align="right"> 16.02 </TD> </TR>
  <TR> <TD align="right"> 70 </TD> <TD align="right"> 545 </TD> <TD align="right"> 18.34 </TD> </TR>
  <TR> <TD align="right"> 71 </TD> <TD align="right"> 550 </TD> <TD align="right"> 39.45 </TD> </TR>
  <TR> <TD align="right"> 72 </TD> <TD align="right"> 555 </TD> <TD align="right"> 44.49 </TD> </TR>
  <TR> <TD align="right"> 73 </TD> <TD align="right"> 600 </TD> <TD align="right"> 31.49 </TD> </TR>
  <TR> <TD align="right"> 74 </TD> <TD align="right"> 605 </TD> <TD align="right"> 49.26 </TD> </TR>
  <TR> <TD align="right"> 75 </TD> <TD align="right"> 610 </TD> <TD align="right"> 53.77 </TD> </TR>
  <TR> <TD align="right"> 76 </TD> <TD align="right"> 615 </TD> <TD align="right"> 63.45 </TD> </TR>
  <TR> <TD align="right"> 77 </TD> <TD align="right"> 620 </TD> <TD align="right"> 49.96 </TD> </TR>
  <TR> <TD align="right"> 78 </TD> <TD align="right"> 625 </TD> <TD align="right"> 47.08 </TD> </TR>
  <TR> <TD align="right"> 79 </TD> <TD align="right"> 630 </TD> <TD align="right"> 52.15 </TD> </TR>
  <TR> <TD align="right"> 80 </TD> <TD align="right"> 635 </TD> <TD align="right"> 39.34 </TD> </TR>
  <TR> <TD align="right"> 81 </TD> <TD align="right"> 640 </TD> <TD align="right"> 44.02 </TD> </TR>
  <TR> <TD align="right"> 82 </TD> <TD align="right"> 645 </TD> <TD align="right"> 44.17 </TD> </TR>
  <TR> <TD align="right"> 83 </TD> <TD align="right"> 650 </TD> <TD align="right"> 37.36 </TD> </TR>
  <TR> <TD align="right"> 84 </TD> <TD align="right"> 655 </TD> <TD align="right"> 49.04 </TD> </TR>
  <TR> <TD align="right"> 85 </TD> <TD align="right"> 700 </TD> <TD align="right"> 43.81 </TD> </TR>
  <TR> <TD align="right"> 86 </TD> <TD align="right"> 705 </TD> <TD align="right"> 44.38 </TD> </TR>
  <TR> <TD align="right"> 87 </TD> <TD align="right"> 710 </TD> <TD align="right"> 50.51 </TD> </TR>
  <TR> <TD align="right"> 88 </TD> <TD align="right"> 715 </TD> <TD align="right"> 54.51 </TD> </TR>
  <TR> <TD align="right"> 89 </TD> <TD align="right"> 720 </TD> <TD align="right"> 49.92 </TD> </TR>
  <TR> <TD align="right"> 90 </TD> <TD align="right"> 725 </TD> <TD align="right"> 50.98 </TD> </TR>
  <TR> <TD align="right"> 91 </TD> <TD align="right"> 730 </TD> <TD align="right"> 55.68 </TD> </TR>
  <TR> <TD align="right"> 92 </TD> <TD align="right"> 735 </TD> <TD align="right"> 44.32 </TD> </TR>
  <TR> <TD align="right"> 93 </TD> <TD align="right"> 740 </TD> <TD align="right"> 52.26 </TD> </TR>
  <TR> <TD align="right"> 94 </TD> <TD align="right"> 745 </TD> <TD align="right"> 69.55 </TD> </TR>
  <TR> <TD align="right"> 95 </TD> <TD align="right"> 750 </TD> <TD align="right"> 57.85 </TD> </TR>
  <TR> <TD align="right"> 96 </TD> <TD align="right"> 755 </TD> <TD align="right"> 56.15 </TD> </TR>
  <TR> <TD align="right"> 97 </TD> <TD align="right"> 800 </TD> <TD align="right"> 73.38 </TD> </TR>
  <TR> <TD align="right"> 98 </TD> <TD align="right"> 805 </TD> <TD align="right"> 68.21 </TD> </TR>
  <TR> <TD align="right"> 99 </TD> <TD align="right"> 810 </TD> <TD align="right"> 129.43 </TD> </TR>
  <TR> <TD align="right"> 100 </TD> <TD align="right"> 815 </TD> <TD align="right"> 157.53 </TD> </TR>
  <TR> <TD align="right"> 101 </TD> <TD align="right"> 820 </TD> <TD align="right"> 171.15 </TD> </TR>
  <TR> <TD align="right"> 102 </TD> <TD align="right"> 825 </TD> <TD align="right"> 155.40 </TD> </TR>
  <TR> <TD align="right"> 103 </TD> <TD align="right"> 830 </TD> <TD align="right"> 177.30 </TD> </TR>
  <TR> <TD align="right"> 104 </TD> <TD align="right"> 835 </TD> <TD align="right"> 206.17 </TD> </TR>
  <TR> <TD align="right"> 105 </TD> <TD align="right"> 840 </TD> <TD align="right"> 195.92 </TD> </TR>
  <TR> <TD align="right"> 106 </TD> <TD align="right"> 845 </TD> <TD align="right"> 179.57 </TD> </TR>
  <TR> <TD align="right"> 107 </TD> <TD align="right"> 850 </TD> <TD align="right"> 183.40 </TD> </TR>
  <TR> <TD align="right"> 108 </TD> <TD align="right"> 855 </TD> <TD align="right"> 167.02 </TD> </TR>
  <TR> <TD align="right"> 109 </TD> <TD align="right"> 900 </TD> <TD align="right"> 143.45 </TD> </TR>
  <TR> <TD align="right"> 110 </TD> <TD align="right"> 905 </TD> <TD align="right"> 124.04 </TD> </TR>
  <TR> <TD align="right"> 111 </TD> <TD align="right"> 910 </TD> <TD align="right"> 109.11 </TD> </TR>
  <TR> <TD align="right"> 112 </TD> <TD align="right"> 915 </TD> <TD align="right"> 108.11 </TD> </TR>
  <TR> <TD align="right"> 113 </TD> <TD align="right"> 920 </TD> <TD align="right"> 103.72 </TD> </TR>
  <TR> <TD align="right"> 114 </TD> <TD align="right"> 925 </TD> <TD align="right"> 95.96 </TD> </TR>
  <TR> <TD align="right"> 115 </TD> <TD align="right"> 930 </TD> <TD align="right"> 66.21 </TD> </TR>
  <TR> <TD align="right"> 116 </TD> <TD align="right"> 935 </TD> <TD align="right"> 45.23 </TD> </TR>
  <TR> <TD align="right"> 117 </TD> <TD align="right"> 940 </TD> <TD align="right"> 24.79 </TD> </TR>
  <TR> <TD align="right"> 118 </TD> <TD align="right"> 945 </TD> <TD align="right"> 38.75 </TD> </TR>
  <TR> <TD align="right"> 119 </TD> <TD align="right"> 950 </TD> <TD align="right"> 34.98 </TD> </TR>
  <TR> <TD align="right"> 120 </TD> <TD align="right"> 955 </TD> <TD align="right"> 21.06 </TD> </TR>
  <TR> <TD align="right"> 121 </TD> <TD align="right"> 1000 </TD> <TD align="right"> 40.57 </TD> </TR>
  <TR> <TD align="right"> 122 </TD> <TD align="right"> 1005 </TD> <TD align="right"> 26.98 </TD> </TR>
  <TR> <TD align="right"> 123 </TD> <TD align="right"> 1010 </TD> <TD align="right"> 42.42 </TD> </TR>
  <TR> <TD align="right"> 124 </TD> <TD align="right"> 1015 </TD> <TD align="right"> 52.66 </TD> </TR>
  <TR> <TD align="right"> 125 </TD> <TD align="right"> 1020 </TD> <TD align="right"> 38.92 </TD> </TR>
  <TR> <TD align="right"> 126 </TD> <TD align="right"> 1025 </TD> <TD align="right"> 50.79 </TD> </TR>
  <TR> <TD align="right"> 127 </TD> <TD align="right"> 1030 </TD> <TD align="right"> 44.28 </TD> </TR>
  <TR> <TD align="right"> 128 </TD> <TD align="right"> 1035 </TD> <TD align="right"> 37.42 </TD> </TR>
  <TR> <TD align="right"> 129 </TD> <TD align="right"> 1040 </TD> <TD align="right"> 34.70 </TD> </TR>
  <TR> <TD align="right"> 130 </TD> <TD align="right"> 1045 </TD> <TD align="right"> 28.34 </TD> </TR>
  <TR> <TD align="right"> 131 </TD> <TD align="right"> 1050 </TD> <TD align="right"> 25.09 </TD> </TR>
  <TR> <TD align="right"> 132 </TD> <TD align="right"> 1055 </TD> <TD align="right"> 31.94 </TD> </TR>
  <TR> <TD align="right"> 133 </TD> <TD align="right"> 1100 </TD> <TD align="right"> 31.36 </TD> </TR>
  <TR> <TD align="right"> 134 </TD> <TD align="right"> 1105 </TD> <TD align="right"> 29.68 </TD> </TR>
  <TR> <TD align="right"> 135 </TD> <TD align="right"> 1110 </TD> <TD align="right"> 21.32 </TD> </TR>
  <TR> <TD align="right"> 136 </TD> <TD align="right"> 1115 </TD> <TD align="right"> 25.55 </TD> </TR>
  <TR> <TD align="right"> 137 </TD> <TD align="right"> 1120 </TD> <TD align="right"> 28.38 </TD> </TR>
  <TR> <TD align="right"> 138 </TD> <TD align="right"> 1125 </TD> <TD align="right"> 26.47 </TD> </TR>
  <TR> <TD align="right"> 139 </TD> <TD align="right"> 1130 </TD> <TD align="right"> 33.43 </TD> </TR>
  <TR> <TD align="right"> 140 </TD> <TD align="right"> 1135 </TD> <TD align="right"> 49.98 </TD> </TR>
  <TR> <TD align="right"> 141 </TD> <TD align="right"> 1140 </TD> <TD align="right"> 42.04 </TD> </TR>
  <TR> <TD align="right"> 142 </TD> <TD align="right"> 1145 </TD> <TD align="right"> 44.60 </TD> </TR>
  <TR> <TD align="right"> 143 </TD> <TD align="right"> 1150 </TD> <TD align="right"> 46.04 </TD> </TR>
  <TR> <TD align="right"> 144 </TD> <TD align="right"> 1155 </TD> <TD align="right"> 59.19 </TD> </TR>
  <TR> <TD align="right"> 145 </TD> <TD align="right"> 1200 </TD> <TD align="right"> 63.87 </TD> </TR>
  <TR> <TD align="right"> 146 </TD> <TD align="right"> 1205 </TD> <TD align="right"> 87.70 </TD> </TR>
  <TR> <TD align="right"> 147 </TD> <TD align="right"> 1210 </TD> <TD align="right"> 94.85 </TD> </TR>
  <TR> <TD align="right"> 148 </TD> <TD align="right"> 1215 </TD> <TD align="right"> 92.77 </TD> </TR>
  <TR> <TD align="right"> 149 </TD> <TD align="right"> 1220 </TD> <TD align="right"> 63.40 </TD> </TR>
  <TR> <TD align="right"> 150 </TD> <TD align="right"> 1225 </TD> <TD align="right"> 50.17 </TD> </TR>
  <TR> <TD align="right"> 151 </TD> <TD align="right"> 1230 </TD> <TD align="right"> 54.47 </TD> </TR>
  <TR> <TD align="right"> 152 </TD> <TD align="right"> 1235 </TD> <TD align="right"> 32.42 </TD> </TR>
  <TR> <TD align="right"> 153 </TD> <TD align="right"> 1240 </TD> <TD align="right"> 26.53 </TD> </TR>
  <TR> <TD align="right"> 154 </TD> <TD align="right"> 1245 </TD> <TD align="right"> 37.74 </TD> </TR>
  <TR> <TD align="right"> 155 </TD> <TD align="right"> 1250 </TD> <TD align="right"> 45.06 </TD> </TR>
  <TR> <TD align="right"> 156 </TD> <TD align="right"> 1255 </TD> <TD align="right"> 67.28 </TD> </TR>
  <TR> <TD align="right"> 157 </TD> <TD align="right"> 1300 </TD> <TD align="right"> 42.34 </TD> </TR>
  <TR> <TD align="right"> 158 </TD> <TD align="right"> 1305 </TD> <TD align="right"> 39.89 </TD> </TR>
  <TR> <TD align="right"> 159 </TD> <TD align="right"> 1310 </TD> <TD align="right"> 43.26 </TD> </TR>
  <TR> <TD align="right"> 160 </TD> <TD align="right"> 1315 </TD> <TD align="right"> 40.98 </TD> </TR>
  <TR> <TD align="right"> 161 </TD> <TD align="right"> 1320 </TD> <TD align="right"> 46.25 </TD> </TR>
  <TR> <TD align="right"> 162 </TD> <TD align="right"> 1325 </TD> <TD align="right"> 56.43 </TD> </TR>
  <TR> <TD align="right"> 163 </TD> <TD align="right"> 1330 </TD> <TD align="right"> 42.75 </TD> </TR>
  <TR> <TD align="right"> 164 </TD> <TD align="right"> 1335 </TD> <TD align="right"> 25.13 </TD> </TR>
  <TR> <TD align="right"> 165 </TD> <TD align="right"> 1340 </TD> <TD align="right"> 39.96 </TD> </TR>
  <TR> <TD align="right"> 166 </TD> <TD align="right"> 1345 </TD> <TD align="right"> 53.55 </TD> </TR>
  <TR> <TD align="right"> 167 </TD> <TD align="right"> 1350 </TD> <TD align="right"> 47.32 </TD> </TR>
  <TR> <TD align="right"> 168 </TD> <TD align="right"> 1355 </TD> <TD align="right"> 60.81 </TD> </TR>
  <TR> <TD align="right"> 169 </TD> <TD align="right"> 1400 </TD> <TD align="right"> 55.75 </TD> </TR>
  <TR> <TD align="right"> 170 </TD> <TD align="right"> 1405 </TD> <TD align="right"> 51.96 </TD> </TR>
  <TR> <TD align="right"> 171 </TD> <TD align="right"> 1410 </TD> <TD align="right"> 43.58 </TD> </TR>
  <TR> <TD align="right"> 172 </TD> <TD align="right"> 1415 </TD> <TD align="right"> 48.70 </TD> </TR>
  <TR> <TD align="right"> 173 </TD> <TD align="right"> 1420 </TD> <TD align="right"> 35.47 </TD> </TR>
  <TR> <TD align="right"> 174 </TD> <TD align="right"> 1425 </TD> <TD align="right"> 37.55 </TD> </TR>
  <TR> <TD align="right"> 175 </TD> <TD align="right"> 1430 </TD> <TD align="right"> 41.85 </TD> </TR>
  <TR> <TD align="right"> 176 </TD> <TD align="right"> 1435 </TD> <TD align="right"> 27.51 </TD> </TR>
  <TR> <TD align="right"> 177 </TD> <TD align="right"> 1440 </TD> <TD align="right"> 17.11 </TD> </TR>
  <TR> <TD align="right"> 178 </TD> <TD align="right"> 1445 </TD> <TD align="right"> 26.08 </TD> </TR>
  <TR> <TD align="right"> 179 </TD> <TD align="right"> 1450 </TD> <TD align="right"> 43.62 </TD> </TR>
  <TR> <TD align="right"> 180 </TD> <TD align="right"> 1455 </TD> <TD align="right"> 43.77 </TD> </TR>
  <TR> <TD align="right"> 181 </TD> <TD align="right"> 1500 </TD> <TD align="right"> 30.02 </TD> </TR>
  <TR> <TD align="right"> 182 </TD> <TD align="right"> 1505 </TD> <TD align="right"> 36.08 </TD> </TR>
  <TR> <TD align="right"> 183 </TD> <TD align="right"> 1510 </TD> <TD align="right"> 35.49 </TD> </TR>
  <TR> <TD align="right"> 184 </TD> <TD align="right"> 1515 </TD> <TD align="right"> 38.85 </TD> </TR>
  <TR> <TD align="right"> 185 </TD> <TD align="right"> 1520 </TD> <TD align="right"> 45.96 </TD> </TR>
  <TR> <TD align="right"> 186 </TD> <TD align="right"> 1525 </TD> <TD align="right"> 47.75 </TD> </TR>
  <TR> <TD align="right"> 187 </TD> <TD align="right"> 1530 </TD> <TD align="right"> 48.13 </TD> </TR>
  <TR> <TD align="right"> 188 </TD> <TD align="right"> 1535 </TD> <TD align="right"> 65.32 </TD> </TR>
  <TR> <TD align="right"> 189 </TD> <TD align="right"> 1540 </TD> <TD align="right"> 82.91 </TD> </TR>
  <TR> <TD align="right"> 190 </TD> <TD align="right"> 1545 </TD> <TD align="right"> 98.66 </TD> </TR>
  <TR> <TD align="right"> 191 </TD> <TD align="right"> 1550 </TD> <TD align="right"> 102.11 </TD> </TR>
  <TR> <TD align="right"> 192 </TD> <TD align="right"> 1555 </TD> <TD align="right"> 83.96 </TD> </TR>
  <TR> <TD align="right"> 193 </TD> <TD align="right"> 1600 </TD> <TD align="right"> 62.13 </TD> </TR>
  <TR> <TD align="right"> 194 </TD> <TD align="right"> 1605 </TD> <TD align="right"> 64.13 </TD> </TR>
  <TR> <TD align="right"> 195 </TD> <TD align="right"> 1610 </TD> <TD align="right"> 74.55 </TD> </TR>
  <TR> <TD align="right"> 196 </TD> <TD align="right"> 1615 </TD> <TD align="right"> 63.17 </TD> </TR>
  <TR> <TD align="right"> 197 </TD> <TD align="right"> 1620 </TD> <TD align="right"> 56.91 </TD> </TR>
  <TR> <TD align="right"> 198 </TD> <TD align="right"> 1625 </TD> <TD align="right"> 59.77 </TD> </TR>
  <TR> <TD align="right"> 199 </TD> <TD align="right"> 1630 </TD> <TD align="right"> 43.87 </TD> </TR>
  <TR> <TD align="right"> 200 </TD> <TD align="right"> 1635 </TD> <TD align="right"> 38.57 </TD> </TR>
  <TR> <TD align="right"> 201 </TD> <TD align="right"> 1640 </TD> <TD align="right"> 44.66 </TD> </TR>
  <TR> <TD align="right"> 202 </TD> <TD align="right"> 1645 </TD> <TD align="right"> 45.45 </TD> </TR>
  <TR> <TD align="right"> 203 </TD> <TD align="right"> 1650 </TD> <TD align="right"> 46.21 </TD> </TR>
  <TR> <TD align="right"> 204 </TD> <TD align="right"> 1655 </TD> <TD align="right"> 43.68 </TD> </TR>
  <TR> <TD align="right"> 205 </TD> <TD align="right"> 1700 </TD> <TD align="right"> 46.62 </TD> </TR>
  <TR> <TD align="right"> 206 </TD> <TD align="right"> 1705 </TD> <TD align="right"> 56.30 </TD> </TR>
  <TR> <TD align="right"> 207 </TD> <TD align="right"> 1710 </TD> <TD align="right"> 50.72 </TD> </TR>
  <TR> <TD align="right"> 208 </TD> <TD align="right"> 1715 </TD> <TD align="right"> 61.23 </TD> </TR>
  <TR> <TD align="right"> 209 </TD> <TD align="right"> 1720 </TD> <TD align="right"> 72.72 </TD> </TR>
  <TR> <TD align="right"> 210 </TD> <TD align="right"> 1725 </TD> <TD align="right"> 78.94 </TD> </TR>
  <TR> <TD align="right"> 211 </TD> <TD align="right"> 1730 </TD> <TD align="right"> 68.94 </TD> </TR>
  <TR> <TD align="right"> 212 </TD> <TD align="right"> 1735 </TD> <TD align="right"> 59.66 </TD> </TR>
  <TR> <TD align="right"> 213 </TD> <TD align="right"> 1740 </TD> <TD align="right"> 75.09 </TD> </TR>
  <TR> <TD align="right"> 214 </TD> <TD align="right"> 1745 </TD> <TD align="right"> 56.51 </TD> </TR>
  <TR> <TD align="right"> 215 </TD> <TD align="right"> 1750 </TD> <TD align="right"> 34.77 </TD> </TR>
  <TR> <TD align="right"> 216 </TD> <TD align="right"> 1755 </TD> <TD align="right"> 37.45 </TD> </TR>
  <TR> <TD align="right"> 217 </TD> <TD align="right"> 1800 </TD> <TD align="right"> 40.68 </TD> </TR>
  <TR> <TD align="right"> 218 </TD> <TD align="right"> 1805 </TD> <TD align="right"> 58.02 </TD> </TR>
  <TR> <TD align="right"> 219 </TD> <TD align="right"> 1810 </TD> <TD align="right"> 74.70 </TD> </TR>
  <TR> <TD align="right"> 220 </TD> <TD align="right"> 1815 </TD> <TD align="right"> 85.32 </TD> </TR>
  <TR> <TD align="right"> 221 </TD> <TD align="right"> 1820 </TD> <TD align="right"> 59.26 </TD> </TR>
  <TR> <TD align="right"> 222 </TD> <TD align="right"> 1825 </TD> <TD align="right"> 67.77 </TD> </TR>
  <TR> <TD align="right"> 223 </TD> <TD align="right"> 1830 </TD> <TD align="right"> 77.70 </TD> </TR>
  <TR> <TD align="right"> 224 </TD> <TD align="right"> 1835 </TD> <TD align="right"> 74.25 </TD> </TR>
  <TR> <TD align="right"> 225 </TD> <TD align="right"> 1840 </TD> <TD align="right"> 85.34 </TD> </TR>
  <TR> <TD align="right"> 226 </TD> <TD align="right"> 1845 </TD> <TD align="right"> 99.45 </TD> </TR>
  <TR> <TD align="right"> 227 </TD> <TD align="right"> 1850 </TD> <TD align="right"> 86.58 </TD> </TR>
  <TR> <TD align="right"> 228 </TD> <TD align="right"> 1855 </TD> <TD align="right"> 85.60 </TD> </TR>
  <TR> <TD align="right"> 229 </TD> <TD align="right"> 1900 </TD> <TD align="right"> 84.87 </TD> </TR>
  <TR> <TD align="right"> 230 </TD> <TD align="right"> 1905 </TD> <TD align="right"> 77.83 </TD> </TR>
  <TR> <TD align="right"> 231 </TD> <TD align="right"> 1910 </TD> <TD align="right"> 58.04 </TD> </TR>
  <TR> <TD align="right"> 232 </TD> <TD align="right"> 1915 </TD> <TD align="right"> 53.36 </TD> </TR>
  <TR> <TD align="right"> 233 </TD> <TD align="right"> 1920 </TD> <TD align="right"> 36.32 </TD> </TR>
  <TR> <TD align="right"> 234 </TD> <TD align="right"> 1925 </TD> <TD align="right"> 20.72 </TD> </TR>
  <TR> <TD align="right"> 235 </TD> <TD align="right"> 1930 </TD> <TD align="right"> 27.40 </TD> </TR>
  <TR> <TD align="right"> 236 </TD> <TD align="right"> 1935 </TD> <TD align="right"> 40.02 </TD> </TR>
  <TR> <TD align="right"> 237 </TD> <TD align="right"> 1940 </TD> <TD align="right"> 30.21 </TD> </TR>
  <TR> <TD align="right"> 238 </TD> <TD align="right"> 1945 </TD> <TD align="right"> 25.55 </TD> </TR>
  <TR> <TD align="right"> 239 </TD> <TD align="right"> 1950 </TD> <TD align="right"> 45.66 </TD> </TR>
  <TR> <TD align="right"> 240 </TD> <TD align="right"> 1955 </TD> <TD align="right"> 33.53 </TD> </TR>
  <TR> <TD align="right"> 241 </TD> <TD align="right"> 2000 </TD> <TD align="right"> 19.62 </TD> </TR>
  <TR> <TD align="right"> 242 </TD> <TD align="right"> 2005 </TD> <TD align="right"> 19.02 </TD> </TR>
  <TR> <TD align="right"> 243 </TD> <TD align="right"> 2010 </TD> <TD align="right"> 19.34 </TD> </TR>
  <TR> <TD align="right"> 244 </TD> <TD align="right"> 2015 </TD> <TD align="right"> 33.34 </TD> </TR>
  <TR> <TD align="right"> 245 </TD> <TD align="right"> 2020 </TD> <TD align="right"> 26.81 </TD> </TR>
  <TR> <TD align="right"> 246 </TD> <TD align="right"> 2025 </TD> <TD align="right"> 21.17 </TD> </TR>
  <TR> <TD align="right"> 247 </TD> <TD align="right"> 2030 </TD> <TD align="right"> 27.30 </TD> </TR>
  <TR> <TD align="right"> 248 </TD> <TD align="right"> 2035 </TD> <TD align="right"> 21.34 </TD> </TR>
  <TR> <TD align="right"> 249 </TD> <TD align="right"> 2040 </TD> <TD align="right"> 19.55 </TD> </TR>
  <TR> <TD align="right"> 250 </TD> <TD align="right"> 2045 </TD> <TD align="right"> 21.32 </TD> </TR>
  <TR> <TD align="right"> 251 </TD> <TD align="right"> 2050 </TD> <TD align="right"> 32.30 </TD> </TR>
  <TR> <TD align="right"> 252 </TD> <TD align="right"> 2055 </TD> <TD align="right"> 20.15 </TD> </TR>
  <TR> <TD align="right"> 253 </TD> <TD align="right"> 2100 </TD> <TD align="right"> 15.94 </TD> </TR>
  <TR> <TD align="right"> 254 </TD> <TD align="right"> 2105 </TD> <TD align="right"> 17.23 </TD> </TR>
  <TR> <TD align="right"> 255 </TD> <TD align="right"> 2110 </TD> <TD align="right"> 23.45 </TD> </TR>
  <TR> <TD align="right"> 256 </TD> <TD align="right"> 2115 </TD> <TD align="right"> 19.25 </TD> </TR>
  <TR> <TD align="right"> 257 </TD> <TD align="right"> 2120 </TD> <TD align="right"> 12.45 </TD> </TR>
  <TR> <TD align="right"> 258 </TD> <TD align="right"> 2125 </TD> <TD align="right"> 8.02 </TD> </TR>
  <TR> <TD align="right"> 259 </TD> <TD align="right"> 2130 </TD> <TD align="right"> 14.66 </TD> </TR>
  <TR> <TD align="right"> 260 </TD> <TD align="right"> 2135 </TD> <TD align="right"> 16.30 </TD> </TR>
  <TR> <TD align="right"> 261 </TD> <TD align="right"> 2140 </TD> <TD align="right"> 8.68 </TD> </TR>
  <TR> <TD align="right"> 262 </TD> <TD align="right"> 2145 </TD> <TD align="right"> 7.79 </TD> </TR>
  <TR> <TD align="right"> 263 </TD> <TD align="right"> 2150 </TD> <TD align="right"> 8.13 </TD> </TR>
  <TR> <TD align="right"> 264 </TD> <TD align="right"> 2155 </TD> <TD align="right"> 2.62 </TD> </TR>
  <TR> <TD align="right"> 265 </TD> <TD align="right"> 2200 </TD> <TD align="right"> 1.45 </TD> </TR>
  <TR> <TD align="right"> 266 </TD> <TD align="right"> 2205 </TD> <TD align="right"> 3.68 </TD> </TR>
  <TR> <TD align="right"> 267 </TD> <TD align="right"> 2210 </TD> <TD align="right"> 4.81 </TD> </TR>
  <TR> <TD align="right"> 268 </TD> <TD align="right"> 2215 </TD> <TD align="right"> 8.51 </TD> </TR>
  <TR> <TD align="right"> 269 </TD> <TD align="right"> 2220 </TD> <TD align="right"> 7.08 </TD> </TR>
  <TR> <TD align="right"> 270 </TD> <TD align="right"> 2225 </TD> <TD align="right"> 8.70 </TD> </TR>
  <TR> <TD align="right"> 271 </TD> <TD align="right"> 2230 </TD> <TD align="right"> 9.75 </TD> </TR>
  <TR> <TD align="right"> 272 </TD> <TD align="right"> 2235 </TD> <TD align="right"> 2.21 </TD> </TR>
  <TR> <TD align="right"> 273 </TD> <TD align="right"> 2240 </TD> <TD align="right"> 0.32 </TD> </TR>
  <TR> <TD align="right"> 274 </TD> <TD align="right"> 2245 </TD> <TD align="right"> 0.11 </TD> </TR>
  <TR> <TD align="right"> 275 </TD> <TD align="right"> 2250 </TD> <TD align="right"> 1.60 </TD> </TR>
  <TR> <TD align="right"> 276 </TD> <TD align="right"> 2255 </TD> <TD align="right"> 4.60 </TD> </TR>
  <TR> <TD align="right"> 277 </TD> <TD align="right"> 2300 </TD> <TD align="right"> 3.30 </TD> </TR>
  <TR> <TD align="right"> 278 </TD> <TD align="right"> 2305 </TD> <TD align="right"> 2.85 </TD> </TR>
  <TR> <TD align="right"> 279 </TD> <TD align="right"> 2310 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 280 </TD> <TD align="right"> 2315 </TD> <TD align="right"> 0.83 </TD> </TR>
  <TR> <TD align="right"> 281 </TD> <TD align="right"> 2320 </TD> <TD align="right"> 0.96 </TD> </TR>
  <TR> <TD align="right"> 282 </TD> <TD align="right"> 2325 </TD> <TD align="right"> 1.58 </TD> </TR>
  <TR> <TD align="right"> 283 </TD> <TD align="right"> 2330 </TD> <TD align="right"> 2.60 </TD> </TR>
  <TR> <TD align="right"> 284 </TD> <TD align="right"> 2335 </TD> <TD align="right"> 4.70 </TD> </TR>
  <TR> <TD align="right"> 285 </TD> <TD align="right"> 2340 </TD> <TD align="right"> 3.30 </TD> </TR>
  <TR> <TD align="right"> 286 </TD> <TD align="right"> 2345 </TD> <TD align="right"> 0.64 </TD> </TR>
  <TR> <TD align="right"> 287 </TD> <TD align="right"> 2350 </TD> <TD align="right"> 0.23 </TD> </TR>
  <TR> <TD align="right"> 288 </TD> <TD align="right"> 2355 </TD> <TD align="right"> 1.08 </TD> </TR>
   </TABLE>


------ END OF DOCUMENT -------

