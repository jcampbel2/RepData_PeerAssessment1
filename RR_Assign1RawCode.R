## Code snips to be included into markdown for Assignment 1 - Repro Research
##
##load Libs
library("plyr")
library("knitr")
library("ggplot2")

## Load of data

##This is hardcoded to location on my PC, ideally this would be an internet location
setwd("C:/Users/james_000/DataScPrj/RepResearch/Assign1/RepData_PeerAssessment1")
actdata <- read.csv("./data/activity.csv")
actdata$date <- as.POSIXct(actdata$date)

##question 1
## create summary data frame by date
stepsbyday <- ddply(actdata[!is.na(actdata$steps),],.(date), 
           summarize,totsteps=sum(steps))

##histogram plot
p1 <- ggplot(stepsbyday,aes(totsteps, fill="red"))
p1 <- p1 +labs(title="Histogram of Number of steps per day", y="Counts Days", x= "Total Steps in 1 day")
p1 <- p1 + geom_histogram(binwidth=1000)

stepmean <- mean(stepsbyday$totsteps)
stepmedian <- median(stepsbyday$totsteps)
rowname <- c("Mean","Median")
Values <- c(stepmean,stepmedian)
xtab1 <-xtable(as.data.frame(x=Values, row.names=rowname ) )  
print(xtab1, type="html")
