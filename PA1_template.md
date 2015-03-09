# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

The activity.zip file is unzipped, the zipped csv file is read into R, and then pre-processed by converting the date to POSIXct/POSIXt format using the ymd function in the lubridate package. Note: the activity.zip file must be in the working directory.




```r
library(lubridate)
unzip("./activity.zip",overwrite=TRUE)
activity <- read.csv("./activity.csv",header=TRUE,na.strings="NA",stringsAsFactors=FALSE,colClasses=c("numeric","character","numeric"))
activity$dateFactor <- as.factor(activity$date)
activity$date <- ymd(activity$date)
```

## What is mean total number of steps taken per day?
The activity dateframe is subsetted to include only those values with steps measured (i.e., non-NA values). The total steps per day are calculated and then dislayed as a histogram below, along with the mean and median total steps per day.


```r
library(ggplot2)
activeDays <- activity[!is.na(activity$steps),]
totalSteps <- tapply(activeDays$steps,activeDays$dateFactor,sum)
totalSteps <- as.data.frame(totalSteps)
totalSteps$date <- as.factor(row.names(totalSteps))
names(totalSteps) <- c("totSteps","date")
stepHisto <- ggplot(totalSteps,aes(x=totSteps)) + geom_histogram(binwidth=1000,color="black",fill="dodgerblue4") + labs(x="Total Steps Taken Per Day",y="Frequency")
print(stepHisto)
```

![](PA1_template_files/figure-html/meanCalcs-1.png) 

```r
meanTotSteps <- mean(totalSteps$totSteps,na.rm=TRUE)
meanTotSteps
```

```
## [1] 10766.19
```

```r
medianTotSteps <- median(totalSteps$totSteps,na.rm=TRUE)
medianTotSteps
```

```
## [1] 10765
```

The mean total number of steps taken per day is 1.0766189\times 10^{4} and the median total number of steps taken per day is 1.0765\times 10^{4}.

## What is the average daily activity pattern?

```r
activeDays$intFactor <- factor(activeDays$interval)
meanStepPattern <- as.data.frame(tapply(activeDays$steps,activeDays$intFactor,mean))
meanStepPattern$interval <- as.numeric(row.names(meanStepPattern))
names(meanStepPattern) <- c("meanSteps","interval")
maxInt <- meanStepPattern$interval[which.max(meanStepPattern$meanSteps)]
pattern <- ggplot(meanStepPattern,aes(x=interval,y=meanSteps)) + geom_line(color="dodgerblue4") + labs(x="Time Interval",y="Mean Steps",title="Mean Steps Taken as a Function of Time of Day")
print(pattern)
```

![](PA1_template_files/figure-html/dailyPattern-1.png) 

```r
maxInt
```

```
## [1] 835
```
The 5-minute time interval during which, on average, the most steps are taken is 835.

## Inputing missing values



## Are there differences in activity patterns between weekdays and weekends?
