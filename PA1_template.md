# Reproducable Research Peer Assessment 1


##Loading and Preprocessing the data

1. Load the data (i.e. read.csv())  

2. Process/transform the data (if necessary) into a format suitable for your analysis  


```r
#
# Set the right working directory
#
#setwd("Github/RepData_PeerAssessment1")

#
# Unzip the data
#
zipdir<-tempfile()
dir.create(zipdir, showWarnings=T)

if (!file.exists(zipdir)) 
  stop(paste0("Failed to create",zipdir))

unzip("activity.zip", exdir=zipdir)

file<-list.files(zipdir)[1]

df<-read.csv(paste0(zipdir,'/',file))

#
# Cleanup the unzipped files
#
unlink(zipdir,recursive=T)

#
# remove incomplete cases (for now)
#
cdf<-df[which(complete.cases(df)),]
```

##What is mean total number of steps taken per day?



```r
# Compute the total # steps/day (ignore NA )

total_steps_per_day<-aggregate(cdf$steps, list(cdf$date), sum)

names(total_steps_per_day)<-c("Date", "Steps")
```

1. Make a histogram of the total number of steps taken each day



```r
hist(total_steps_per_day$Steps, xlab="Steps/Day", main="Histogram of number of steps per day")
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

2. Calculate the mean and median of the total of number of steps/day


```r
mean(total_steps_per_day$Steps,na.rm=T)
```

```
## [1] 10766.19
```

```r
median(total_steps_per_day$Steps,na.rm=T)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
#
# Calculate the average number of steps per interval
#
avg_steps_per_interval<-aggregate(cdf$steps,list(cdf$interval),mean, na.rm=T)

names(avg_steps_per_interval)<-c("Interval", "Steps")

#
# Plot it
#
plot(avg_steps_per_interval,ylab="Average #Steps in this interval", type="l")
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
avg_steps_per_interval[which(avg_steps_per_interval$Steps==max(avg_steps_per_interval$Steps)),]
```

```
##     Interval    Steps
## 104      835 206.1698
```


## Incomplete cases in the dataset  
  
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
nrow(df)-length(which(complete.cases(df)))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated  
  
If the case is incomplete, use the mean for thatinterval for
the specific variable (here, only Steps).

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
#
# Define a function that given an
# index in the df dataframe, returns the mean for the interval associated withh
# the index
myfunction<-function(index){
 
  (avg_steps_per_interval[avg_steps_per_interval$Interval==df[index,3],2])
 
}

l<-sapply(which(!complete.cases(df)),myfunction)

#
#now apply to this to the incomplete cases of df
#

df[which(!complete.cases(df)),1] <- l
```


Redo the first part

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
#
#Compute the total # steps/day 
#
total_steps_per_day<-aggregate(df$steps, list(df$date), sum)

names(total_steps_per_day)<-c("Date", "Steps")

# Plot the histogram of total steps per day
#
hist(total_steps_per_day$Steps, xlab="Steps/Day", main="Histogram of number of steps per day")
```

![](./PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

Calculate the mean of the total of number of steps/day

```r
mean(total_steps_per_day$Steps,na.rm=T)
```

```
## [1] 10766.19
```

Calculate the median of the total number of steps/day


```r
median(total_steps_per_day$Steps,na.rm=T)
```

```
## [1] 10766.19
```

What is the impact of imputing missing data on the estimates of the total daily number of steps?
  
The impact is not significant.  
  
    
##Are there differences in activity patterns between weekdays and weekends?      
    

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day



```r
#Set locale to English
#
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
#
# Add weekday column
#
df$weekday<-as.character(weekdays(as.Date(df$date)))


#
# Transform
#
weekends<-which(df$weekday=="Saturday" | df$weekday=="Sunday")
weekdays<-which(df$weekday!="Saturday" & df$weekday!="Sunday")
df[weekends,4]<-as.character("weekend")
df[weekdays,4]<-as.character("weekday")
df$weekday<-as.factor(df$weekday)

#Calculate

fdf<-df[weekends,]
wdf<-df[weekdays,]

avg_steps_int_weekend<-aggregate(fdf$steps,list(fdf$interval), mean)
avg_steps_int_weekday<-aggregate(wdf$steps,list(wdf$interval), mean)

par(mfrow=c(2,1))

plot(avg_steps_int_weekend,type="l",ylab="Avg. #of steps in this interval", xlab="Interval", main="Weekend")
plot(avg_steps_int_weekday,type="l", ylab="Avg. #of steps in this interval", xlab="Interval", main="Weekdays")
```

![](./PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

Conclusion: there are differences in the patterns between weekdays and weekend days. 




