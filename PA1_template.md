# Reproducible Research: Peer Assessment 1
Luigi Pistis  
14 maggio 2015  


## Loading and preprocessing the data

```r
#As first step let's import data
activity <- read.csv("~/Downloads/activity.csv")
```

## What is mean total number of steps taken per day?


```r
#aggregate the number of step by day 
agg<-aggregate(activity$steps,by = list(day=activity$date), FUN=sum)

#plot
library(ggplot2)

ggplot(agg,aes(x=x))+geom_histogram()+
    xlab("Total number of steps")
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
#calculate mean and median
mean(agg$x)
```

```
## [1] NA
```

```r
median(agg$x)
```

```
## [1] NA
```


## What is the average daily activity pattern?

```r
#aggregate the number of step by interval 
#to calculate the average I have to use na.rm=TRUE otherwise the calculation returns NA

agg1<-aggregate(activity$steps,by = list(interval=activity$interval), FUN=mean,na.rm=TRUE)

#plot 
plot(agg1$x~agg1$interval,xlab="interval",ylab="Steps",type="l")


abline(v =agg1[agg1$x==max(agg1$x),1],col="red",lty=2)
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
#maximum value
print(paste0("Max Value: x=",agg1[agg1$x==max(agg1$x),1],";  ","y=",agg1[agg1$x==max(agg1$x),2]))
```

```
## [1] "Max Value: x=835;  y=206.169811320755"
```


## Imputing missing values

```r
#first i duplicate the dataframe with all data
activityfillNA<-activity

#then I replace all NA with the average of the correspondent inverval (the averarage where calculated on the previous step and saved on agg1)
for(i in 1:nrow(activityfillNA))
    if(is.na(activityfillNA[i,1]))
        activityfillNA[i,1]<-agg1[agg1$interval==activityfillNA[i,3],"x"]
```


## Are there differences in activity patterns between weekdays and weekends?

```r
#create a new columw with "weekend/weekdays"
for(i in 1:nrow(activity)){
    if(weekdays(as.Date(activity[i,2]))=="Domenica"|weekdays(as.Date(activity[i,2]))=="Sabato")
    {
        activity$day[i]<-"weekend"
    }
    
    else{        
        activity$day[i]<-"weekdays"
    }
}
aggf<-aggregate(activity$steps,by = list(interval=activity$interval,day=activity$day), FUN=mean,na.rm=TRUE)

ggplot(aggf,aes(fill=day,y=x,x=(interval)))+geom_line(stat="identity")+facet_grid(day ~. )+xlab("interval")
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
