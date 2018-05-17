1.  Loading and preprocessing the data

-   Required packages: dplyr, magrittr, ggplot2

<!-- -->

    ## Loading required package: dplyr

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Loading required package: ggplot2

    ## Loading required package: magrittr

-   Load the data

<!-- -->

        activityData<-read.csv("C:/Users/kkosins/Desktop/Coursera Data Science/Data Science Reporting in R/Project 1/activity.csv", header = TRUE)

1.  What is mean total number of steps taken per day? <br /> For this
    part of the assignment, the missing values in the dataset are
    ignored.

-   Calculate the total number of steps taken per day

<!-- -->

          #Histogram of the total number of steps taken each day
          #Aggregating data - sum by day
          DailyStepsData<-activityData %>% 
            group_by(date) %>%
              summarise(total_daily=sum(steps, na.rm=TRUE)) #%>%
              #filter(total_daily!="NA")
          #Histogram
          with(DailyStepsData, hist(DailyStepsData$total_daily, 
                                   main="Total Number of Steps Taken Each Day", 
                                   xlab="Steps Total", 
                                   ylab="Frequency"))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

-   Calculate and report the mean and median of the total number of
    steps taken per day

<!-- -->

          mean_daily<- round(mean(DailyStepsData$total_daily, na.rm=TRUE), 0)
          median_daily<- median(DailyStepsData$total_daily, na.rm=TRUE)

-   **Mean of the total number of steps taken per day is 9354.**
-   **Median of the total number of steps taken per day is 10395.**

1.  What is the average daily activity pattern? <br />

-   Time series plot of the 5-minute interval (x-axis) and the average
    number of steps taken, averaged across all days (y-axis)

<!-- -->

    #Aggregating data - average by interval
    AvgStepsData<-activityData %>% 
      group_by(interval) %>%
      summarise(avg_daily=mean(steps, na.rm=TRUE))
    #Time Series plot
    with(AvgStepsData, plot(x=AvgStepsData$interval, y=AvgStepsData$avg_daily, type = "l",
                            main="Average Number of Steps Taken", 
                            xlab="Time Interval", 
                            ylab="Avg Number of Steps",
                            cex.axis=0.75))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    #The 5-minute interval that, on average, contains the maximum number of steps
    maxInterval<-AvgStepsData$interval[AvgStepsData$avg_daily==max(AvgStepsData$avg_daily)]

-   835 is 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps. <br />

1.  Imputing missing values <br />

-   Calculate and report the total number of missing values in the
    dataset

<!-- -->

    #Calculate and report the total number of missing values in the dataset
    numberOfNAs<-sum(is.na(activityData[,1]))

-   **Total number of missing values is 2304**
-   Devise a strategy for filling in all of the missing values in the
    dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc.
-   **Missing steps values will be replaced by average number of steps
    for 5-minute interval.**
-   Create a new dataset that is equal to the original dataset but with
    the missing data filled in.

<!-- -->

    #Average number of Steps taken in each 5-minute interval, rounded to the whole numbers
    AvgStepsData$AvgStepsInteger<-round(AvgStepsData$avg_daily, digits = 0)
    #Creating new data table with activity data and avg number of steps (left join)
    activityData_noMissing<- left_join(activityData, AvgStepsData, by="interval")
    #Replacing NA values in steps column with avgeage steps value for interval accordingly (avg rounded
    #   to zero decimal places)  
    activityData_noMissing$steps_repl_na<- 
        ifelse(is.na(activityData_noMissing$steps),
          activityData_noMissing$AvgStepsInteger,
          activityData_noMissing$steps)
    #Creating a new dataset that is equal to the original dataset but with the missing data filled in
    activityData_noMissing<-activityData_noMissing[,c(2,3,6)]

-   Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day. Do these values differ from the estimates from the first
    part of the assignment? What is the impact of imputing missing data
    on the estimates of the total daily number of steps?

<!-- -->

    #Aggregating data - sum by day
    DailyStepsData_noNA<-activityData_noMissing %>% 
      group_by(date) %>%
      summarise(total_daily=sum(steps_repl_na))
    #Histogram
    with(DailyStepsData_noNA, hist(DailyStepsData_noNA$total_daily, 
                              main="Total Number of Steps Taken Each Day", 
                              xlab="Steps Total", 
                              ylab="Frequency"))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)

    #Mean and median total number of steps taken per day
    StatsStepsPerDay2<- DailyStepsData_noNA %>% 
      summarise(mean_daily=mean(total_daily, na.rm=TRUE), median_daily=median(total_daily, na.rm=TRUE))

-   **Mean of the total number of steps taken per day is 10766.**
-   **Median of the total number of steps taken per day is 10762.**
-   **After replcing missing values from original dataset with avergaes
    for 5-minute intervals, more datapoints in column 'steps' are
    available and taken into account when calculating mean and median,
    so ultimately those values changed comapring to the values from the
    first part of the assignment.** <br />

1.  Are there differences in activity patterns between weekdays and
    weekends?

-   Create a new factor variable in the dataset with two levels -
    "weekday" and "weekend" indicating whether a given date is a weekday
    or weekend day.

<!-- -->

    #Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
    # indicating whether a given date is a weekday or weekend day.
    activityData_noMissing$day<- ifelse(weekdays(as.Date(activityData_noMissing$date))=="Saturday" |
                                        weekdays(as.Date(activityData_noMissing$date))=="Sunday",
                                                 "weekend", "weekday")

-   Make a panel plot containing a time series plot of the 5-minute
    interval (x-axis) and the average number of steps taken, averaged
    across all weekday days or weekend days (y-axis).

<!-- -->

    #Aggregating data - average by interval
    avgSteps_weekday_weekend<- activityData_noMissing %>%
                                      group_by(interval, day) %>%
                                        summarise(avgSteps=mean(steps_repl_na))
    #Panel plot
    ggplot(data=avgSteps_weekday_weekend, aes(x=interval, y=avgSteps)) +
      geom_line() + 
      facet_grid(day ~.) + 
      labs(title="Avergae number of steps taken per 5-minute interval", 
           x="Interval", y="Avergae Number of Steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)
<br />
fig.path='figure/' 
