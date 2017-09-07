# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

'''{r}
# First question: Downloading and reading the file
URL_file <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(URL_file, "~/R/Course 5/Factivity.zip")
downloaddatum <- Sys.Date()

Factivity <- read.csv((unz("~/R/Course 5/Factivity.zip", "activity.csv")))

Factivity$date <- as.Date(as.character(Factivity$date))

'''


## What is mean total number of steps taken per day?

'''{r}
#Second question: histogram number of steps per day

Sum_per_day <- Factivity %>% group_by(date) %>% summarize(total_steps=sum(steps))
hist(Sum_per_day$total_steps, breaks = 20,  xlab = "Day", ylab = "number of steps", main = "Number of steps per day")

'''

## What is the average daily activity pattern?

'''{r}
#Third question: mean and medium steps per day

Mean_per_day   <- mean( Sum_per_day$total_steps, na.rm = TRUE)
Median_per_day <- median(Sum_per_day$total_steps, na.rm = TRUE)

cat("Mean   :", Mean_per_day)
cat("Median :", Median_per_day)


#Forth question: time series plot of average number of steps taken

Mean_per_interval <- filter(Factivity, !is.na(Factivity$steps))
Mean_per_interval <- Mean_per_interval %>% group_by(interval) %>% summarize(total_steps=mean(steps))
plot(Mean_per_interval$interval, Mean_per_interval$total_steps, type = 'l',xlab = "daynumber", ylab = "number of steps", main = "Average number of steps per interval")

# Fifth question: interval that takes on average maximum number of steps per day
max_steps <- filter(Mean_per_interval, Mean_per_interval$total_steps == max(Mean_per_interval$total_steps))
print(max_steps)



'''

## Imputing missing values

'''{r}

# chosen to impute the steps with NA with the average of the interval 

Number_of_NA <- sum(is.na(Factivity$steps))

aant_int <- as.integer(count(Factivity))

for (i in 1:aant_int) {
    if(is.na(Factivity$steps[[i]])) {
        Interval <- as.integer(Factivity$interval[[i]])
        Mean_step <- filter(Mean_per_interval, Mean_per_interval$interval == Interval)
        Factivity$steps[[i]] <- as.integer(Mean_step$total_steps)
        print(Factivity$steps[[i]])    

    } 
}

# Seventh question: Histogram of the total number of steps taken each day 
# after missing values are imputed

Sum_per_day <- Factivity %>% group_by(date) %>% summarize(total_steps=sum(steps))
hist(Sum_per_day$total_steps, breaks = 20,  xlab = "Day", ylab = "number of steps", main = "Number of steps per day")

'''

## Are there differences in activity patterns between weekdays and weekends?

'''{r}
# Eighth question: Panel plot comparing the average number of steps taken
# per 5-minute interval across weekdays and weekends

Factivity$weekday1 <- wday(Factivity$date)
for (i in 1:aant_int) {
        if(Factivity$weekday1[[i]] %in% c(2, 3, 4, 5, 6)) {
                Factivity$weekday2[[i]] <- "weekday"    
        } else {
                 Factivity$weekday2[[i]] <- "weekend"
         }
}

Fact_weekday_interval <- Factivity %>% group_by(weekday2, interval) %>% summarize(total_steps=mean(steps))

xyplot(total_steps ~ interval | weekday2, 
       data = Fact_weekday_interval, 
       layout = c(1,2), 
       type = 'l', 
       main = "Main number of steps per interval weekend versus weekday",
       xlab = "daynumber", 
       ylab = "mean steps")



'''
