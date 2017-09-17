### 1. Code for reading in the dataset and/or processing the data

activity <- read.csv("activity.csv", as.is = TRUE)

# Remove the NA values and store in a separate structure for future use
good_act <- activity[complete.cases(activity), ]

# Calculate the total number of steps taken per day
steps_per_day <- aggregate(steps ~ date, good_act, sum)

# 2. Histogram of the total number of steps taken each day
hist(steps_per_day$steps, main = "Histogram of total number of steps per day", xlab = "Steps per day")

# 3. Mean and median number of steps taken each day
round(mean(steps_per_day$steps))

median(steps_per_day$steps)

# 4. Time series plot of the average number of steps taken
avg_steps_per_interval <- aggregate(steps ~ interval, good_act, mean)

# Plot the time series with appropriate labels and heading
plot(avg_steps_per_interval$interval, avg_steps_per_interval$steps, type='l', col=1, main="Average number of steps by Interval", xlab="Time Intervals", ylab="Average number of steps")

# 5. The 5-minute interval that, on average, contains the maximum number of steps
interval_idx <- which.max(avg_steps_per_interval$steps)

# Identify the specific interval and the average steps for that interval
print (paste("The interval with the highest avg steps is ", avg_steps_per_interval[interval_idx, ]$interval, " and the no of steps for that interval is ", round(avg_steps_per_interval[interval_idx, ]$steps, digits = 1)))

# 6. Code to describe and show a strategy for imputing missing data
missing_value_act <- activity[!complete.cases(activity), ]
nrow(missing_value_act)

# 7. Histogram of the total number of steps taken each day after missing values are imputed
# Loop thru all the rows of activity, find the one with NA for steps.
# For each identify the interval for that row
# Then identify the avg steps for that interval in avg_steps_per_interval
# Substitute the NA value with that value

for (i in 1:nrow(activity)) {
    if(is.na(activity$steps[i])) {
        val <- avg_steps_per_interval$steps[which(avg_steps_per_interval$interval == activity$interval[i])]
        activity$steps[i] <- val 
    }
}

# Aggregate the steps per day with the imputed values
steps_per_day_impute <- aggregate(steps ~ date, activity, sum)

# Draw a histogram of the value 
hist(steps_per_day_impute$steps, main = "Histogram of total number of steps per day (IMPUTED)", xlab = "Steps per day")

# 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

week_day <- function(date_val) {
    wd <- weekdays(as.Date(date_val, '%Y-%m-%d'))
    if  (!(wd == 'Saturday' || wd == 'Sunday')) {
        x <- 'Weekday'
    } else {
        x <- 'Weekend'
    }
    x
}

# Apply the week_day function and add a new column to activity dataset
activity$day_type <- as.factor(sapply(activity$date, week_day))

#load the ggplot library
library(ggplot2)

# Create the aggregated data frame by intervals and day_type
steps_per_day_impute <- aggregate(steps ~ interval+day_type, activity, mean)

# Create the plot
plt <- ggplot(steps_per_day_impute, aes(interval, steps)) +
    geom_line(stat = "identity", aes(colour = day_type)) +
    theme_gray() +
    facet_grid(day_type ~ ., scales="fixed", space="fixed") +
    labs(x="Interval", y=expression("No of Steps")) +
    ggtitle("No of steps Per Interval by day type")
print(plt)