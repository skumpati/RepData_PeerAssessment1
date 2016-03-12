  # Reproducible Research: Course Project 1
  
  ## Loading and preprocessing the df
  ```{r loaddf}
cls = c("integer", "character", "integer")
df <- read.csv("activity.csv", head=TRUE, colClasses=cls, na.strings="NA")
head(df)
```

## What is mean total number of steps taken per day?
```{r}
library(ggplot2)
total.steps <- tapply(df$steps, df$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)
```
![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png)

## What is the average daily activity pattern?
```{r}
library(ggplot2)
averages <- aggregate(x=list(steps=df$steps), by=list(interval=df$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(df=averages, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken")
```

On average across all the days in the dfset, the 5-minute interval contains
the maximum number of steps?
```{r}
averages[which.max(averages$steps),]
```

## Imputing missing values

There are many days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the df.

```{r how_many_missing}
missing <- is.na(df$steps)
# How many missing
table(missing)
```

All of the missing values are filled in with mean value for that 5-minute
interval.

```{r}
# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (averages[averages$interval==interval, "steps"])
  return(filled)
}
filled.df <- df
filled.df$steps <- mapply(fill.value, filled.df$steps, filled.df$interval)
```
Now, using the filled df set, let's make a histogram of the total number of steps taken each day and calculate the mean and median total number of steps.

```{r}
total.steps <- tapply(filled.df$steps, filled.df$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps)
median(total.steps)
```

Mean and median values are higher after imputing missing df. The reason is
that in the original df, there are some days with `steps` values `NA` for 
any `interval`. The total number of steps taken in such days are set to 0s by
default. However, after replacing missing `steps` values with the mean `steps`
of associated `interval` value, these 0 values are removed from the histogram
of total number of steps taken each day.

## Are there differences in activity patterns between weekdays and weekends?
First, let's find the day of the week for each measurement in the dfset. In
this part, we use the dfset with the filled-in values.

```{r}
weekday.or.weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
filled.df$date <- as.Date(filled.df$date)
filled.df$day <- sapply(filled.df$date, FUN=weekday.or.weekend)
```

Now, let's make a panel plot containing plots of average number of steps taken
on weekdays and weekends.
```{r}
averages <- aggregate(steps ~ interval + day, df=filled.df, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
xlab("5-minute interval") + ylab("Number of steps")
```




