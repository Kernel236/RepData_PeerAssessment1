---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

``` r
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# Load the dataset
activity_data <- read.csv(here::here("data", "activity.csv"))
# Convert date column to Date type
activity_data$date <- as.Date(activity_data$date)

# Check the structure of the dataset
str(activity_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

``` r
# Check for missing values
sum(is.na(activity_data))
```

```
## [1] 2304
```

## What is mean total number of steps taken per day?

``` r
# Calculate total steps per day
total_steps_per_day <- activity_data %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm = TRUE))
```

## What is the average daily activity pattern?

``` r
# Calculate average steps per interval
average_steps_per_day <- activity_data %>%
  group_by(date) %>%
  summarise(average_steps = mean(steps, na.rm = TRUE))
```

## Imputing missing values


``` r
# Check for missing values in the dataset and look at distribution of steps
# Use median to impute missing values

missing_steps <- sum(is.na(activity_data$steps))
if (missing_steps > 0) {
  # Impute missing values with the mean of the respective interval
  activity_data_imputed <- activity_data %>%
    group_by(interval) %>%
    mutate(steps = ifelse(is.na(steps), median(steps, na.rm = TRUE), steps)) %>%
    ungroup()
}
```

## Are there differences in activity patterns between weekdays and weekends?

``` r
# Create a new column to indicate whether the day is a weekday or weekend
activity_data_imputed <- activity_data_imputed %>%
  mutate(day_type = ifelse(wday(date) %in% c(1, 7), "Weekend", "Weekday"))

# Calculate average steps per interval for weekdays and weekends
average_steps_weekday <- activity_data_imputed %>%
  filter(day_type == "Weekday") %>%
  group_by(interval) %>%
  summarise(average_steps = mean(steps, na.rm = TRUE))

average_steps_weekend <- activity_data_imputed %>%
  filter(day_type == "Weekend") %>%
  group_by(interval) %>%
  summarise(average_steps = mean(steps, na.rm = TRUE))
```

## Plotting the results

After performing the calculations and imputations, we can visualize the results to better understand the activity patterns.

### Plot the average daily activity pattern

``` r
# Plot the total steps per day
ggplot(total_steps_per_day, aes(x = date, y = total_steps)) +
  geom_line(color = "blue") +
  labs(title = "Total Steps per Day", x = "Date", y = "Total Steps") +
  theme_minimal()
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

``` r
#save in figures
ggsave("figures/total_steps_per_day.png", width = 8, height = 6)
```

### Plot the average steps per interval for weekdays and weekends

``` r
# Plot the average steps per interval for weekdays and weekends
ggplot() +
  geom_line(data = average_steps_weekday, aes(x = interval, y = average_steps), color = "steelblue") +
  geom_line(data = average_steps_weekend, aes(x = interval, y = average_steps), color = "red") +
  labs(title = "Average Steps per Interval", x = "Interval", y = "Average Steps") +
  scale_x_continuous(breaks = seq(0, 2400, by = 200)) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("Weekday" = "steelblue", "Weekend" = "red"))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

``` r
# Save the plot
ggsave("figures/average_steps_per_interval.png", width = 8, height = 6)
```

