# SETTING UP
# ==========
# Set working directory
setwd("C:/Users/Darren/Desktop")

# Load relevant packages
library(tidyverse)
library(readr)
library(skimr)
library(lubridate)
library(ggplot2)
library(mapview)
library(dplyr)
library(scales)
library(wesanderson)
library(remotes)


# Read in csv files
jul_2021 <- read_csv(file = 'Google Data Analytics Capstone Project/Cyclistic_Rides_2021_07.csv')
aug_2021 <- read_csv(file = 'Google Data Analytics Capstone Project/Cyclistic_Rides_2021_08.csv')
sep_2021 <- read_csv(file = 'Google Data Analytics Capstone Project/Cyclistic_Rides_2021_09.csv')
oct_2021 <- read_csv(file = 'Google Data Analytics Capstone Project/Cyclistic_Rides_2021_10.csv')
nov_2021 <- read_csv(file = 'Google Data Analytics Capstone Project/Cyclistic_Rides_2021_11.csv')
dec_2021 <- read_csv(file = 'Google Data Analytics Capstone Project/Cyclistic_Rides_2021_12.csv')
jan_2022 <- read_csv(file = 'Google Data Analytics Capstone Project/Cyclistic_Rides_2022_01.csv')
feb_2022 <- read_csv(file = 'Google Data Analytics Capstone Project/Cyclistic_Rides_2022_02.csv')
mar_2022 <- read_csv(file = 'Google Data Analytics Capstone Project/Cyclistic_Rides_2022_03.csv')
apr_2022 <- read_csv(file = 'Google Data Analytics Capstone Project/Cyclistic_Rides_2022_04.csv')
may_2022 <- read_csv(file = 'Google Data Analytics Capstone Project/Cyclistic_Rides_2022_05.csv')
jun_2022 <- read_csv(file = 'Google Data Analytics Capstone Project/Cyclistic_Rides_2022_06.csv')

# Skim through all 12 tables to ensure consistency to allow for combining via rbind
str(jul_2021)
str(aug_2021)
str(sep_2021)
str(oct_2021)
str(nov_2021)
str(dec_2021)
str(jan_2022)
str(feb_2022)
str(mar_2022)
str(apr_2022)
str(may_2022)
str(jun_2022)

# Combine them into one single table
all_rides <- rbind(jul_2021, aug_2021, sep_2021, oct_2021, nov_2021, dec_2021, 
                   jan_2022, feb_2022, mar_2022, apr_2022, may_2022, jun_2022)


# DATA CLEANING
# =============

# Create new column called "time_of_day"
all_rides$time_of_day <- hour(all_rides$started_at) 

# Create new column called "day_of_week"
all_rides$day_of_week <- weekdays(all_rides$started_at)
# Reorder "day_of_week" so that it starts on Sunday and ends on Saturday
all_rides$day_of_week <- ordered(all_rides$day_of_week, levels = c("Sunday", 
                                                                   "Monday", 
                                                                   "Tuesday",
                                                                   "Wednesday", 
                                                                   "Thursday", 
                                                                   "Friday", 
                                                                   "Saturday"))

# Create new column called "month"
all_rides$month <- month(all_rides$started_at)
# Need to manually reorder because data is from July 2021 to June 2022
all_rides$month <- ordered(all_rides$month, levels = c("Jul", "Aug", "Sep",
                                                                         "Oct", "Nov", "Dec",
                                                                         "Jan", "Feb", "Mar", 
                                                                         "Apr", "May", "Jun"))

# Create new column called "ride_length" by subtracting "started_at" column from "ended_at" columns
# diff_time finds the difference between two dates, put later date first then earlier date
all_rides$ride_length <- difftime(all_rides$ended_at,all_rides$started_at)
# Convert difftime to numeric
all_rides$ride_length <- as.numeric(all_rides$ride_length)

# Check newly created columns
str(all_rides)

# There are values in ride_length that are negative from skimming the data, do a quick check to see how many are there exactly
all_rides %>% 
  filter(all_rides$ride_length <= 0) %>%
  count()
# There are 646 rows that is either negative or has zero as a value for ride length, and no other way to correct the data
# This does not make sense, so I am removing them 
all_rides <- all_rides %>% filter(all_rides$ride_length > 0)

# On the other hand, the max value of ride_length is about 34 days
# Investigate how many rides have a ride_length of more than 1 day
all_rides %>% 
  filter(all_rides$ride_length > 86400) %>% 
  count() #4,718 cases OR 0.08% of total cases
all_rides %>% 
  filter(all_rides$ride_length < 3) %>% 
  count() #6,120 OR 0.1% of total cases
# I concluded that these extreme data points should not be removed as I don't have enough information
# and they make up a small portion of the data
# and it's possible that customers either abuse the system by hogging bikes for days
# or they change their mind (for whatever reason) they no longer want to ride after unlocking a bike

# Note to self to understand outliers found via interquartile range (IQR) method.
# IQR is the central 50% between the 75th and 25th percentile of a distribution.
# A point is an outlier if it's above the 75th or below the 25th percentile by a factor of 1.5*IQR.
# Example:
# A = 25th percentile, B = 75th percentile
# IQR = B - A, outlier = A - 1.5*IQR OR B + 1.5*IQR
# Good article on explaining why use 1.5: https://towardsdatascience.com/why-1-5-in-iqr-method-of-outlier-detection-5d07fdc82097

# Remove rows with NAs in either end_lat or end_lng
all_rides <- all_rides %>% filter(!is.na(end_lat) | !is.na(end_lng))

# Final inspection of data 
colnames(all_rides)  #List of column names
nrow(all_rides)  #How many rows are in data frame?
dim(all_rides)  #Dimensions of the data frame?
head(all_rides, 6)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_rides)  #See list of columns and data types (numeric, character, etc)
summary(all_rides)  #Statistical summary of data. Mainly for numerics
skim_without_charts(all_rides)

# Create tables to reflect casual and member riders' start & end location by number of rides
casual_riders <- all_rides %>%
  filter(all_rides$member_casual == "casual")
casual_start <- casual_riders %>% 
  group_by(start_station_name, start_lat, start_lng) %>% 
  summarise(n_rides_start = n()) %>%
  arrange(-n_rides_start)
member_riders <- all_rides %>%
  filter(all_rides$member_casual == "member")
member_start <- member_riders %>% 
  group_by(start_station_name, start_lat, start_lng) %>% 
  summarise(n_rides_start = n()) %>%
  arrange(-n_rides_start)



# DESCRIPTIVE ANALYSIS
#=====================

# Total number of rides casual vs. member
all_rides %>% 
  group_by(member_casual) %>%
  ggplot(aes(x = member_casual)) +
  geom_bar(aes(fill = member_casual)) +
  theme(panel.background = element_blank()) +
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = wes_palette("Darjeeling2"), labels = c("Casual", "Member")) +
  scale_x_discrete(labels = c("Casual", "Member")) +
  xlab(" ") +
  theme(axis.title.x = element_text(size = 14, margin = margin(t=20))) +
  ylab("Number of Rides") +
  theme(axis.title.y = element_text(size = 14, margin = margin(r = 20))) +
  labs(fill = "Type of Riders")
  

# Create function to calculate mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Mode
getmode(ytd_combined_clean$day_of_week) # Find the day with highest number of rides

# Mean, Median, Max, Min ride times of casual vs. member
# aggregate function takes numerical variable as first argument, then categorical variable, then the function to run: numerical ~ categorical, data = df, mean/median/max/min
aggregate(all_rides$ride_length ~ all_rides$member_casual, data = all_rides, mean) # Mean ride time of casual and member
aggregate(all_rides$ride_length ~ all_rides$member_casual, data = all_rides, median) # Median ride time of casual and member
aggregate(all_rides$ride_length ~ all_rides$member_casual, data = all_rides, max) # Max ride time of casual and member
aggregate(all_rides$ride_length ~ all_rides$member_casual, data = all_rides, min) # Min ride time of casual and member

# Mean ride times of casual vs. member in days of week
aggregate(ytd_combined_clean$ride_length ~ ytd_combined_clean$member_casual + ytd_combined_clean$day_of_week, ytd_combined_clean, mean)

# Notice that days of the week are out of order. Let's fix it
ytd_combined_clean$day_of_week <- ordered(ytd_combined_clean$day_of_week, levels = c("Sunday", "Monday", "Tuesday",
                                                                                     "Wednesday", "Thursday", "Friday", "Saturday"))

# Run average ride time by each day for casual vs. member again
aggregate(ytd_combined_clean$ride_length ~ ytd_combined_clean$member_casual + ytd_combined_clean$day_of_week, ytd_combined_clean, mean)

# Analyze ridership data by type and weekday
ytd_combined_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Count of ride types by casual vs. member
ytd_combined_clean %>% 
  group_by(member_casual, rideable_type) %>%
  summarize(count = n())

#
ytd_combined_clean %>% 
  group_by(member_casual, day_of_week) %>%
  summarize(count = n())



ytd_combined_clean %>% 
  group_by(member_casual, start_station_name) %>%
  summarize(count = n()) %>%
  slice_max(order_by = count, n = 6)

ytd_combined_clean %>% 
  group_by(member_casual, end_station_name) %>%
  summarize(count = n()) %>%
  slice_max(order_by = count, n = 6)




# VISUALISE
#==========

# Let's visualize the number of rides by rider type
ytd_combined_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  theme(panel.background = element_blank()) +
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = wes_palette("Darjeeling2"), labels = c("Casual", "Member")) +
  xlab("Days of Week") +
  theme(axis.title.x = element_text(size = 14, margin = margin(t=20))) +
  ylab("Number of Rides") +
  theme(axis.title.y = element_text(size = 14, margin = margin(r = 20))) +
  labs(fill = "Type of Riders")


# Let's create a visualization for average duration
ytd_combined_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length/60)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  theme(panel.background = element_blank()) + 
  scale_fill_manual(values = wes_palette("Darjeeling2"), labels = c("Casual", "Member")) +
  xlab("Days of Week") +
  theme(axis.title.x = element_text(size = 14, margin = margin(t=20))) +
  ylab("Avg Ride Time (Mins)") +
  theme(axis.title.y = element_text(size = 14, margin = margin(r = 20))) +
  labs(fill = "Type of Riders")


# Rides by Casual and Members by Hours in Day
ytd_combined_clean %>% 
  group_by(member_casual, hour) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, hour)  %>% 
  ggplot(aes(x = hour, y = number_of_rides, group = member_casual)) +
  geom_line(aes(color = member_casual), size = 1.5) +
  theme(panel.background = element_blank()) + 
  scale_color_manual(values = wes_palette("Darjeeling2"), labels = c("Casual", "Member")) +
  xlab("Hours of Day") +
  theme(axis.title.x = element_text(size = 14, margin = margin(t=20))) +
  ylab("Number of Rides") +
  scale_y_continuous(labels = comma) +
  theme(axis.title.y = element_text(size = 14, margin = margin(r = 20))) +
  labs(color = "Type of Riders")


# PLOTTING MAPS
casual_start[1:50,] %>%
  mapview(
    xcol = "start_lng", 
    ycol = "start_lat",
    zcol = "n_rides_start",
    cex = "n_rides_start",
    at = c(10000, 20000, 30000, 40000, 50000, 60000),
    crs = 4326,
    grid = F, 
    legend = T,
    layer.name = "Casual Riders' Favourite Start Locations")
member_start[1:50,] %>%
  mapview(
    xcol = "start_lng", 
    ycol = "start_lat",
    zcol = "n_rides_start",
    cex = "n_rides_start",
    at = c(10000, 20000, 30000, 40000, 50000, 60000),
    crs = 4326,
    grid = F, 
    legend = T,
    layer.name = "Member Riders' Favourite Start Locations")

#DONE!


