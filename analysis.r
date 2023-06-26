# load required packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)

# set the work directory to where the data is stored on the local disk
setwd("E:\\githar5h\\Data Analytics Project\\Raw_CSV_Data")

# each csv file contains data for a particular month of the year 2022
# import each csv as a variable 
m1 = read_csv("202201-divvy-tripdata.csv")
m2 = read_csv("202202-divvy-tripdata.csv")
m3 = read_csv("202203-divvy-tripdata.csv")
m4 = read_csv("202204-divvy-tripdata.csv")
m5 = read_csv("202205-divvy-tripdata.csv")
m6 = read_csv("202206-divvy-tripdata.csv")
m7 = read_csv("202207-divvy-tripdata.csv")
m8 = read_csv("202208-divvy-tripdata.csv")
m9 = read_csv("202209-divvy-tripdata.csv")
m10 = read_csv("202210-divvy-tripdata.csv")
m11 = read_csv("202211-divvy-tripdata.csv")
m12 = read_csv("202212-divvy-tripdata.csv")

# stack monthly data into one combined data frame
all_trips_2022 = bind_rows(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12)

# remove unnecessary columns to declutter
all_trips_2022 = all_trips_2022 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

# remove inconsistency in the "member_casual" column by replacing subscriber with member and customer with casual
all_trips_2022 = all_trips_2022 %>%
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

# add a "ride_length" calculation (in seconds)
all_trips_2022$ride_length = difftime(all_trips_2022$ended_at,all_trips_2022$started_at)

# convert "ride_length" from Factor to numeric in order to be able to run calculations on it
all_trips_2022$ride_length = as.numeric(as.character(all_trips_2022$ride_length))

# create new data frame with entries from the company HQ and negative entries from the "ride_length" removed
all_trips_2022_v2 = all_trips_2022[!(all_trips_2022$start_station_name == "HQ QR" | all_trips_2022$ride_length<0),]

# remove duplicate rows
all_trips_2022_v2 = distinct(all_trips_2022)

# remove rows with null values as there is no way to collect the data again
all_trips_2022_v2 = na.omit(all_trips_2022)

# descriptive analysis on the "ride_length" column
# all figures are in seconds
summary(all_trips_2022_v2$ride_length)

# compare members and casual users based on "ride_length"
aggregate(all_trips_2022_v2$ride_length ~ all_trips_2022_v2$member_casual, FUN = mean)
aggregate(all_trips_2022_v2$ride_length ~ all_trips_2022_v2$member_casual, FUN = median)
aggregate(all_trips_2022_v2$ride_length ~ all_trips_2022_v2$member_casual, FUN = max)
aggregate(all_trips_2022_v2$ride_length ~ all_trips_2022_v2$member_casual, FUN = min)

# add columns that list the date, month, day and year of each ride to better aggregate data
all_trips_2022_v2$date = as.Date(all_trips_2022_v2$started_at)
all_trips_2022_v2$month = format(as.Date(all_trips_2022_v2$date), "%m")
all_trips_2022_v2$day = format(as.Date(all_trips_2022_v2$date), "%d")
all_trips_2022_v2$year = format(as.Date(all_trips_2022_v2$date), "%Y")
all_trips_2022_v2$day_of_week = format(as.Date(all_trips_2022_v2$date), "%A")

# see total number of rides and what part of it comes from different rider types
# total number of rides (approximate)
round(nrow(all_trips_2022_v2), digits = -5)

#count member types
all_trips_2022_v2 %>% count(member_casual)

# see the average ride time by each day for members vs casual users, after setting the order of the days of the week
all_trips_2022_v2$day_of_week <- ordered(all_trips_2022_v2$day_of_week, levels=c("Sunday", "Monday",
                                                                       "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips_2022_v2$ride_length ~ all_trips_2022_v2$member_casual + all_trips_2022_v2$day_of_week,
          FUN = mean)

# total rides by bike type
all_trips_2022_v2 %>%
  group_by(member_casual, rideable_type) %>% 
  count(rideable_type)

# override ggplot2 scientific notation default
options(scipen = 999)

# visualize the number of rides by rider type
all_trips_2022_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  theme_bw()+
  geom_col(position = "dodge")

# visualize average duration
all_trips_2022_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  theme_bw() +
  geom_col(position = "dodge")

# visualize bike usage by different member types in different months
#arranges months in order
all_trips_2022_v2$month <- ordered(all_trips_2022_v2$month, levels=c("01", "02", "03", "04", "05", "06", "07","08","09","10","11","12"))

# plot number of rides per month
all_trips_2022_v2 %>%
  group_by(member_casual, month) %>%
  dplyr::summarize(count_trips = n()) %>%  
  ggplot(aes(x= month, y=count_trips, fill=member_casual, color=member_casual)) +
  geom_bar(stat='identity', position = 'dodge') +
  theme_bw()
