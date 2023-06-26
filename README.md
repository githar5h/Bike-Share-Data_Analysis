# **Cyclistic Bike Share Data Analysis**

Data Analytics on Cyclistic case study; A part of the Google Data Analytics Professional Certificate on Coursera

## Background & Business Question

Launched in 2016, Cyclistic is a successful point-to-point fictional bike-share
company consisting of 5,824 geo-tracked bicycles and 692 locking
stations.

Historically, Cyclistic's marketing strategy has focused on increasing
public awareness and appealing to a broad market. Flexible pricing plans
are core to this strategy and three purchasing tiers are currently
offered; (1) single-ride, (2) full-day, and (3) annual memberships.

Cyclistic financial analysts have concluded annual members are the most
profitable to the company.

The goal of this analysis is to help convert casual riders into annual members by helping the marketing analyst team to better understand how casual riders and annual members use Cyclistic bikes differently through drive data-driven decision making.

The R programming language has been used to carry out this analysis.

## Data Preparation

Cyclistic ride data has been made available at the following link by Motivate International Inc.:
<https://divvy-tripdata.s3.amazonaws.com/index.html>.

This analysis is based on the rider data for the year 2022. The data is available as a CSV file for every month. Each of this csv file is imported and combined into a single data frame in R.

The following libraries were used to process data - tidyverse for data
cleaning, lubridate and dplyr for date manipulation, and ggplot2 for data visualiztion.

```{r}
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
```

On initial inspection of the data, the following problems are identified and fixed.
- In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual").
- The data can only be aggregated at the ride-level, which is too granular. Some additional columns of data, such as day, month, year, that provide additional opportunities to aggregate the data can be added.
- There are some rides where tripduration shows up as negative, including several hundred rides where bikes were taken out of circulation for Quality Control reasons. These rides need to be deleted.

## Data Processing

Columns unnecessary for the objective of this analysis were removed.

```{r}
all_trips_2022 = all_trips_2022 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))
```

The problems idetified earlier in the data were addressed and fixed in the following way.

```{r}
# remove inconsistency in the "member_casual" column by replacing subscriber with member and customer with casual
all_trips_2022 = all_trips_2022 %>%
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

# Add a "ride_length" calculation (in seconds)
all_trips_2022$ride_length = difftime(all_trips_2022$ended_at,all_trips_2022$started_at)

# convert "ride_length" from Factor to numeric in order to be able to run calculations on it
all_trips_2022$ride_length = as.numeric(as.character(all_trips_2022$ride_length))

# create new data frame with entries from the company HQ and negative entries from the "ride_length" removed
all_trips_2022_v2 = all_trips_2022[!(all_trips_2022$start_station_name == "HQ QR" | all_trips_2022$ride_length<0),]

```
In the new data frame, duplicate rows and null values were removed as collecting new data or getting the data corrected was not possible due the raw data being available as-is.

```
# remove duplicate rows
all_trips_2022_v2 = distinct(all_trips_2022)

# remove rows with null values
all_trips_2022_v2 = na.omit(all_trips_2022)
```

## Data Analysis

The total number of rides and what part of it comes from different member types was counted.

```{r}
# total number of rides (approximate)
round(nrow(all_trips_2022_v2), digits = -5)

# count member types
all_trips_2022_v2 %>% count(member_casual)
```
Approximately 4,400,000 rides were taken in the year 2022, out of which, 1,758,189 were by casual members and 2,611,171 were by annual members.

Descriptive analysis was run on the "ride_length" column to get the average, median, maximum and minimum ride lengths. 

```{r}
summary(all_trips_2022_v2$ride_length)
```
On the basis of the descriptive analysis, members and casual users were compared as follows.

```{r}
aggregate(all_trips_2022_v2$ride_length ~ all_trips_2022_v2$member_casual, FUN = mean)
aggregate(all_trips_2022_v2$ride_length ~ all_trips_2022_v2$member_casual, FUN = median)
aggregate(all_trips_2022_v2$ride_length ~ all_trips_2022_v2$member_casual, FUN = max)
aggregate(all_trips_2022_v2$ride_length ~ all_trips_2022_v2$member_casual, FUN = min)
```

To dive deeper, the average ride of members and casual riders was compared on the day of the week basis. The days were ordered as well.

```{r}
all_trips_2022_v2$day_of_week <- ordered(all_trips_2022_v2$day_of_week, levels=c("Sunday", "Monday",
                                                                       "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips_2022_v2$ride_length ~ all_trips_2022_v2$member_casual + all_trips_2022_v2$day_of_week,
          FUN = mean)
```

Number of rides on different types of bikes available was found out.

```{r}
all_trips_2022_v2 %>%
  group_by(member_casual, rideable_type) %>% 
  count(rideable_type)
  ```
Docked bikes are the least popular choice among both the member types. Only 174,858 out of the 1,758,189 rides taken by casual riders were on a docked bike. 0 out of the 2,611,171 rides taken by annual members were on a docked bike.

## Data Visualization

Due to the data frame containing millions of rows, ggplot2 defaulted to labeling axes in scientific notation. This default was overridden.

```{r}
options(scipen = 999)
```

The number of rides by rider types on each day of the week was visualized using the ggplot2 library.

```{r}
all_trips_2022_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
  ```

![Number of trips on every day of the week](https://github.com/githar5h/Bike-Share-Data_Analysis/assets/96515805/2ec9a1f1-65ce-4b06-a1ff-c44fe225c90f)


The most popular day for casual users is Saturday, for members the most popular day is Thursday.

The average ride duration of each rider type was visualized using the ggplot2 library.

```{r}
all_trips_2022_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
  ```

![Average duration](https://github.com/githar5h/Bike-Share-Data_Analysis/assets/96515805/7eb9a1c4-b105-41b4-8d90-249917fbbde1)


Bike usage by different member types in different months was plotted.

```{r}
#arranges months in order
data_12_months_2$month <- ordered(data_12_months_2$month, levels=c("01", "02", "03", "04", "05", "06", "07","08","09","10","11","12"))

# plot number of rides per month
all_trips_2022_v2 %>%
  group_by(member_casual, month) %>%
  dplyr::summarize(count_trips = n()) %>%  
  ggplot(aes(x= month, y=count_trips, fill=member_casual, color=member_casual)) +
  geom_bar(stat='identity', position = 'dodge') +
  theme_bw() +
  labs(title ="Number of Rides per Month", x = "Month", y = "Number of Trips")
```

![Number of rides per month](https://github.com/githar5h/Bike-Share-Data_Analysis/assets/96515805/770c154c-23c8-431b-8570-9944ab76428a)


The months of June, July, August and September are the most popular.

## Recommendations

The goal of this analysis was to answer how casual riders and annual members use Cyclistic bikes differently and how can casual riders be converted to annual members.

Based on my analysis, following are my recommendations to convert more casual riders into annual members.
- Offer incentives for casual riders to upgrade to annual memberships, such as discounts and promotions.
- Target the most popular months between riders in general to further boost the number of customers.
- Slowly phase out docked bikes, in order to reduce maintenance costs arising from more number of bike types.
