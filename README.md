![This is an image](https://bayodeolorundare.com/wp-content/uploads/2023/02/tableau_hero.jpg)

# Divvy-Exercise-Case-study
*Google Data Analyst Capstone Project*

The purpose of this notebook is to conduct an analysis for the Google Data Analysis Capstone Project on Coursera. In this case study I was provided data from a bike-share company called Divvy. For the purpose of this exercise, and from now on, Divvy will be referred to as Cyclistic. In this exercise, I work for a fictional company, Cyclistic. In order to answer the key business questions, I will follow the steps of the Data Analysis Process: Act, Prepare, Process, Analyze, Share and Act.

**The Business Task**

To deliver recommendations by analyzing the 2019 - 2020 trip data for Cyclistic that is used to answer the key question: "In what ways do casual riders and members use Divvy bikes differently?". The data will be consolidated into a single dataframe prior to starting the analysis.

**Description of Data Sources Used**

I used twelve-months of historical trip dataset provided by Cyclistic to perform this analysis. This includes four different files each containing a quarterly (3 months) of data ranging from April 2019 - March 2020. The data is public and has been made available by Motivate International Inc. under this [License](https://ride.divvybikes.com/data-license-agreement).

**Data Cleaning and Manipulation**

I used R to have the data collected, wrangled, cleaned, stacked, manipulated and combined to perform my analysis. I will outline the steps with code samples to show my steps of the Data Analysis Process.

**Load the required analytics packages in R**

* tidyverse for data import and wrangling
> * ggplot2 for visualizations
> * dplyr for mechanisms
> * readr helps read csv files
* magrittr provides mechanisms for chaning commands with a new forward-pipe operator, %>%
> * dplyr also provides a similar mechanism for chaning commands with forward-pipe operator, %>%
* dslabs provides datasets and functions that can be used for data analysis
* lubridate makes it easier to work with dates and times

```R
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
getwd() #displays your working directory
```

## STEP 1: COLLECT DATA

```R
# Change source path of files
q2_2019 = read.csv("../Divvy_Trips_2019_Q2.csv")
q3_2019 = read.csv("../Divvy_Trips_2019_Q3.csv")
q4_2019 = read.csv("../Divvy_Trips_2019_Q4.csv")
q1_2020 = read.csv("../Divvy_Trips_2020_Q1.csv")
```

## STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE

```R
# Compare column names each of the files
# While the names don't have to be in the same order, 
# they DO need to match perfectly before we can use a command to join them into one file
colnames(q3_2019)
colnames(q4_2019)
colnames(q2_2019)
colnames(q1_2020)
```

```R
# Rename columns  to make them consistent with q1_2020 
# (as this will be the supposed going-forward table design for Divvy)

 (q4_2019 <- rename(q4_2019
                  ,ride_id = trip_id
                  ,rideable_type = bikeid
                  ,started_at = start_time
                  ,ended_at = end_time
                  ,start_station_name = from_station_name
                  ,start_station_id = from_station_id
                  ,end_station_name = to_station_name
                  ,end_station_id = to_station_id
                  ,member_casual = usertype))

(q3_2019 <- rename(q3_2019
                  ,ride_id = trip_id
                  ,rideable_type = bikeid
                  ,started_at = start_time
                  ,ended_at = end_time
                  ,start_station_name = from_station_name
                  ,start_station_id = from_station_id
                  ,end_station_name = to_station_name
                  ,end_station_id = to_station_id
                  ,member_casual = usertype))

(q2_2019 <- rename(q2_2019
                  ,ride_id = "01 - Rental Details Rental ID"
                  ,rideable_type = "01 - Rental Details Bike ID"
                  ,started_at = "01 - Rental Details Local Start Time"
                  ,ended_at = "01 - Rental Details Local End Time"
                  ,start_station_name = "03 - Rental Start Station Name"
                  ,start_station_id = "03 - Rental Start Station ID"
                  ,end_station_name = "02 - Rental End Station Name"
                  ,end_station_id = "02 - Rental End Station ID"
                  ,member_casual = "User Type"))
```

```R
# Inspect the dataframes and look for incongruencies
str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)
```

```R
# Convert columns to ensure they are consistent so that they can stack correctly
q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
```

```R
# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)
```

```R
# Remove lat, long, birthyear, and gender fields so all quarterly data consistent
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, tripduration))
```

## STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS

#### Inspect the new table that has been created

```R
colnames(all_trips)  #List of column names
```

```R
dim(all_trips)  #Dimensions of the data frame?
```

```R
head(all_trips)  #See the first 6 rows of data frame. Also tail(all_trips)
```

```R
str(all_trips)  #See list of columns and data types (numeric, character, etc)
```

```R
summary(all_trips)  #Statistical summary of data. Mainly for numerics
```

There are a few problems we will need to fix:
1. In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). We will need to consolidate that from four to two labels.
1. The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
1. We will want to add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency.
1. There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

```R
# In the "member_casual" column, replace "Subscriber" with "member" and "Customer" with "casual"
# Before 2020, Divvy used different labels for these two types of riders ... we will want to make our dataframe consistent with their current nomenclature
# N.B.: "Level" is a special property of a column that is retained even if a subset does not contain any values from a specific level
# Begin by seeing how many observations fall under each usertype

table(all_trips$member_casual)

# Reassign to the desired values (we will go with the current 2020 labels)
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                           ,"Subscriber" = "member"
                           ,"Customer" = "casual"))
```

```R
# Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)
```

```R
# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
```

```R
# Inspect the structure of the columns
str(all_trips)
```

```R
# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
```

```R
# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
```

## STEP 4: CONDUCT DESCRIPTIVE ANALYSIS

```R
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)
```

```R
# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
```

```R
# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday",
                                                                       "Thursday", "Friday", "Saturday"))
```

```R
# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
```

```R
# analyze ridership data by type and weekday
# this will generate a tibble
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%                  #groups by usertype and weekday
  summarise(number_of_rides = n()                       #calculates the number of rides and average duration 
  ,average_duration = mean(ride_length)) %>%            # calculates the average duration
  arrange(member_casual, weekday)                       # sorts
```
![This is an image](https://bayodeolorundare.com/wp-content/uploads/2023/02/rides_weekday_tibble.jpg)

```R
# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title="Total Number Rides of Member & Casual During Weekday",
  caption="Data from 2019-04 to 2020-03")+
  theme(plot.title =element_text(hjust = 0.5,size=15),
  legend.title = element_text(size=18),
  legend.text = element_text(size = 15))
```
![This is an image](https://bayodeolorundare.com/wp-content/uploads/2023/02/rides_weekday-1.jpeg)

```R
# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title="Total Durations of Member & Casual During Weekday",
  caption="Data from 2019-04 to 2020-03")+
  theme(plot.title =element_text(hjust = 0.5,size=15),
  legend.title = element_text(size=18),
  legend.text = element_text(size = 15))
```
![This is an image](https://bayodeolorundare.com/wp-content/uploads/2023/02/avg_duration_weekday-1.jpeg)

```R
# Let's create a visualization for number of rides, per hour of day, by rider type
all_trips_v2 %>% 
  mutate(hour = hour(started_at)) %>% 
  group_by(member_casual, hour) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, hour)  %>% 
  ggplot(aes(x = hour, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title="Total Number of Rides Per Hour of Day of Member & Casual",
  caption="Data from 2019-04 to 2020-03")+
  theme(plot.title =element_text(hjust = 0.5,size=15),
  legend.title = element_text(size=18),
  legend.text = element_text(size = 15))
```
![This is an image](https://bayodeolorundare.com/wp-content/uploads/2023/02/rides_hour_weekday-1.jpeg)

```R
# Let's create a visualization for number of rides, during month of year, by rider type
all_trips_v2 %>% 
  mutate(month = month(started_at, label = TRUE)) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title="Total Number of Rides Per Month of Member & Casual",
  caption="Data from 2019-04 to 2020-03")+
  theme(plot.title =element_text(hjust = 0.5,size=15),
  legend.title = element_text(size=18),
  legend.text = element_text(size = 15))
```
![This is an image](https://bayodeolorundare.com/wp-content/uploads/2023/02/rides_month-1.jpeg)

## STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS

## Summary

casual riders tend to have fewer trips during the weekday than annual members. 
We believe this is due to members taking more rides to and from work. 
We suggest increasing casual riders weekday trips by advertsing at work communute hours (07:00 - 09:00 and 16:00 - 18:00).

We also recommend advertising between May and September. These were the busiest months of the year.

During our analysis we needed to clean the data performing the following tasks:
* renaming columns in the data files
* convert columns so the dataframes are consistent
* then we removed unwanted columns before stacking rows
* renamed the labels in member_casual to consolidate for consistancy
* added additional columns of data that provided additional oppertunities to aggregate data
* added calulcated field for length of ride for all data
* removed ride durations that were negative
* aggregate data to show mean, median, max and min for casual and member riders for analysis

The issues mentioned could have affected the intrgity of this analysis, and we recommend looking into the source of the these problems.

[Kaggle Notebook](https://www.kaggle.com/code/bayodeolorundare/divvy-exercise-case-study?scriptVersionId=119403583) // [Tableau Viz](https://public.tableau.com/app/profile/bayode.olorundare/viz/CasestudyCyclingridertrends/DayofWeekBreakdown) // [Looker Datastudio](https://lookerstudio.google.com/u/0/reporting/e2a64c12-36f6-4879-8441-9cd67f9f9bfc/page/tEnnC)
