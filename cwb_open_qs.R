# Open Questions.

# importing the essential libraries.

library(ggplot2)
library(dplyr)
library(data.table)
library(leaflet)
library(xts)
library(zoo)

# a. In what trips can you confidently use respective means as measures of central tendency to estimate fare, 
#    time taken, etc.

# For Fare.

# Summary Statistics, IQR and Outliers.
summary(trip_fare$fare_amount)
IQR(trip_fare$fare_amount)
outlier1 <- 6.5 - (1.5*7.5)  # (Q1 - 1.5*IQR) anything smaller than this value is an outlier
outlier2 <- 14 + (1.5*7.5)   # (Q3 + 1.5*IQR) anything greater than this value is an outlier

# Minimum Fare Amount.
min(trip_fare$fare_amount)

# Removing Outliers.
trip_fare_no_outliers <- which(trip_fare$fare_amount > 25.25)
mean_central_tendency_fare <- trip_fare[-trip_fare_no_outliers,"fare_amount"]

# Creating a histogram without outliers. For these trips mean as a measure of central tendency will be good to estimate fare. 
p3 <- ggplot(mean_central_tendency_fare, aes(x=fare_amount)) + 
  geom_histogram(color="black", fill="gray") +
  theme_bw()+labs(x="Fare Amount",y="Frequency",title="Fare Amount Distribution without outliers")

p3 + geom_vline(aes(xintercept=mean(fare_amount)),color="blue", linetype="dashed", size=1) 


# For Time Taken.

# Summary Statistics, IQR and Outliers.
summary(trip_data$trip_time_in_secs)
IQR(trip_data$trip_time_in_secs)
outlier1_sec <- 360 - (1.5*600)  # (Q1 - 1.5*IQR) anything smaller than this value is an outlier
outlier2_sec <- 960 + (1.5*600)   # (Q3 + 1.5*IQR) anything greater than this value is an outlier

# Minimum Time Taken.
min(trip_data$trip_time_in_secs)

# Removing Outliers.
trip_fare_no_outliers_sec <- which(trip_data$trip_time_in_secs > 1860 | trip_data$trip_time_in_secs < 0)
mean_central_trip_sec <- trip_data[-trip_fare_no_outliers_sec,"trip_time_in_secs"]

# # Creating a histogram without outliers. For these trips mean as a measure of central tendency will be good to time taken. 
p4 <- ggplot(mean_central_trip_sec, aes(x=trip_time_in_secs)) + 
  geom_histogram(color="black", fill="gray") +
  theme_bw()+labs(x="Trip Time in seconds",y="Frequency",title="Trip Time Distribution without outliers")

p4 + geom_vline(aes(xintercept=mean(trip_time_in_secs)),color="blue", linetype="dashed", size=1) 

# b. Can we build a model to predict fare and tip amount given pick up and drop off coordinates, time of day and week?

## Yes you can build a model where pick up and drop off coordinates, time and day of the week could be the features for predicting the fare amount of the trip. 

# c. If you were a taxi owner, how would you maximize your earnings in a day?

## From the previous analysis I already know that 6pm - 10pm is the most busiest time of the day. And I also know which are the top locations for pickup, so to maximize my earnings I will target the top areas from 6pm in the evening to 10 pm in the night.

# d. If you were a taxi owner, how would you minimize your work time while retaining the
# average wages earned by a typical taxi in the dataset?

# Binding the relevant columns from both the dataframes.
avg_payment_low_time <- cbind(trip_data[,c(9,11,12)], trip_fare[,c(6,11)])

# Summary Statistics.
summary(avg_payment_low_time$trip_time_in_secs)
summary(avg_payment_low_time$total_amount)     # mean wages is 15 dollars approx

# Subsetting dataframe with average Fare Amount and minimum trip time in seconds. I am considering it as 60 seconds.
mean_wages_low_trip <- avg_payment_low_time %>% 
  filter(total_amount >= 15) %>%
  filter(trip_time_in_secs < 61) %>%
  filter(trip_time_in_secs > 10)

# Plotting the map
leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addAwesomeMarkers(
    lng=mean_wages_low_trip$pickup_longitude, 
    lat=mean_wages_low_trip$pickup_latitude,
    clusterOptions = markerClusterOptions()
  )

# e. If you run a taxi company with 10 taxis, how would you maximize your earnings?

# I have already located the top 10 locations where there is lot of activity and I also know the busiest hours for taxi business, so I will ask my 10 taxi drivers to drive in the 10 most popular locations at the busiest time. That way I can maximize my earnings. 

