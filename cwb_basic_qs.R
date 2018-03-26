# Basic Questions.

# importing the essential libraries.

library(ggplot2)
library(dplyr)
library(data.table)
library(leaflet)
library(xts)
library(zoo)

# import the data using fread() as data size is huge.

trip_fare <- fread("trip_fare_4.csv")
trip_data <- fread("trip_data_4.csv")

print(object.size(trip_fare),units="Gb")
print(object.size(trip_data),units="Gb")

# The data size is big ~ 15 million rows. For most of the questions I am using the entire data, only for some questions I am using sample data.


# Basic questions

# a. What is the distribution of number of passengers per trip?

# convert the variable into categorical variable and create a separate dataframe.
passenger_per_trip <- as.data.frame(table(as.factor(trip_data$passenger_count))) 

# I calculated the percentages for each passenger count class.Creating a column for percentages.
passenger_per_trip$percent <- c(0,71,13,4,2,6,4,0,0)

# Changing the column names to make it more readable.
colnames(passenger_per_trip) <- c("Number_of_Passengers","Frequency","Percent")

# Creating the barplot to show the passenger distribution.
ggplot(data=passenger_per_trip[-c(1,8,9),], aes(x=Number_of_Passengers, y=Frequency)) +
  geom_bar(stat="identity",fill="steelblue",width = 0.75)+
  geom_text(aes(label=Percent), vjust=-0.3, size=3,color='black')+
  labs(title="Passenger Distribution", x="Number of Passengers in a trip", y = "Frequency")+
  theme_minimal()

# b. What is the distribution of payment_type?

# convert the variable into categorical variable and create a separate dataframe.
payment_type <- as.data.frame(table(as.factor(trip_fare$payment_type)))

# I calculated the percentages for each payment type class.Creating a column for percentages.
payment_type$percent <- c(53.5,46.5,0.3,0.6,0.1)

# Changing the column names to make it more readable.
colnames(payment_type) <- c("Payment_Type","Frequency","Percent")

# Creating the barplot to show the payment type distribution.
ggplot(data=payment_type, aes(x=Payment_Type, y=Frequency)) +
  geom_bar(stat="identity", fill="steelblue",width = 0.75)+
  geom_text(aes(label=Percent), vjust=-0.3, size=3,color='black')+
  labs(title="Payment Type Distribution", x="Payment Type", y = "Frequency")+
  theme_minimal()

# c. What is the distribution of fare amount?

# Looking at the summary statistics.
summary(trip_fare$fare_amount)

# Creating a histogram for fare amount distribution.
p <- ggplot(trip_fare, aes(x=fare_amount)) + 
  geom_histogram(color="black", fill="gray") +
  labs(title="Fare Amount Distribution", x="Fare Amount", y = "Frequency")+
  theme_minimal()

# Adding a line for mean.
p + geom_vline(aes(xintercept=mean(fare_amount)),color="blue", linetype="dashed", size=1) 


# d. What is the distribution of tip amount?

# Looking at the summary statistics.
summary(trip_fare$tip_amount)

# Creating a histogram for tip amount distribution.
p1 <- ggplot(trip_fare, aes(x=tip_amount)) + 
  geom_histogram(color="black", fill="gray") +
  labs(title="Tip Amount Distribution", x="Tip Amount", y = "Frequency")+
  theme_minimal()

# Adding a line for mean.
p1 + geom_vline(aes(xintercept=mean(tip_amount)),color="blue", linetype="dashed", size=1)  

# e. What is the distribution of total amount?

# Looking at the summary statistics.
summary(trip_fare$total_amount)

# Creating a histogram for total amount distribution.
p2 <- ggplot(trip_fare, aes(x=total_amount)) + 
  geom_histogram(color="black", fill="gray") +
  labs(title="Total Amount Distribution", x="Total Amount", y = "Frequency")+
  theme_minimal()

# Adding a line for mean.
p2 + geom_vline(aes(xintercept=mean(total_amount)),color="blue", linetype="dashed", size=1) 


# f. What are top 5 busiest hours of the day?

# Creating a dataframe which has the pickup hour information. 
pickup_hours <- as.data.frame(table(as.factor(substr(as.character(trip_data$pickup_datetime),12,13))))

# Changing the column names to make it more readable.
colnames(pickup_hours) <- c("Hour","Frequency")

# Sorting the dataframe in descending order.
pickup_hours <- pickup_hours[with(pickup_hours, order(-Frequency)), ]

# Creating a dataframe which has the dropoff hour information. 
dropoff_hours <- as.data.frame(table(as.factor(substr(as.character(trip_data$dropoff_datetime),12,13))))

# Changing the column names to make it more readable.
colnames(dropoff_hours) <- c("Hour","Frequency")

# Sorting the dataframe in descending order.
dropoff_hours <- dropoff_hours[with(dropoff_hours, order(-Frequency)), ]

# Creating a barplot to see the busiest pickup hours.
ggplot(pickup_hours,aes(x=reorder(Hour,Frequency), y=Frequency)) +
  geom_bar(stat='identity',fill = "#F4A582",width = 0.75) + 
  coord_flip()+
  labs(title="Busiest Pickup Hours", x="Hour", y = "Frequency")+
  theme_light()

# Creating a barplot to see the busiest dropoff hours.
ggplot(dropoff_hours,aes(x=reorder(Hour,Frequency), y=Frequency)) +
  geom_bar(stat='identity',fill = "#F4A582",width = 0.75) + 
  coord_flip()+
  labs(title="Busiest Dropoff Hours", x="Hour", y = "Frequency")+
  theme_light()

# g. What are the top 10 busiest locations of the city?

# Creating sample data.
sample_trip_data <- trip_data[1:1000000,]

# Creating a NYC map plot using leaflet. 
leaflet() %>% #initialize object
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(data = sample_trip_data[1:10000,], lng = ~ pickup_longitude, lat = ~ pickup_latitude, radius = 0.3,
                   color = "blue")

# Creating a NYC map to see the top locations for pickup. 
leaflet() %>%
  addTiles() %>%  
  addAwesomeMarkers(
    lng=sample_trip_data$pickup_longitude, 
    lat=sample_trip_data$pickup_latitude,
    clusterOptions = markerClusterOptions()
  )

# h. Which trip has the highest standard deviation of travel time?

# standard deviation of trip time.
sd(trip_data$trip_time_in_secs)

# summary statistics of trip time
summary(trip_data$trip_time_in_secs)

# Checking which trips has the highest deviation.
trip_data$trip_time_deviation <- (trip_data$trip_time_in_secs - 550.4414) ^ 2
which(trip_data$trip_time_deviation >= 105053451)

# These two trips had travel time of 3 hours
trip_data[9093472,"trip_time_in_secs"]  
trip_data[9094070,"trip_time_in_secs"]

# i. Which trip has most consistent fares?

# In this case you may consider either fare amount or total amount, I am considering fare amount.

# Summary Statistics of fare amount.
summary(as.numeric(trip_fare$fare_amount))

# Inter-Quartile range.
IQR(trip_fare$fare_amount)

# Outliers.
outlier1 <- 6.5 - (1.5*7.5)  # (Q1 - 1.5*IQR) anything smaller than this value is an outlier
outlier2 <- 14 + (1.5*7.5)   # (Q3 + 1.5*IQR) anything greater than this value is an outlier

# So trips whose fare amount are in the range of -4.75$ and 25.25$ are trips with consistent fares. 
# Since a trip cannot have negative fare we will consider the range as 2.5$ to 25.25$ for trips with consistent fares.
# Minimum fare amount for a trip is 2.5$ 

# The trip with most consistent fares is the one which has fare amount as the median value

most_consistent <- which(trip_fare$fare_amount == 9.50)

# There are 566,348 trips with median fare amount values which is the most consistent fare amount.


  