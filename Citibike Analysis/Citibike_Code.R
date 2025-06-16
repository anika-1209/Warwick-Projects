# Load required libraries
install.packages("geosphere")
library(dplyr)
library(ggplot2)
library(lubridate)
library(geosphere)


#DATA CLEANING  
bike_data <- read.csv("Citibike_2023.csv")
head(bike_data)

str(bike_data)
bike_data <- bike_data %>%
  filter(start_station_name != "" & end_station_name != "")
#gettign rid of any unwanted spaces
bike_data$started_at <- trimws(bike_data$started_at)
bike_data$ended_at <- trimws(bike_data$ended_at)

bike_data$started_at <- ymd_hms(bike_data$started_at)
bike_data$ended_at <- ymd_hms(bike_data$ended_at)
head(bike_data$started_at)
head(bike_data$ended_at)

bike_data$start_hour <- hour(bike_data$started_at)
bike_data$start_day <- wday(bike_data$started_at, label = TRUE)
bike_data$start_month <- month(bike_data$started_at, label = TRUE)

bike_data$trip_duration <- as.numeric(difftime(bike_data$ended_at, bike_data$started_at, units = "mins"))


# Trip Pattern Analysis
# Group and summarize trips by station
busiest_stations <- bike_data %>%
  group_by(start_station_name) %>%
  summarise(
    total_trips = n(),
    avg_trip_duration = mean(trip_duration, na.rm = TRUE),
    latitude = mean(start_lat),
    longitude = mean(start_lng)
  ) %>%
  arrange(desc(total_trips))

# Visualize hourly and daily trip patterns
ggplot(bike_data, aes(x = start_hour)) +
  geom_bar(fill = "lightblue", alpha = 0.7) +
  labs(title = "Hourly Trip Distribution", x = "Hour of the Day", y = "Number of Trips") +
  theme_minimal()

ggplot(bike_data, aes(x = start_day)) +
  geom_bar(fill = "lightgreen", alpha = 0.7) +
  labs(title = "Daily Trip Distribution", x = "Day of the Week", y = "Number of Trips") +
  theme_minimal()


ggplot(bike_data, aes(x = start_month)) +
  geom_bar(fill = "lightpink", alpha = 0.7) +
  labs(title = "Monthly Trip Distribution", x = "Day of the Week", y = "Number of Trips") +
  theme_minimal()

# ---- User Behavior Analysis ----
# Popular routes
pop_routes <- bike_data %>%
  group_by(start_station_name, end_station_name) %>%
  summarise(total_trips = n()) %>%
  arrange(desc(total_trips))

# Calculate average trip distance by route
avg_distance_by_route <- bike_data %>%
  group_by(start_station_name, end_station_name) %>%
  summarise(avg_trip_distance_km = mean(trip_distance, na.rm = TRUE))

# Join avg_trip_distance to pop_routes
pop_routes_with_distance <- pop_routes %>%
  left_join(avg_distance_by_route, by = c("start_station_name", "end_station_name"))

# View the updated pop_routes data
head(pop_routes_with_distance, 10)

# Calculate trip distances
bike_data$trip_distance <- distHaversine(
  cbind(bike_data$start_lng, bike_data$start_lat),
  cbind(bike_data$end_lng, bike_data$end_lat)
) / 1000 # Convert meters to kilometers



# -----distance by hour----
# Aggregate data by hour
avg_distance_by_hour <- bike_data %>%
  group_by(start_hour) %>%
  summarise(avg_trip_distance = mean(trip_distance, na.rm = TRUE))

# Plot average trip distance by hour
ggplot(avg_distance_by_hour, aes(x = start_hour, y = avg_trip_distance)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  scale_x_continuous(breaks = seq(0, 23, by = 1)) +  # Ensure all hours (0-23) are displayed
  labs(
    title = "Average Trip Distance by Hour of the Day",
    x = "Hour of the Day",
    y = "Average Trip Distance (km)"
  ) +
  theme_minimal()

# ---- Clustering Stations by Usage ----
# Aggregate station metrics
station_usage <- bike_data %>%
  group_by(start_station_name) %>%
  summarise(
    total_start_trips = n(),
    avg_lat = mean(start_lat),
    avg_lng = mean(start_lng)
  )

# Perform k-means clustering
set.seed(123)
clusters <- kmeans(station_usage[, c("avg_lat", "avg_lng")], centers = 4)
# Add cluster information
station_usage$cluster <- clusters$cluster

ggplot(station_usage, aes(x = avg_lng, y = avg_lat, color = as.factor(cluster))) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Clustering of Stations by Usage", x = "Longitude", y = "Latitude") +
  theme_minimal()



## Regression Analysis
# Aggregating data at the station level
station_data <- bike_data %>%
  group_by(start_station_name) %>%
  summarise(
    total_trips = n(),
    avg_trip_distance = mean(trip_distance, na.rm = TRUE),
    avg_trip_duration = mean(trip_duration, na.rm = TRUE),
    latitude = mean(start_lat, na.rm = TRUE),
    longitude = mean(start_lng, na.rm = TRUE)
  )

# Fit a regression model
station_popularity_model <- lm(total_trips ~ avg_trip_distance + avg_trip_duration + latitude + longitude, data = station_data)

# Model summary
summary(station_popularity_model)


# Add predicted values to the station_data
station_data$predicted_trips <- predict(station_popularity_model, station_data)

# Create a scatter plot for predicted vs. actual total trips
library(ggplot2)

ggplot(station_data, aes(x = total_trips, y = predicted_trips)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Predicted vs. Actual Total Trips",
    x = "Actual Total Trips",
    y = "Predicted Total Trips"
  ) +
  theme_minimal()


head(busiest_stations,10)
head(pop_routes,10)