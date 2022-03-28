# UBER DATA ANALYSIS
# problem statement : Analyze the Uber Pickups in New York City dataset.
## Importing Essential Packages
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)

# input
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)

#lets create some vector color that we will implement in our plot as it make our plot more attractive
colors = col("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")

# Reading the data  0=- all the data from April 2014 to september 2014 and we are storing the dat ain varibal ename of month
apr_data <- read.csv("uber-raw-data-apr14.csv")
may_data <- read.csv("uber-raw-data-may14.csv")
jun_data <- read.csv("uber-raw-data-jun14.csv")
jul_data <- read.csv("uber-raw-data-jul14.csv")
aug_data <- read.csv("uber-raw-data-aug14.csv")
sep_data <- read.csv("uber-raw-data-sep14.csv")

# we are trying to combine all the data into single frame with name data-2014
data_2014 <- rbind(apr_data,may_data, jun_data, jul_data, aug_data, sep_data)


# perform the appropriate formatting of Date.Time column. Then, we will proceed to create factors of time objects like day, month, year etc.
data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")
data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)
data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))
data_2014$year <- factor(year(data_2014$Date.Time))
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE))

# Convert some of them into factor format
data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))

#Visualization part - Plotting the trips bs the hours in a day
## use the ggplot function to plot the number of trips that the passengers had made in a day. 
## We will also use dplyr to aggregate our data. In the resulting visualizations, 
## we can understand how the number of passengers fares throughout the day.

## We observe that the number of trips are higher in the evening around 5:00 and 6:00 PM.

hour_data <- data_2014 %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n()) 
datatable(hour_data)

ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)
month_hour <- data_2014 %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())
ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma)

## We observe that the number of trips are higher in the evening around 5:00 and 6:00 PM.











