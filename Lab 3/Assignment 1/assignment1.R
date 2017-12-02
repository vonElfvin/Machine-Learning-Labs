# Appendix 1

################
# Assignment 1 #
################

# Libraries
library(geosphere)

# Setup
set.seed(12345)
stations.dataframe = read.csv("stations.csv", fileEncoding="latin1")
temps.dataframe = read.csv("temps50k.csv", fileEncoding="latin1")
dataframe = merge(stations.dataframe, temps.dataframe, by="station_number")
times = c("04:00:00","06:00:00","08:00:00","10:00:00","12:00:00","14:00:00",
          "16:00:00","18:00:00","20:00:00","22:00:00","24:00:00")
temp = numeric(length(times))

# Functions
# Returns the data excluding those with dates after given date
get_filtered_data = function(date){
  dates.valid = as.Date(dataframe$date)<as.Date(date)
  return(dataframe[dates.valid,])
}

get_distance = function(p1,p2){
  return(distHaversine(p1,p2))
}

get_time_difference = function(time, times){
  time = strptime(time, format="%H:")
  times = strptime(times, format="%H:")
  time.diff = as.numeric(difftime(time,times))
  time.diff[which(time.diff>12)]=time.diff[which(time.diff>12)]-24
  time.diff[which(time.diff<(-12))]=time.diff[which(time.diff<(-12))]+24
  return(timediff)
}

get_date_difference = function(date, dates){
    date = as.Date(date)
    dates = as.Date(dates)
    date.diff = as.numeric(difftime(date,dates))
    date.diff = date.diff %% 365
    date.diff[which(date.diff<(-183))]=date.diff[which(date.diff>183)]+365
    return(date.diff)
}

h_distance = function(point, data){
  points = matrix(c(data$latitude, data$longitude), nrow=dim(data[1]), ncol=2)
  distances = get_distance(point, points)
  # x = sqrt(sum(distances^2))
  return(density(distances, kernel="gaussian"))
}


h_date = function(date, data){
  days = get_date_difference(date, data$date)
  return(density(days, kernel="gaussian"))
}


h_time = function(time, date){
  times = get_times_difference(time, data$time)
  return(density(times, kernel="gaussian"))
}

# Implementation

lat = 58.4274
long = 14.826
point = c(lat, long)
date = "2099-08-04"
data.filtered = get_filtered_data(date)
plot(h_distance(point, data.filtered))
plot(h_date(date, data.filtered))
plot(h_time(times, data.filtered))

