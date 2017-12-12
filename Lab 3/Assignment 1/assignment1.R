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
get_filtered_data_date = function(date){
  dates.valid = as.Date(dataframe$date)<=as.Date(date)
  return(dataframe[dates.valid,])
}

get_filtered_data_time = function(data, date, time){
  diff = as.numeric(difftime(strptime(time, format="%H"), strptime(data$time, format="%H"), units="hours"))
  times.valid = as.Date(data$date)<as.Date(date) | (diff < 0)
  return(data[times.valid,])
}

get_distances = function(p1,p2){
  return(distHaversine(p1,p2))
}

get_time_differences = function(time, times){
  time = strptime(time, format="%H:")
  times = strptime(times, format="%H:")
  times.diff = as.numeric(difftime(time,times, units="hours"))
  times.diff = times.diff %% 24
  return(times.diff)
}

get_date_differences = function(date, dates){
    date = as.Date(date)
    dates = as.Date(dates)
    date.diff = as.numeric(difftime(date,dates))
    date.diff = date.diff
    return(date.diff)
}

get_gaussian_kernel = function(u){
  return(exp(-u^2))
}

get_gaussian_kernel_distance = function(point, data, h.distance){
  points = matrix(c(data$latitude, data$longitude), nrow=dim(data[1]), ncol=2)
  distances = get_distances(point, points)
  u.distance = distances/h.distance
  gaussian.kernel.distance = get_gaussian_kernel(u.distance)
  return(gaussian.kernel.distance)
}

get_gaussian_kernel_date = function(date, data, h.days){
  days = get_date_differences(date, data$date)
  u.days = days/h.days
  gaussian.kernel.date = get_gaussian_kernel(u.days)
  return(gaussian.kernel.date)
}

get_gaussian_kernel_time = function(time, data, h.time){ 
  times = get_time_differences(time, data$time)
  u.times = times/h.time
  gaussian.kernel.time = get_gaussian_kernel(u.times)
  return(gaussian.kernel.time)
}

y_sum = function(kernel.distance, kernel.date, kernel.time, data){
  kernel.sum = kernel.distance + kernel.date + kernel.time
  y.sum = (kernel.sum %*% data$air_temperature) / sum(kernel.sum)
  return(y.sum)
}

y_prod = function(kernel.distance, kernel.date, kernel.time, data){
  kernel.prod = kernel.distance * kernel.date * kernel.time
  y.prod = (kernel.prod %*% data$air_temperature) / sum(kernel.prod)
  return(y.prod)
}

# Find reasonable kernal value
xgrid.distance = seq(0,1000000)
xgrid.date = seq(0,365)
xgrid.time = seq(0:24)

h_distance = 100000
h_date = 7
h_time = 5

plot(xgrid.distance, get_gaussian_kernel(xgrid.distance/h_distance), type="l")
plot(xgrid.date, get_gaussian_kernel(xgrid.date/h_date), type="l")
plot(xgrid.time, get_gaussian_kernel(xgrid.time/h_time), type="l")

# Implementation
stockholm_lat = 59.3293235
stockholm_long = 18.0685808
point = c(stockholm_lat, stockholm_long)
date = "2004-06-28"
data.filtered.date = get_filtered_data_date(date)
y.prod = numeric(length(times))
y.sum = numeric(length(times))
for(i in 1:length(times)){
  data.filtered.time = get_filtered_data_time(data.filtered.date, date, times[i])
  gaussian.kernel.distance = get_gaussian_kernel_distance(point, data.filtered.time, h_distance)
  gaussian.kernel.date = get_gaussian_kernel_date(date, data.filtered.time, h_date)
  gaussian.kernel.time = get_gaussian_kernel_time(times[i], data.filtered.time, h_time)
  y.sum[i] = y_sum(gaussian.kernel.distance, gaussian.kernel.date, gaussian.kernel.time, data.filtered.time)
  y.prod[i] = y_prod(gaussian.kernel.distance, gaussian.kernel.date, gaussian.kernel.time, data.filtered.time)
}
plot(y.sum, xaxt='n')
axis(1, at=1:length(times), labels=times)

plot(y.prod, xaxt='n')
axis(1, at=1:length(times), labels=times)

