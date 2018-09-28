################################################################################

# ------------------------------------------------------------------------------
# DESCRIPTION
# ------------------------------------------------------------------------------

# This function takes as input a data frame that was returned by the 
# get_weatherdata function. 

# The average_stations function comes into play, when the user has extracted
# weatherdata with get_weatherdata for a city in which several different 
# weather stations exist. If this is the case, then there will be several 
# data points for the value (e.g. TAVG) for every day for the city, namely 
# one data point for each station that has information about the city.

# average_stations calculates the mean of the value column from the data frame 
# that was returned by get_weatherdata. The mean is calculated across stations 
# for every day. Hence, the average_stations returns a modified data frame 
# that really contains one value per day for the given city.


# ------------------------------------------------------------------------------
# Required Packages
# ------------------------------------------------------------------------------

library(dplyr)

# ------------------------------------------------------------------------------
# Example (Run this after loading the packages, functions etc.)
# ------------------------------------------------------------------------------

# That's Julius NOAA_TOKEN (get yours here: https://www.ncdc.noaa.gov/cdo-web/token)
Sys.setenv("NOAA_TOKEN" = "cfcoWaQRykPCelOlZitLALPtKQuhBXnR")

# Get weatherdata for Berlin (only September)
head(locations[which(locations$country == "GM"), ]) # Pick GM000001 (Berlin)
weatherdata = get_weatherdata("TAVG", "GM000001", "2018-09-01", "2018-09-23")

# Since there are 2 stations with weather data for Berlin, we modify the data

weatherdata_averaged_across_stations = average_stations(weatherdata)

################################################################################


# ------------------------------------------------------------------------------
# Aggregate weatherdata accross different stations by day
# ------------------------------------------------------------------------------

average_stations = function(weatherdata) {
  
  # ----------------------------------------------------------------------------
  # Stop Conditions
  # ----------------------------------------------------------------------------
  
  # Token ----------------------------------------------------------------------
  
  # Get token for NOAA connection
  mytoken = Sys.getenv("NOAA_TOKEN")
  
  # Check if parameter is of class character
  stopifnot(class(mytoken) == "character")
  
  # Check if simple query returns 200 code (success)
  url = "https://www.ncdc.noaa.gov/cdo-web/api/v2/locations"
  resp = GET(url, add_headers(token = mytoken))
  stopifnot(resp$status_code == 200)
  
  # weatherdata ----------------------------------------------------------------
  
  # Check if parameter is of class data frame
  stopifnot(class(weatherdata) == "data.frame")
  
  # Check if there is a numeric "value" column
  stopifnot(is.numeric(weatherdata[, "value"]))
  
  # Check if there is a character "station" column
  stopifnot(is.character(weatherdata[, "station"]))
  
  # Check if there is a character "date" column
  stopifnot(is.character(weatherdata[, "date"]))
  
  # Check if there is a character "datatype" column with 1 unique value
  stopifnot(is.character(weatherdata[, "datatype"]))
  stopifnot(length(unique(weatherdata[, "datatype"])) == 1)
  
  # ----------------------------------------------------------------------------
  # Average value across stations by date
  # ----------------------------------------------------------------------------
  
  # Compute the average values and save a tibble (no. rows = no. unique dates)
  tibble = weatherdata %>% group_by(date) %>% summarise(avg_value = mean(value))
  
  # Convert tibble back to data frame
  weatherdata_avg = as.data.frame(tibble)
  
  # Rename avg_value column according to datatype
  idx = which(colnames(weatherdata_avg) == "avg_value")
  datatype = as.character(unique(weatherdata[, "datatype"]))
  colnames(weatherdata_avg)[idx] = datatype
  
  # Reduce the date column to the actual date instead of date and time
  weatherdata_avg$date = substr(weatherdata_avg$date, 1, 10)
  
  # ----------------------------------------------------------------------------
  # Return modified data frame
  # ----------------------------------------------------------------------------
  
  return(weatherdata_avg)
  
}


