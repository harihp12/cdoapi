#' Simplify weather data for multi-station cities
#' 
#' For multi-station cities, aggregates numeric values of weather data, calculates 
#' their mean value for each day and simplifies the columns to an easy-to-use daily
#' data format.
#'
#' @param weatherdata Data frame containing the result of \code{\link{get_weatherdata}}
#' function
#'
#' @return
#' Modified data frame containing one row for each day; 2 columns containing date 
#' and mean weather data value for that date.
#'
#' @examples
#' \dontrun{
#' # First get your API-token here: https://www.ncdc.noaa.gov/cdo-web/token
#' # Then, set it as system variable (as character variable):
#' Sys.setenv("NOAA_TOKEN" = "YOUR_TOKEN_GOES_IN_HERE")
#' 
#' # Get weatherdata for Berlin (only September)
#' weatherdata = get_weatherdata("TAVG", "GM000001", "2018-09-01", "2018-09-23")
#' 
#' # Since there are 2 stations with weather data for Berlin, we modify the data
#' weatherdata_simplified = simplify_weatherdata(weatherdata)
#' }
#' 
#' @importFrom dplyr summarise group_by
#' @importFrom stringr %>%
#' 
#' @export

# ------------------------------------------------------------------------------
# Aggregate and simplify weatherdata across different stations by day
# ------------------------------------------------------------------------------

simplify_weatherdata = function(weatherdata) {
  
  # ----------------------------------------------------------------------------
  # Stop Conditions
  # ----------------------------------------------------------------------------

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


