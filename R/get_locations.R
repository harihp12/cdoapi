#' Get information about available cities
#' 
#' \code{get_locations} is a function that lists all the available cities for 
#' which the data can be queried with \code{get_weatherdata}
#'
#' @return
#' Data frame containing information about the available cities.
#'
#' @examples
#' 
#' locations = get_locations()
#' head(locations)
#' 
#' head(locations[which(locations$country == "SW"), ]) # Locations in Sweden
#' head(locations[which(locations$country == "GM"), ]) # Locations in Germany
#' 
#' @export
################################################################################

# ------------------------------------------------------------------------------
# Get information about available cities
# ------------------------------------------------------------------------------

get_locations = function() {
  
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
  
  # ----------------------------------------------------------------------------
  # Data Retrieval
  # ----------------------------------------------------------------------------
  
  # Get information about number of available locations
  url = "https://www.ncdc.noaa.gov/cdo-web/api/v2/locations"
  resp = GET(url, add_headers(token = mytoken))
  parsed = fromJSON(content(resp, type = "text", encoding = "UTF-8"))
  
  number = parsed$metadata$resultset$count # number of locations
  p = ceiling(number / 1000) - 1 # no. of required loops (due to max limit 1000)
  
  # Print a quick info for the user
  cat("We are getting", number, "locations for you. Please be patient...")
  
  # Initialize dataset with information about first 1000 locations
  url = "https://www.ncdc.noaa.gov/cdo-web/api/v2/locations?limit=1000"
  resp = GET(url, add_headers(token = mytoken))
  parsed = fromJSON(content(resp, type = "text", encoding = "UTF-8"))
  data = parsed$results
  
  # Add the remaining location rows to the data set
  for (i in 1:p) {
    
    if (i == p) {
      
      # Pull the last bulk of locations from NOAA
      url_base = "https://www.ncdc.noaa.gov/cdo-web/api/v2/locations"
      limit = number - p * 1000
      url = paste0(url_base, "?limit=", limit , "&offset=", i * 1000 + 1)
      resp = GET(url, add_headers(token = mytoken))
      parsed = fromJSON(content(resp, type = "text", encoding = "UTF-8"))
      
      # Add information of locations do the data set
      data = rbind(data, parsed$results)
      
    } else {
      
      # Pull the next 1000 locations from NOAA
      url_base = "https://www.ncdc.noaa.gov/cdo-web/api/v2/locations?limit=1000"
      url = paste0(url_base, "&offset=", i * 1000 + 1)
      resp = GET(url, add_headers(token = mytoken))
      parsed = fromJSON(content(resp, type = "text", encoding = "UTF-8"))
      
      # Add information of locations do the data set
      data = rbind(data, parsed$results)
      
    }
    
  }
  
  # ----------------------------------------------------------------------------
  # Data Modifications and Return
  # ----------------------------------------------------------------------------
  
  # Split the name column (with City, Countrycode into 2 columns)
  data$country = substr(data$name, nchar(data$name)-2+1, nchar(data$name))
  data$city = substr(data$name, 1, nchar(data$name)-4)
  
  # Remove the name column
  data = data[, -which(colnames(data) %in% "name")]
  
  # Modify city ID (by removing a "CITY:" prefix that is not part of the id)
  data$id = substr(data$id, 6, nchar(data$id))
  
  # Return data
  return(data)
  
}

# That's Julius NOAA_TOKEN (get yours here: https://www.ncdc.noaa.gov/cdo-web/token)
#Sys.setenv("NOAA_TOKEN" = "cfcoWaQRykPCelOlZitLALPtKQuhBXnR")


