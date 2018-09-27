################################################################################

# Note: First, the user should run the get_locations functions to see all the 
# city IDs for which we can query data with this function here: get_weatherdata

# Note: The weatherdata from all stations in the specified city is returned. 
# Hence, it might make sense to create another function that averages the values
# over different stations by date.

################################################################################


#' Get weatherdata for specific time period and cities
#' 
#' get_weatherdata allows the user to get weatherdata for a specified 
#' 
#' a) data_type (e.g. TAVG, TMIN, TMAX, SNOW, PRCP, 
#'    others: https://docs.opendata.aws/noaa-ghcn-pds/readme.html)
#' b) city_ID (which should be taken from the output of get_locations)
#' c) start_date (in the format "YYYY-MM-DD")
#' d) end_date (in the format "YYYY-MM-DD")
#'
#' @param data_type Character
#' @param city_ID Character
#' @param start_date Character, valid date
#' @param end_date Character, valid date
#'
#' @return
#'
#' @examples
#' \dontrun{
#' # Get location data to select the city_IDs we are interested in
#' locations = get_locations()
#' 
#' # Get weatherdata for Linköping (only September)
#' head(locations[which(locations$country == "SW"), ]) # Pick SW000006 (Linköping)
#' weatherdata = get_weatherdata("TAVG", "SW000006", "2018-09-01", "2018-09-23")
#' 
#' # Get weatherdata for Berlin (only September)
#' head(locations[which(locations$country == "GM"), ]) # Pick GM000001 (Berlin)
#' weatherdata = get_weatherdata("TAVG", "GM000001", "2018-09-01", "2018-09-23")
#' }
#' 
#' @export
#' 
################################################################################

# ------------------------------------------------------------------------------
# Get weatherdata for specific time period and cities 
# ------------------------------------------------------------------------------

get_weatherdata = function(data_type, city_ID, start_date, end_date) {

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
  
  # data_type --------------------------------------------------------------------
  
  # Check if parameter is of class character
  stopifnot(class(data_type) == "character")
  
  # city_ID --------------------------------------------------------------------
  
  # Check if parameter is of class character
  stopifnot(class(city_ID) == "character")
  
  # start_date -----------------------------------------------------------------
  
  # Check if parameter is of class character
  stopifnot(class(start_date) == "character")
  
  # end_date -------------------------------------------------------------------
  
  # Check if parameter is of class character
  stopifnot(class(end_date) == "character")
  
  # ----------------------------------------------------------------------------
  # Data Retrieval
  # ----------------------------------------------------------------------------
  
  # Get information about specified city and time period
  url_base = "https://www.ncdc.noaa.gov/cdo-web/api/v2/data?datasetid=GHCND"
  url_base = paste0(url_base, "&locationid=CITY:", city_ID, 
               "&datatypeid=", data_type, 
               "&startdate=", start_date, 
               "&enddate=", end_date)
  
  resp = GET(url_base, add_headers(token = mytoken))
  parsed = fromJSON(content(resp, type = "text", encoding = "UTF-8"))
  
  number = parsed$metadata$resultset$count # number of observations
  p = ceiling(number / 1000) - 1 # no. of required loops (due to max limit 1000)
  
  # Print a quick info for the user
  cat("We are getting", number, "observations for you. Please be patient...")
  
  # Initialize dataset with information about first 1000 observations
  url = paste0(url_base, "&limit=1000")
  resp = GET(url, add_headers(token = mytoken))
  parsed = fromJSON(content(resp, type = "text", encoding = "UTF-8"))
  data = parsed$results

  # Add the remaining observations to the data set
  for (i in 1:p) {
    
    if (i == p) {
      
      # Pull the last bulk of observations from NOAA
      limit = number - p * 1000
      url = paste0(url_base, "?limit=", limit , "&offset=", i * 1000 + 1)
      resp = GET(url, add_headers(token = mytoken))
      parsed = fromJSON(content(resp, type = "text", encoding = "UTF-8"))
      
      # Add information of locations do the data set
      data = rbind(data, parsed$results)
      
    } else {
      
      # Pull the next 1000 observations from NOAA
      url = paste0(url_base, "&limit=1000", "&offset=", i * 1000 + 1)
      resp = GET(url, add_headers(token = mytoken))
      parsed = fromJSON(content(resp, type = "text", encoding = "UTF-8"))
      
      # Add information of locations do the data set
      data = rbind(data, parsed$results)
      
    }
    
  }
  
  # ----------------------------------------------------------------------------
  # Data Modifications and Return
  # ----------------------------------------------------------------------------
  
  return(data)
  
}



