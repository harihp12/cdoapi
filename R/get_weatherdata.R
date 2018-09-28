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
#' Note: The weatherdata from all stations in the specified city is returned. The
#' function average_stations can be used to automatically modify the data returned
#' by the get_weatherdata function so that there is only one avg. value per date.
#'
#' Note: For this function, get_weatherdata, we also check the city_ID, the 
#' start_date and end_date. Because this requires another run of the function 
#' get_locations, which takes time, we give the user the option to switch this 
#' off with check = FALSE assuming that he himself checks the parameter inputs 
#' thouroughly when he does indeed set check = FALSE.
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
#' 
#' 
#' # If you want to download data for several years, you need to do it year by year
#' weatherdata_2018 = get_weatherdata("TAVG", "SW000006", "2018-01-01", "2018-08-31", check = FALSE)
#' weatherdata_2017 = get_weatherdata("TAVG", "SW000006", "2017-01-01", "2017-12-31", check = FALSE)
#' weatherdata_17to18 = rbind(weatherdata_2017, weatherdata_2018)
#' plot(x = as.Date(weatherdata_17to18$date), y = weatherdata_17to18$value/10, 
#'      main = "Avg. Temperature in Linköping, Sweden [2017-2018]", 
#'      xlab = "Date", ylab= "Temperature in C")
#'      
#' }
#' @export
#' 


# ------------------------------------------------------------------------------
# Get weatherdata for specific time period and cities 
# ------------------------------------------------------------------------------

get_weatherdata = function(data_type, city_ID, start_date, end_date, check = TRUE) {

  # ----------------------------------------------------------------------------
  # Stop Conditions
  # ----------------------------------------------------------------------------
  
  # If check = TRUE, download location data to check the input parameters
  # city_ID, the start_date and end_date. 
  
  if (check == TRUE) {
    invisible(capture.output(locations = get_locations()))
    cat("We are checking the availability of data for your city_ID, start_date and end_date...\n")
  }
  
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
  
  # Check if data_type is among the available core or other data types 
  available = c("PRCP", "SNOW", "SNWD", "TMAX", "TMIN")
  
  # Check if data_type is among any of the other data types
  # Reference (Sept. 2018): https://docs.opendata.aws/noaa-ghcn-pds/readme.html
  other = c("ACMC", "ACMH", "ACSC", "ACSH", "AWDR", "AWND", "DAEV", "DAPR", 
            "DASF", "DATN", "DATX", "DAWM", "DWPR", "EVAP", "FMTM", "FRGB", 
            "FRGT", "FRTH", "GAHT", "MDEV", "MDPR", "MDSF", "MDTN", "MDTX", 
            "MDWM", "MNPN", "MXPN", "PGTM", "PSUN", "TAVG", "THIC", "TOBS", 
            "TSUN", "WDF1", "WDF2", "WDF5", "WDFG", "WDFI", "WDFM", "WDMV", 
            "WESD", "WESF", "WSF1", "WSF2", "WSF5", "WSFG", "WSFI", "WSFM")
  
  stopifnot(data_type %in% available || data_type %in% other)
  
  # city_ID --------------------------------------------------------------------
  
  # Check if parameter is of class character
  stopifnot(class(city_ID) == "character")
  
  # If check = TRUE, check if city ID is among locations
  if (check == TRUE) stopifnot(city_ID %in% locations$id)
    
  # start_date -----------------------------------------------------------------
  
  # Check if parameter is of class character
  stopifnot(class(start_date) == "character")
  
  # If check = TRUE, check if start_date is not before mindate
  if (check == TRUE) {
    mindate_chr = locations[which(locations$id == city_ID), "mindate"]
    mindate_date = as.Date(mindate_chr)
    start_date_date = as.Date(start_date)
    
    stopifnot(mindate_date <= start_date_date)
  }

  # end_date -------------------------------------------------------------------
  
  # Check if parameter is of class character
  stopifnot(class(end_date) == "character")
  
  # If check = TRUE, check if end_date is not after mindate
  if (check == TRUE) {
    maxdate_chr = locations[which(locations$id == city_ID), "maxdate"]
    maxdate_date = as.Date(maxdate_chr)
    end_date_date = as.Date(end_date)
    
    stopifnot(end_date_date <= maxdate_date)
  }

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
  
  # Return weatherdata
  return(data)
  
}



