context("get_weatherdata")

test_that("NOAA_TOKEN is correctly checked by get_weatherdata", {
  
  # Test that numeric token causes error
  Sys.setenv("NOAA_TOKEN" = 1) 
  expect_error(get_weatherdata("TAVG", "SW000006", "2018-09-01", "2018-09-23", check = FALSE))
  
  # Test that wrong character token causes error
  Sys.setenv("NOAA_TOKEN" = "XBAVDSF19")
  expect_error(get_weatherdata("TAVG", "SW000006", "2018-09-01", "2018-09-23", check = FALSE))
  
})

test_that("data_type is correctly checked by get_weatherdata", {
  
  # Test case with data_type = 1
  expect_error(get_weatherdata(1, "SW000006", "2018-09-01", "2018-09-23", check = FALSE)) 
  
  # Test case with data_type = "WRONG"
  expect_error(get_weatherdata("WRONG", "SW000006", "2018-09-01", "2018-09-23", check = FALSE)) 
  
})

test_that("city_ID is correctly checked by get_weatherdata", {
  
  # Test case with city_ID = 1
  expect_error(get_weatherdata("TAVG", 1 , "2018-09-01", "2018-09-23", check = FALSE)) 

  # Test case with length of 9 instead of 8
  expect_error(get_weatherdata("TAVG", "SW0000066", "2018-09-01", "2018-09-23", check = FALSE))
  
})

test_that("start_date is correctly checked by get_weatherdata", {
  
  # Test case with start_date = 1
  expect_error(get_weatherdata("TAVG", "SW000006", 1, "2018-09-23", check = FALSE)) 
  
  # Test case with start_date of length 11 instead of 10
  expect_error(get_weatherdata("TAVG", "SW000006", "2018-09-011", "2018-09-23", check = FALSE))
  
})


test_that("end_date is correctly checked by get_weatherdata", {
  
  # Test case with end_date = 1
  expect_error(get_weatherdata("TAVG", "SW000006", "2018-09-01", 1, check = FALSE)) 
  
  # Test case with end_date of length 11 instead of 10
  expect_error(get_weatherdata("TAVG", "SW000006", "2018-09-01", "2018-09-235", check = FALSE))
  
})


test_that("get_weatherdata works", {
  
  # Set sample token as global variable for all tests below 
  Sys.setenv("NOAA_TOKEN" = "cfcoWaQRykPCelOlZitLALPtKQuhBXnR")
  
  # Get weatherdata for tests below
  data = get_weatherdata("TAVG", "SW000006", "2018-09-01", "2018-09-23", check = FALSE)
  
  correct_names = c("date", "datatype", "station", "attributes", "value")
    
  # Test that number of observations is 23
  expect_equal(nrow(data), 23)
  
  # Test that colnames match with correct colnames
  expect_equal(colnames(data), correct_names)
  
  # Test that column classes match with correct classes
  expect_equal(class(data$date), "character")
  expect_equal(class(data$datatype), "character")
  expect_equal(class(data$station), "character")
  expect_true(is.numeric(data$value))
  
  # Test that all values in the datatype column are equal
  expect_equal(length(unique(data$datatype)), 1)
  
})

