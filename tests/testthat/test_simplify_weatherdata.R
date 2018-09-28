context("simplify_weatherdata")

test_that("weatherdata is correctly checked by simplify_weatherdata", {
  
  # Set sample token as global variable for all tests below 
  Sys.setenv("NOAA_TOKEN" = "cfcoWaQRykPCelOlZitLALPtKQuhBXnR")
  
  # Get weatherdata for tests below
  data = get_weatherdata("TAVG", "SW000006", "2018-09-01", "2018-09-23", check = FALSE)
  
  # Test that character matrix input causes errors
  expect_error(simplify_weatherdata(as.matrix(data[, -ncol(data)])))
  
  # Test that data frame with value column of character class causes errors
  data_sub = data
  data_sub$value = as.character(data_sub$value)
  expect_error(simplify_weatherdata(data_sub))
  
  # Test that data frame with station column of factor class causes errors
  data_sub = data
  data_sub$station = as.factor(data_sub$station)
  expect_error(simplify_weatherdata(data_sub))
  
  # Test that data frame with date column of Date class causes errors
  data_sub = data
  data_sub$date = as.Date(data_sub$date)
  expect_error(simplify_weatherdata(data_sub))
  
  # Test that data frame with datatype column of factor class causes errors
  data_sub = data
  data_sub$datatype = as.factor(data_sub$datatype)
  expect_error(simplify_weatherdata(data_sub))
  
})


test_that("simplify_weatherdata returns correct output", {

  # Set sample token as global variable for all tests below 
  Sys.setenv("NOAA_TOKEN" = "cfcoWaQRykPCelOlZitLALPtKQuhBXnR")
  
  # Get weatherdata for tests below
  data = get_weatherdata("TAVG", "SW000006", "2018-09-01", "2018-09-23", check = FALSE)
  
  # Create simplify_weatherdata ouput for tests below
  data_simp = simplify_weatherdata(data)
  
  # Test that number of columns is 2
  expect_equal(ncol(data_simp), 2)
  
  # Test that number of rows equals number of unique dates in original weatherdata
  expect_equal(nrow(data_simp), length(unique(data[, "date"])))
  
  # Test that first column is "date" column
  expect_equal(colnames(data_simp)[1], "date")
  
  # Test that name of second column corresponds to original weatherdata
  expect_equal(colnames(data_simp)[2], unique(data[, "datatype"]))
  
  # Test that second column is numeric
  expect_equal(class(data_simp[, 2]), "numeric")
  
})
