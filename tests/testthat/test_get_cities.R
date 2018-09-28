context("get_cities")

test_that("NOAA_TOKEN is correctly checked by get_cities", {
  
  # Test that numeric token causes error
  Sys.setenv("NOAA_TOKEN" = 1) 
  expect_error(get_cities())
  
  # Test that wrong character token causes error
  Sys.setenv("NOAA_TOKEN" = "XBAVDSF19")
  expect_error(get_cities())
  
})

test_that("get_cities works", {
  
  # Preparation ----------------------------------------------------------------
  
  # Get cities data from NOAA
  Sys.setenv("NOAA_TOKEN" = "cfcoWaQRykPCelOlZitLALPtKQuhBXnR")
  cities = get_cities()
  
  # Columns --------------------------------------------------------------------
  
  # Test that colnames match the expected colnames
  correct_names = c("mindate", "maxdate", "datacoverage", "id", "country", "city")
  expect_equal(correct_names, colnames(cities))
  
  # Test that classes match expected classes
  expect_equal(class(cities$mindate), "character")
  expect_equal(class(cities$maxdate), "character")
  expect_equal(class(cities$datacoverage), "numeric")
  expect_equal(class(cities$id), "character")
  expect_equal(class(cities$country), "character")
  
  # Rows -----------------------------------------------------------------------
  
  # Test that number of rows is >= 1987
  expect_true(nrow(cities) >= 1987)
  
  # Specific Cases -------------------------------------------------------------
  
  # Test that city IDs used in get_weatherdata examples match city names
  linkoeping = cities[which(cities$id == "SW000006"), "city"]
  berlin = cities[which(cities$id == "GM000001"), "city"]
  
  expect_equal(linkoeping, "Linkoping")
  expect_equal(berlin, "Berlin")
  
  # Test that length of id is always 8 
  expect_equal(max(nchar(cities$id)), 8)
  expect_equal(min(nchar(cities$id)), 8)
  
})

