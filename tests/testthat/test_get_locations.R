context("cdoapi_get_locations")

test_that("lenreg rejects errounous input", {
  Sys.setenv("NOAA_TOKEN" = 1)
  expect_error(get_locations())
})