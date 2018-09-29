# cdoapi
R Interface for the NOAA Climate Data Online API

[![Build Status](https://travis-ci.com/harihp12/cdoapi.svg?token=zu39qmCz9tp7Bhzm3kEA&branch=master)](https://travis-ci.com/harihp12/cdoapi)
[![Coverage status](https://codecov.io/gh/harihp12/cdoapi/branch/master/graph/badge.svg)](https://codecov.io/github/harihp12/cdoapi?branch=master)

This package creates an R interface for the NOAA Climate Data Online API. Through this interface, weather data can be accessed for various cities for specific time periods.

## Usage

To use this interface, you require a unique API access token. You can get your access token \href{https://www.ncdc.noaa.gov/cdo-web/token}{here} by registering with your email address. Before using the functions provided by the \code{cdoapi} package, you should set the character token as a system variable.

```R
Sys.setenv("NOAA_TOKEN" = "YOUR_TOKEN_GOES_IN_HERE")
```
## Features

### get_cities
Gets the list of cities for which weather data is available.

### get_weatherdata
Gets the weather data for a specified city and specified time period.

### simplify_weatherdata
Aggregates and simplifies weather data to an easy-to-use daily data format.

## How to install?
```R
devtools::install_github("harihp12/cdoapi")
```
