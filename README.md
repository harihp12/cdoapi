# cdoapi
R Interface for the NOAA Climate Data Online API

[![Build Status](https://travis-ci.com/harihp12/cdoapi.svg?token=zu39qmCz9tp7Bhzm3kEA&branch=master)](https://travis-ci.com/harihp12/cdoapi)
[![codecov](https://codecov.io/gh/harihp12/cdoapi/branch/master/graph/badge.svg?token=qSGva0cJQs)](https://codecov.io/gh/harihp12/cdoapi)

This package provides an R interface for the NOAA Climate Data Online API. Through this interface, different elements of weather data like temperatures (avg/min/max), precipitation, snowfall, etc. can be accessed for various cities for specific time periods.

## Installation

```R
devtools::install_github("harihp12/cdoapi")
```

## Authentication

To use this interface, you require a unique API access token. You can get your access token [here](https://www.ncdc.noaa.gov/cdo-web/token) by registering with your email address. Before using the functions provided by the `cdoapi` package, you should set the character token as a system variable.

```R
Sys.setenv("NOAA_TOKEN" = "YOUR_TOKEN_GOES_IN_HERE")
```
## Features

### get_cities
Lists all the available cities for which the weather data can be fetched with `get_weatherdata` along with their data availability details.

```R
cities = get_cities()
head(cities)

head(cities[which(cities$country == "SW"), ]) # Cities in Sweden
head(cities[which(cities$country == "GM"), ]) # Cities in Germany
```

### get_weatherdata
Gets the weather data for a specified - data type (Refer [here](https://docs.opendata.aws/noaa-ghcn-pds/readme.html#element-summary) for detailed documentation about available data types), city ID, start date and end date.

```R
# Get weatherdata for Berlin (only September)
weatherdata = get_weatherdata("TAVG", "GM000001", "2018-09-01", "2018-09-23")
```

### simplify_weatherdata
Aggregates and simplifies weather data to an easy-to-use daily data format, which is particularly useful for cities having multiple stations.

```R
weatherdata_simplified = simplify_weatherdata(weatherdata)
```

## Shiny App
To load the shiny app you need to have the library installed and call the following commands on Rstudio:
```R
library(cdoapi)
library(shiny)

runGitHub("harihp12/cdoapi")
```