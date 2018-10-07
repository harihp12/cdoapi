# Function for getting
# the last month of a specific date.
get_month = function(date){
  # Date should be a string
  # of the format: "y%-m%-d%".
  # Getting the month.
  month = as.numeric(substr(date, 6, 7))
  
  #  If it's January, the month needs to be changed
  # to December and a year needs to be substracted.
  if (month == 1){
    # From January to December.
    month = 12
    
    # Going to the last year.
    year = as.numeric(substr(date, 1, 4))
    year = as.character(year - 1)
    substr(date, 1, 4) = year
    
  } else {
    # Otherwise substract the a month.
    month = month - 1
  }
  
  # Getting the month as a character
  month = as.character(month)
  
  # Verifying it's well formated and if
  # not, correct it.
  if (nchar(month) != 2){
    month = paste("0", month, sep="")
  }
  
  substr(date, 6, 7) = month
  
  # Return modified date.
  return(date)
}


# Starting the data processing.
# Setting up the token.
Sys.setenv("NOAA_TOKEN" = "RCxwJXwZHAFJRIzCPdfLhOshjnRycvio")

# Getting the cities data.
cat("getting cities bla bla blla")
cities = get_cities()

# Needs to be a function so nested dependencies are lazily loaded.
# My ui.
ui = function(){fluidPage(
  # Title of the shiny app.
  titlePanel("CDO Visualization App"),
  
  sidebarLayout(position="left",
                sidebarPanel(
                  selectInput(inputId="country",
                              label="Select Country Code",
                              choices=unique(cities$country)),
                  uiOutput("drop_down"),
                  p("Get the country codes", 
                    a("here", href="https://docs.opendata.aws/noaa-ghcn-pds/readme.html#lookup-table-of-country-codes"))),
                
                  
  mainPanel(plotlyOutput("weather_plot")))

)}

# My server.
server = function(input, output){

  output$drop_down = renderUI(
    if (is.null(input$country)){
      return()
    }
    else{
      selectInput(inputId="city",
                  label="Select a city",
                  choices=cities$city[cities$country == input$country])
    }
  )
  
  # Filtering to get the city info.
  output$weather_plot = renderPlotly(
    if (is.null(input$city)){
      p = NULL
      return(p)
    } else {
      city_info = cities[cities$city == input$city, ]
      weatherdata = get_weatherdata("TAVG",
                                    city_info$id,
                                    get_month(city_info$maxdate),
                                    city_info$maxdate,
                                    check=FALSE)
      weatherdata_simplified = simplify_weatherdata(weatherdata)
      
      p = plot_ly(type="scatter", mode="lines") %>%
        add_trace(x=weatherdata_simplified$date,
                  y=weatherdata_simplified$TAVG,
                  mode="lines+markers",
                  name=paste("Last month average temperature of:", input$city))
      
      return(p)
    }
  )
  
  
  
}
#' @import shiny
#' @import plotly
#' @import ggplot2
my_app = function(){
  shinyApp(ui=ui(), server=server)
}
