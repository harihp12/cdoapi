#' @title Function \code{my_app}
#' @description Runs the Shiny app.
#' @import shiny
#' @export

# Starting the data processing.
# Setting up the token.
Sys.setenv("NOAA_TOKEN" = "eqtRUMJbywsmAOOgzEJyrfcUAmHVwxPA")

# Getting the cities data.
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
                
                  
  mainPanel("ASD"))

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
}

my_app = function(){
  shinyApp(ui=ui(), server=server)
}

my_app()
