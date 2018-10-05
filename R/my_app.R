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
  titlePanel("TEST")
)}

# My server.
server = function(input, output){
  
}

my_app = function(){
  shinyApp(ui=ui(), server=server)
}
