rm( list = ls( ) )

library( "DT" )
library( "markdown" )
library( "plotly" )
library( "rhandsontable" )
library( "shiny" )
library( "shinycssloaders" )
library( "shinydashboard" )

source ( "server.R" )
source ( "ui.R" )

# Run the application 
shinyApp( 
	ui     = ui,
	server = server
)
