rm( list = ls( ) )

# install.packages( c( "DT", "markdown", "plotrix", "plotly", "rhandsontable", "shiny", "shinycssloaders", "shinydashboard" ) )

library( "DT" )
library( "markdown" )
library( "plotly" )
library( "rhandsontable" )
library( "shiny" )
library( "shinycssloaders" )
library( "shinydashboard" )
library( "stringr" )

source ( "server.R" )
source ( "ui.R" )
# source( "testUL.R" )
# Run the application 
shinyApp( 
	ui     = ui,
	server = server
)
	
