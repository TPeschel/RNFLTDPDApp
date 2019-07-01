rm( list = ls( ) )

neccessary.pkgs <- c( "dk", "DT", "markdown", "plotrix", "plotly", "rhandsontable", "shiny", "shinycssloaders", "shinydashboard", "stringr", "dplyr" )

installed.pkgs <- rownames( installed.packages( ) )

install.packages( setdiff( neccessary.pkgs, installed.pkgs ) )

library( "dplyr" )
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
	
