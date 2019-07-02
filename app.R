rm( list = ls( ) )

# neccessary.pkgs <- c( "installr", "DT", "markdown", "plotrix", "plotly", "rhandsontable", "shiny", "shinycssloaders", "shinydashboard", "stringr", "dplyr", "pdf", "gamlss" )
# 
# installed.pkgs <- rownames( installed.packages( ) )
# 
# pkgs.2.install <- setdiff( neccessary.pkgs, installed.pkgs )
# 
# if( 0 < length( pkgs.2.install ) ) 
# 	install.packages( pkgs.2.install, repos="https://cran.uni-muenster.de/" )

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
	
