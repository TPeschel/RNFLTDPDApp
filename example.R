rm( list = ls( ) )

source( "sources.R" ) # load the functions

library(shiny)

ui <-
	fluidPage(
		fluidRow(
			column(
				6,
				sliderInput( "ID_SI_AGE", "Age[y]", 20, 80, 50, animate = T )
			),
			column(
				6,
				sliderInput( "ID_SI_RAD", "Rad[mm]", -.25, .25, 0, .01, animate = T )
			)
		),
		fluidRow(
			column(
				12,
				plotOutput( "ID_PLOT", height = "800" )
			)
		)
	)

server <-
	function( input, output, session ) {
		
		output$ID_PLOT <-
			renderPlot(
				difference.colorplot( rnfltdiff, input$ID_SI_AGE, input$ID_SI_RAD )
				#difference.colorplot( rnfltdiff, input$ID_SI_AGE, 0 )
			)
	}

shinyApp( ui, server )
