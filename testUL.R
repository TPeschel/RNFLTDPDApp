rm( list = ls( ) )

library( "shiny" )
#library( "shinydashboard" )
library( "plotly" )
source( "pdf.R" )
source( "tobias.R" )
#addResourcePath( "tmp", paste0( getwd( ), "/tmp" ) )

ui <-
	fluidPage(
	
		title = "PDF - Upload",
		
		hr( ),
		
		fluidRow(
	
			box(
				width = 2,
				fileInput( 'file_input', 'upload file ( . pdf format only)', accept = c( '.pdf' ) )
			),
			
			box(
				width = 4,
				uiOutput( "pdfview" )
			),
			
			box( 
				width = 6,
				plotlyOutput( "visPlot" ),
				plotOutput( "heidelPlot" )
			)
		)
	)

server <- 
	shinyServer(
		function( input, output ) {
			
			rv <- reactiveValues( )
			
			rv$visitor <- NULL
			
			output$pdfview <- renderUI( {
				tags$iframe(style="height:900px; width:100%", src="tmp/upload-pdf.pdf" )
			} )

			observe( {
				
				req( input$file_input )
				
				file.copy( input$file_input$datapath, "tmp/tmp.pdf", overwrite = T )
				
				output$pdfview <-
					renderUI( {
						tags$iframe(style="height:900px; width:100%", src="tmp/tmp.pdf" )
					} )
				
				rv$visitor <- xtrct.plot.from.pdf( "tmp/tmp.pdf" )
				
				output$visPlot <-
					renderPlotly( {
						
						#colors <- c( "gray", "black", "blue", "red" )
						
						plot_ly( rv$visitor, x = ~ angle, y = ~ od, type = "scatter", name = "od", mode = "lines", line = list( color = "#000000" ) ) %>%
							add_trace( y = ~ os, name = "os", mode = "lines", line = list( color = "#808080" ) ) %>%
							add_trace( y = ~ abs( os - od ), name = "| os - od |", mode = "lines", opacity = .5, fill = "tozeroy", fillcolor = 'rgba( 255, 255, 127, 0.5)', line = list( color = "#FFFF7F" ) ) %>%
							add_trace( y = ~ ( os - od ), name = "os - od", mode = "lines", opacity = .5, fill = "tozeroy", fillcolor = 'rgba( 127, 127, 255, 0.5)', line = list( color = "#000000", width = 5 ) ) %>%
							layout( title = "RNFLT-Plots", yaxis = list( title = "RNFLT [µm]" ) )
					} )
		
				output$heidelPlot <-
					renderPlot( {
						
						difference.colorplot( rv$visitor$os - rv$visitor$od, 55, .001, params.rdiff = parameters$rnflt_diff_params, params.absrdiff = parameters$sector_abs_rnflt_diff_params )
					} )
				
			} )
		} )

shinyApp(ui = ui, server = server)

