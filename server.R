library( "dplyr" )
library( "DT" )
library( "markdown" )
library( "plotly" )
library( "rhandsontable" )
library( "shiny" )
library( "shinycssloaders" )
library( "shinydashboard" )
library( "stringr" )

source( "pdf.R" )
source( "tobias.R" )

addResourcePath( "tmp", paste0( getwd( ), "/tmp" ) )

heidelPlot <- 
	function( output, rv ) {
		
		output$heidelPlot <-
			renderPlot( {
				
				validate(
					need( ! is.null( rv$visitor ), "no visitor available" ),
					need( ! is.null( rv$age ) & rv$age > 10, "age to low" )
				)
				
				difference.colorplot( rv$visitor$os - rv$visitor$od, rv$age, .001, params.rdiff = parameters$rnflt_diff_params, params.absrdiff = parameters$sector_abs_rnflt_diff_params )
			} )
	}

rnfltPlot2D <- 
	function( output, rv ) {
		output$rnfltPlot2D <-
			renderPlotly( {
				
				validate(
					need( ! is.null( rv$visitor ), "no visitor available" )
				)
				
				plot_ly( rv$visitor, x = ~ angle, y = ~ od, type = "scatter", name = "od", mode = "lines", line = list( color = "#000000" ) ) %>%
					add_trace( y = ~ os, name = "os", mode = "lines", line = list( color = "#808080" ) ) %>%
					layout( title = "RNFLT-Plots", yaxis = list( title = "RNFLT [µm]" ) )
			} )
	}

rnfltdPlot2D <- 
	function( output, rv ) {
		output$rnfltdPlot2D <-
			renderPlotly( {
				
				validate(
					need( ! is.null( rv$visitor ), "no visitor available" )
				)
				
				plot_ly( rv$visitor, x = ~ angle, y = ~ abs( os - od ), name = "| os - od |", mode = "lines", opacity = .5, fill = "tozeroy", fillcolor = 'rgba( 255, 255, 127, 0.5)', line = list( color = "#FFFF7F" ) ) %>%
					add_trace( y = ~ ( os - od ), name = "os - od", mode = "lines", opacity = .5, fill = "tozeroy", fillcolor = 'rgba( 127, 127, 255, 0.5)', line = list( color = "#000000", width = 5 ) ) %>%
					layout( title = "RNFLTD-Plots", yaxis = list( title = "RNFLTD [µm]" ) )
			} )
	}

server <-
	function( input, output, session ) {
		
		###
		# delete temporary pdf on close
		###
		onStop( function( ) { if( file.exists( "tmp/tmp.pdf" ) ) file.remove( "tmp/tmp.pdf" ) } )
		
		rv <- reactiveValues( )
		
		rv$visitor <- NULL
		
		rv$sex     <- "unknown"
		
		output$pdfview <- renderUI( {
			tags$iframe( style = "height:700px; width:100%", src = "tmp/upload-pdf.pdf" )
		} )
		
		observeEvent(
			eventExpr = {
				
				c( input$examDate, input$birthDate )
			},
			handlerExpr = {
				
				rv$age <- as.double( round( ( input$examDate - input$birthDate ) / 365.25, 1 ) )
				
				output$ageText <-
					renderText(
						expr = {
							paste0( "Age: ", rv$age, " years" )
						}
					)
			}
		)
		
		observeEvent(
			eventExpr = input$file_input,
			handlerExpr = {
				
				withProgress(
					{
						incProgress( 1, "Uploading...", detail = "and copying..." )
						
						file.copy( input$file_input$datapath, "tmp/tmp.pdf", overwrite = T )
						
						incProgress( 1, "Render pdf...", detail = "from copy" )
						
						output$pdfview <-
							renderUI( {
								tags$iframe( style =" height:700px; width:100%", src = "tmp/tmp.pdf" )
							} )
						
						incProgress( 1, "Analyse pdf...", detail = "extract plot" )
						
						rv$visitor <- xtrct.plot.from.pdf( "tmp/tmp.pdf" )
						
						incProgress( 1, "Analyse pdf...", detail = "extract dates" )
						
						d <- xtrct.dates.from.pdf( "tmp/tmp.pdf" )
						
						updateDateInput( session, "birthDate", value = d [[ "birth" ]] )
						
						updateDateInput( session, "examDate",  value = d [[ "exam" ]] )
						
						updateRadioButtons( session, "sex",    selected = d [[ "sex" ]] )
						
						output$sexText <- renderText( paste0( "Sex: ", rv$sex <- d [[ "sex" ]] ) )
						
						incProgress( 1, "Analyse pdf...", detail = "finished" )
						
					},
					min = 0, max = 5, value = 0, message = "Load And Analyse PDF"
				)
			}
		)
		
		observeEvent(
			eventExpr = c( rv$visitor, rv$age ),
			handlerExpr = {
				
				withProgress(
					{
						incProgress( 1, "Render plots...", detail = "RNDLT2D" )
						
						rnfltPlot2D( output, rv )
						
						incProgress( 1, "Render plots...", detail = "RNDFLTD2D" )
						
						rnfltdPlot2D( output, rv )
						
						incProgress( 1, "Render plots...", detail = "Heidelberg" )
						
						heidelPlot( output, rv )
						
						incProgress( 1, "Finish...", detail = "...ed" )
					},
					min = 0, max = 4, value = 0, message = "Render plots..."
				)
			}
		)
	} 
