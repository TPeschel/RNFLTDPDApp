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
	function( output, rv, height ) {
		
		output$heidelPlot <-
			renderPlot( {
				
				validate(
					need( ! is.null( rv$visitor ), "no visitor available" ),
					need( ! is.null( rv$age ) & rv$age > 10, "age to low" )
				)
				
				difference.colorplot( rv$visitor$os - rv$visitor$od, rv$age, .001, params.rdiff = parameters$rnflt_diff_params, params.absrdiff = parameters$sector_abs_rnflt_diff_params )
			} )
	
		output$UIheidelPlot <- renderUI( plotOutput( "heidelPlot", height = height ) )
	}

rnfltPlot2D <- 
	function( output, rv, height ) {
		output$rnfltPlot2D <-
			renderPlotly( {
				
				validate(
					need( ! is.null( rv$visitor ), "no visitor available" )
				)
				
				plot_ly( rv$visitor, x = ~ angle, y = ~ od, type = "scatter", name = "od", mode = "lines", line = list( color = "#000000" ), fill = "None", fillcolor = "#ff4040" ) %>%
					add_trace( y = ~ os, name = "os", mode = "lines", line = list( color = "#808080" ), fill = "tonexty", fillcolor = "#ff4040" ) %>%
					layout( title = "RNFLT-Plots", yaxis = list( title = "RNFLT [µm]" ) )
			} )
		
		output$UIrnfltPlot2D <- renderUI( plotlyOutput( "rnfltPlot2D", height = height ) %>% withSpinner( ) )
	}

rnfltdPlot2D <- 
	function( output, rv, height ) {
		output$rnfltdPlot2D <-
			renderPlotly( {
				
				validate(
					need( ! is.null( rv$visitor ), "no visitor available" )
				)
				
				plot_ly( rv$visitor, x = ~ angle, y = ~ abs( os - od ), name = "| os - od |", mode = "lines", opacity = .5, fill = "tozeroy", fillcolor = 'rgba( 255, 255, 127, 0.5)', line = list( color = "#FFFF7F" ) ) %>%
					add_trace( y = ~ ( os - od ), name = "os - od", mode = "lines", opacity = .5, fill = "tozeroy", fillcolor = 'rgba( 127, 127, 255, 0.5)', line = list( color = "#000000", width = 5 ) ) %>%
					layout( title = "RNFLTD-Plots", yaxis = list( title = "RNFLTD [µm]" ) )
			} )
		
		output$UIrnfltdPlot2D <- renderUI( plotlyOutput( "rnfltdPlot2D", height = height ) %>% withSpinner( ) )
	}

min.plot.height <- 50
y.offset <- 150

server <-
	function( input, output, session ) {
		
		screen.height <- function( ) {
			
			print( input$resolution )
			
			i <-
				ifelse(
					is.null( input$resolution ) || input$resolution$height < y.offset + min.plot.height,
					min.plot.height,
					as.integer( input$resolution$height )
				)
			
			print( i )
			
			i
		}
		
		###
		# delete temporary pdf on close
		###
		onStop( function( ) { if( file.exists( "tmp/tmp.pdf" ) ) file.remove( "tmp/tmp.pdf" ) } )
		
		rv <- reactiveValues( )
		
		rv$visitor <- NULL
		
		rv$sex     <- "unknown"
		rv$birth   <- Sys.Date( )
		rv$exam    <- Sys.Date( ) + 20
		rv$age     <- 20
		
		output$pdfview <- renderUI( {
			tags$iframe( style = "height:500px; width:100%", src = "tmp/upload-pdf.pdf" )
		} )
		
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
								tags$iframe( style = paste0( "height:", screen.height( ) - y.offset - 20, "px; width:100%" ), src = "tmp/tmp.pdf" )
							} )
						
						incProgress( 1, "Analyse pdf...", detail = "extract plot" )
						
						rv$visitor <- xtrct.plot.from.pdf( "tmp/tmp.pdf" )

						incProgress( 1, "Analyse pdf...", detail = "extract dates" )
						
						d <- xtrct.text.from.pdf( "tmp/tmp.pdf" )
						
						output$idText <- renderText( paste0( "Pat ID: ", rv$id <- d [[ "id" ]] ) )
						
						output$birthText <- renderText( paste0( "Birth: ", rv$birth <- d [[ "birth" ]] ) )

						output$examText  <- renderText( paste0( "Exam: ", rv$exam  <- d [[ "exam" ]] ) )
						
						output$ageText <- renderText( paste0(   "Age: ", rv$age <- d [[ "age" ]] ) )
						
						output$sexText <- renderText( paste0(   "Sex: ", rv$sex <- d [[ "sex" ]] ) )
						
						print( paste0( "visitor/visitor_", d [[ "id" ]], "_", d [[ "exam" ]], ".csv" ) )
						
						write.csv2( rv$visitor, file = paste0( "visitor/visitor_", d [[ "id" ]], "_", d [[ "exam" ]], ".csv" ) )
						
						incProgress( 1, "Analyse pdf...", detail = "finished" )
						
					},
					min = 0, max = 5, value = 0, message = "Load And Analyse PDF"
				)
			}
		)
		
		observeEvent(
			eventExpr = c( rv$visitor, rv$age, input$resolution$height ),
			handlerExpr = {
				
				withProgress(
					{
						incProgress( 1, "Render plots...", detail = "RNDLT2D" )
						
						rnfltPlot2D( output, rv, screen.height( ) - y.offset )
						
						incProgress( 1, "Render plots...", detail = "RNDFLTD2D" )
						
						rnfltdPlot2D( output, rv, screen.height( ) - y.offset )
						
						incProgress( 1, "Render plots...", detail = "Heidelberg" )
						
						heidelPlot( output, rv, screen.height( ) - y.offset )
						
						incProgress( 1, "Finish...", detail = "...ed" )
					},
					min = 0, max = 4, value = 0, message = "Render plots..."
				)
			}
		)
		
		# output$rnfltPlot2D <- renderUI( {
		# 	plotlyOutput( "rnfltPlot2D", height = '700px' ) %>% withSpinner( )
		# } )
	} 
