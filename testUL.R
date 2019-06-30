rm( list = ls( ) )

library( "shiny" )
library( "shinydashboard" )
library( "shinycssloaders" )
library( "plotly" )

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

visPlot <- 
	function( output, rv ) {
		output$visPlot <-
			renderPlotly( {
				
				validate(
					need( ! is.null( rv$visitor ), "no visitor available" )
				)
				
				plot_ly( rv$visitor, x = ~ angle, y = ~ od, type = "scatter", name = "od", mode = "lines", line = list( color = "#000000" ) ) %>%
					add_trace( y = ~ os, name = "os", mode = "lines", line = list( color = "#808080" ) ) %>%
					add_trace( y = ~ abs( os - od ), name = "| os - od |", mode = "lines", opacity = .5, fill = "tozeroy", fillcolor = 'rgba( 255, 255, 127, 0.5)', line = list( color = "#FFFF7F" ) ) %>%
					add_trace( y = ~ ( os - od ), name = "os - od", mode = "lines", opacity = .5, fill = "tozeroy", fillcolor = 'rgba( 127, 127, 255, 0.5)', line = list( color = "#000000", width = 5 ) ) %>%
					layout( title = "RNFLT-Plots", yaxis = list( title = "RNFLT [µm]" ) )
			} )
	}

ui1 <-
	dashboardPage(
		
		title = "RNFLT - App" ,
		
		skin = "black",

		header = dashboardHeader(
			title = "RNFLT",
			titleWidth = '15%'
		),
		
		sidebar = dashboardSidebar(
			#width = '15%',
			sidebarMenu(
				menuItem(
					text = "VISITOR",
					# menuSubItem(
					# 	text = "upload visitor pdf",
					# 	tabName = "TAB_UPLOAD_VISITOR_PDF"
					# ),
					menuSubItem(
						text = "view visitor pdf",
						tabName = "TAB_VIEW_VISITOR_PDF"
					),
					menuSubItem(
						text = "plot visitor plotly 2d",
						tabName = "TAB_PLOT_VISITOR_PLOTLY_2D"
					),
					menuSubItem(
						text = "plot visitor heidelberg",
						tabName = "TAB_PLOT_VISITOR_HEIDELBERG"
					)
				)
			),
			hr( ),
			box(
				width = 12,
				title = "UPLOAD A PDF",
				collapsible = T,
				collapsed = F,
				background = "black",
				fileInput( 'file_input', 'upload file ( . pdf format only)', accept = c( '.pdf' ) ),
				hr( ),
				dateInput( "dob",      label = "DOB" ),
				dateInput( "examDate", label = "Exam Date", value = Sys.Date( ) ),
				hr( ),
				textOutput( "ageText" )
			)
		),
		
		body = dashboardBody(
			tabItems(
				# tabItem(
				# 	tabName = "TAB_UPLOAD_VISITOR_PDF"
				# ),
				tabItem(
					tabName = "TAB_VIEW_VISITOR_PDF",
					box(
						title = "VIEW PDF",
						height = "800px",
						collapsible = T,
						collapsed = F,
						uiOutput( "pdfview" ) %>% withSpinner( )
					)
				),
				tabItem(
					tabName = "TAB_PLOT_VISITOR_PLOTLY_2D",
					box(
						width = 12,
						height = "800px",
						title = "PLOTLY-PLOT",
						collapsible = T,
						plotlyOutput( "visPlot", height = '700px' ) %>% withSpinner( )
					)
				),
				tabItem(
					tabName = "TAB_PLOT_VISITOR_HEIDELBERG",
					box( 
						width = 12,
						height = "800px",
						title = "HEIDELBERG-STYLE-PLOT",
						collapsible = T,
						plotOutput( "heidelPlot", height = '700px' ) %>% withSpinner( )
					)
				)
			)
		)
	)

s1 <-
	function( input, output, session ) {
		
		print( "was here" )
		
		rv <- reactiveValues( )

		rv$visitor <- NULL

		output$pdfview <- renderUI( {
			tags$iframe( style = "height:700px; width:100%", src = "tmp/upload-pdf.pdf" )
		} )

		observeEvent(
			eventExpr = {

				c( input$examDate, input$dob )
			},
			handlerExpr = {

				rv$age <- as.double( round( ( input$examDate - input$dob ) / 365.25, 1 ) )

				print( paste0( "Age: ", rv$age, " years" ) )

				output$ageText <-
					renderText(
						expr = {
							paste0( "Age: ", rv$age, " years" )
						}
					)
			}
		)

		# observeEvent(
		# 	eventExpr = rv$age,
		# 	handlerExpr = {
		#
		# 	} )

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

						incProgress( 1, "Analyse pdf...", detail = "from copy" )

						rv$visitor <- xtrct.plot.from.pdf( "tmp/tmp.pdf" )

						incProgress( 1, "Analyse pdf...", detail = "finished" )

					},
					min = 0, max = 4, value = 0, message = "Load And Analyse PFD"
				)
			}
		)

		observeEvent(
			eventExpr = c( rv$visitor, rv$age ),
			handlerExpr = {

				withProgress(
					{
						incProgress( 1, "Render plots...", detail = "Plotly" )

						visPlot( output, rv )

						incProgress( 1, "Render plots...", detail = "Heidelplot" )

						heidelPlot( output, rv )

						incProgress( 1, "Finish...", detail = "...ed" )
					},
					min = 0, max = 3, value = 0, message = "Render plots..."
				)
			}
		)
	} 

shinyApp(
	ui     = ui1, 
	server = s1
)


# shinyApp(
# 	ui = dashboardPage(
# 		header = dashboardHeader(
# 			title = "TITLE"
# 		),
# 		sidebar = dashboardSidebar(
# 
# 			sidebarMenu(
# 				menuItem(
# 					text = "A",
# 					tabName = "TAB_A"
# 				),
# 				menuItem(
# 					text = "B",
# 					tabName = "TAB_B",
# 					menuSubItem(
# 						text = "B.A",
# 						tabName = "TAB_B_A"
# 					),
# 					menuSubItem(
# 						text = "B.B",
# 						tabName = "TAB_B_B"
# 					)
# 				)
# 			)
# 		),
# 		body = dashboardBody(
# 			tabItems(
# 				tabItem(
# 					tabName = "TAB_A",
# 					sliderInput( "SI_A", label = "A", min = 0, max = 100, value = 50 )
# 				),
# 				tabItem(
# 					tabName = "TAB_B_A",
# 					textOutput( "textA" )
# 				),
# 				tabItem(
# 					tabName = "TAB_B_B",
# 					plotOutput( "plotA" )
# 				)
# 			)
# 		)
# 	),
# 	server = function( input, output ) {
# 
# 		observeEvent(
# 			eventExpr = input$SI_A,
# 			handlerExpr = {
# 
# 				output$textA <- renderText( { paste0( "A: ", input$SI_A ) } )
# 				output$plotA <- renderPlot( { plot( 1 : 100, rep( input$SI_A, 100 ) ) } )
# 			}
# 		)
# 	}
# )

