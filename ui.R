library( "dplyr" )
library( "DT" )
library( "markdown" )
library( "plotly" )
library( "rhandsontable" )
library( "shiny" )
library( "shinycssloaders" )
library( "shinydashboard" )
library( "stringr" )

ui <-
	dashboardPage(
		
		title = "RNFLT - App" ,
		
		skin = "black",
		
		header = dashboardHeader(
			title = "RNFLT"#,
			#titleWidth = '97.5%'
		),
		
		sidebar = dashboardSidebar(collapsed = F,
			#width = '20%',
			sidebarMenu(
				menuItem(
					text = "VISITOR",
					tabName = "TAB_VIEW_VISITOR_PDF"
				),
				menuItem(
					text = "PLOTS",
					menuSubItem(
						text = "PLOT RNFLT 2D",
						tabName = "TAB_PLOT_RNFLT_2D"
					),
					menuSubItem(
						text = "PLOT RNFLTD 2D",
						tabName = "TAB_PLOT_RNFLTD_2D"
					),
					menuSubItem(
						text = "PLOT RNFLTD Heidelberg",
						tabName = "TAB_PLOT_VISITOR_HEIDELBERG"
					)
				)
			)
		),
		
		body = dashboardBody(
			shiny::tags$head(
				shiny::tags$script(
					"
					// define a function that returns a struct named resolution
					// containing width and height of the inner window
					// accessible in shiny as a list named input$resolution
					// which elements are
					// width:  input$resolution$width
					// height: input$resolution$height
					function resized( e ) {
						Shiny.onInputChange( 
							'resolution', 
							{ 
								width  : window.innerWidth,
								height : window.innerHeight
							}
						);
					}
					
					// call the function resized when shiny connects
					$( document ).on(
						'shiny:connected',
						resized
					);
					
					// call the function resized when the window resizes
					$( window ).resize(
						resized
					);
					"
				),
				tags$style(
					HTML(
						'.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}'
					)
				)
			),
			tabItems(
				# tabItem(
				# 	tabName = "TAB_UPLOAD_VISITOR_PDF"
				# ),
				tabItem(
					tabName = "TAB_VIEW_VISITOR_PDF",
					fluidRow(
						box(
							width = 4,
							title = "UPLOAD A PDF",
							collapsible = T,
							collapsed = F,
							fileInput( 'file_input', 'upload file ( . pdf format only)', accept = c( '.pdf' ) ),
							fluidRow(
								column( width = 6, h5( textOutput( "idText" ) ) ),
								column( width = 6, h5( textOutput( "sexText" ) ) ) 
							),
							hr( ),
							fluidRow(
								column( width = 6, h5( textOutput( "birthText" ) ) ),
								column( width = 6, h5( textOutput( "examText" ) ) )
							),
							hr( ),
							fluidRow(
								column( width = 12, h5( textOutput( "ageText" ) ) ) 
							)
						),
						box(
							width = 8,
							title = "PDF",
							height = "auto",
							collapsible = T,
							collapsed = F,
							uiOutput( "pdfview" )
						)
					)
				),
				tabItem(
					tabName = "TAB_PLOT_RNFLT_2D",
					box(
						width = 12,
						#height = "800px",
						title = "RNFLT-PLOT",
						collapsible = F,
						uiOutput( "UIrnfltPlot2D" )
					)
				),
				tabItem(
					tabName = "TAB_PLOT_RNFLTD_2D",
					box(
						width = 12,
						#height = "800px",
						title = "RNFLTD-PLOT",
						collapsible = F,
						uiOutput( "UIrnfltdPlot2D" )
					)
				),
				tabItem(
					tabName = "TAB_PLOT_VISITOR_HEIDELBERG",
					box( 
						width = 12,
						#height = "800px",
						title = "HEIDELBERG-STYLE-PLOT",
						collapsible = F,
						uiOutput( "UIheidelPlot" )
					)
				)
			)
		)
	)
