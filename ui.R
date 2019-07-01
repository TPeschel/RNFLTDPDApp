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
			title = "RNFLT",
			titleWidth = '97.5%'
		),
		
		sidebar = dashboardSidebar(
			#width = '20%',
			sidebarMenu(
				menuItem(
					text = "VISITOR",
					# menuSubItem(
					# 	text = "upload visitor pdf",
					# 	tabName = "TAB_UPLOAD_VISITOR_PDF"
					# ),
					menuSubItem(
						text = "VIEW PDF",
						tabName = "TAB_VIEW_VISITOR_PDF"
					),
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
			),
			hr( ),
			box(
				width = 12,
				title = "UPLOAD A PDF",
				collapsible = T,
				collapsed = F,
				background = "black",
				fileInput( 'file_input', 'upload file ( . pdf format only)', accept = c( '.pdf' ) )
			),
			hr( ),
			box(
				width = 12,
				title = "Visitor's Data",
				collapsible = T,
				collapsed = F,
				background = "black",
				dateInput( "birthDate", label = "Birth Date: " ),
				dateInput( "examDate",  label = "Exam Date:  ", value = Sys.Date( ) ),
				radioButtons( "sex",    label = "Sex:        ", choiceNames = c( "unknown", "male", "female" ), choiceValues = c( "unknown", "male", "female" ), selected = "unknown" ),
				hr( ),
				h4( textOutput( "ageText" ) ),
				hr( )
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
					tabName = "TAB_PLOT_RNFLT_2D",
					box(
						width = 12,
						height = "800px",
						title = "RNFLT-PLOT",
						collapsible = T,
						plotlyOutput( "rnfltPlot2D", height = '700px' ) %>% withSpinner( )
					)
				),
				tabItem(
					tabName = "TAB_PLOT_RNFLTD_2D",
					box(
						width = 12,
						height = "800px",
						title = "RNFLTD-PLOT",
						collapsible = T,
						plotlyOutput( "rnfltdPlot2D", height = '700px' ) %>% withSpinner( )
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
