library( "shiny" )
library( "shinycssloaders" )
library( "shinydashboard" )
library( "rhandsontable" )
library( "plotly" )

vip.card <-
	function( name, task, institute.link = NULL, tel, email ) {
		if( is.null( institute.link ) || nchar( institute.link ) < 1 ) {
			
			box(
				width = 6,
				#offset = 2,
				align='center',
				tags$div(
					tags$h3(
						name
					),
					task,
					tags$h5(
						"email: ",
						tags$a(
							email,
							href = paste0( "mailto:", email )
						)
					),
					tags$h5(
						tags$span(
							paste0( "phone: ", tel )
						)
					)
				)
			)
		}
		else {
			
			box(
				width = 6,
				#offset = 2,
				align='center',
				tags$div(
					tags$h3(
						tags$a(
							name,
							href = institute.link,
							target = "_blank",
							rel = "noopener"
						)
					),
					task,
					tags$h5(
						"email: ",
						tags$a(
							email,
							href = paste0( "mailto:", email )
						)
					),
					tags$h5(
						tags$span(
							paste0( "phone: ", tel )
						)
					)
				)
			)
		}
	}

ui <-
	dashboardPage(
		
		title = "RNFLTDPD-App",
		
		skin = "black",
		
		header = 
			dashboardHeader(
				
				title = "Retinal Nerve Fiber Layer Thickness Difference Percentiles Demonstrator App",
				titleWidth = '97.5%'
			),
		
		sidebar = 
			dashboardSidebar(
				sidebarMenu(
					menuItem(
						text = "Welcome",
						tabName = "TAB_WELCOME"
					),
					menuItem(
						text = "Visitor",
						tabName = "TAB_VISITOR"
					),
					menuItem(
						text = "RNFLTDP PLOTS",
						menuSubItem(
							text = "HEIDELBERG",
							tabName = "TAB_RNFLTD_2D_HEIDELBERG"
						),
						menuSubItem(
							text = "2D over angle",
							tabName = "TAB_RNFLTD_2D"
						),
						menuSubItem(
							text = "3D over age and angle",
							tabName = "TAB_RNFLTD_3D_AA"
						),
						menuSubItem(
							text = "3D over rad diff and angle",
							tabName = "TAB_RNFLTD_3D_RDA"
						)
					),
					# menuItem(
					# 	text = "Tables Percentiles",
					# 	tabName = "TAB_TABLE_PERCECNTILES"
					# ),
					menuItem(
						text = "Methods",
						tabName = "TAB_METHODS"
					),
					menuItem(
						text = "Team",
						tabName = "TAB_TEAM"
					),
					menuItem(
						text = "References",
						tabName = "TAB_REFS"
					),
					menuItem(
						text = "Help",
						tabName = "TAB_HELP"
					)#,
					# menuItem(
					# 	text = "Setup",
					# 	tabName = "TAB_SETUP"
					# )
				),
				
				tags$hr( ),
				
				actionButton(
					"ID_CB_SET_TO_VISITOR_AGE",
					"set to visitor's age",
					width = "88%"
				),
				
				sliderInput(
					"ID_SI_AGE",
					label = "age [y]",
					min = 20,
					max = 80,
					animate = F,
					value = 50
				),
				
				tags$hr( ),
				
				actionButton(
					"ID_CB_SET_TO_VISITOR_RADDIFF",
					"set to visitor's radius difference"
				),
				
				sliderInput(
					"ID_SI_RADDIFF",
					label = "rad dif [°]",
					min = -.1,
					max = +.1,
					ticks = .01,
					animate = F,
					value = 0
				),
				
				tags$hr( ),
				
				box(
					title = "SDS - PERCENTILES",
					width = 12,
					collapsible = T,
					rHandsontableOutput(
						outputId = "ID_TABLE_PERCENTILES",
						width = '100%'
					)
				)
			),
		
		body =
			dashboardBody(
				tabItems(
					tabItem(
						tabName = "TAB_RNFLTD_2D_HEIDELBERG",
						#"RNFLTD OS - OD 2D",
						fluidRow(
							box(
								title = "Heidelberg Engeneering Plot",
								width = 12,
								height = "auto",
								plotOutput(
									"PLOT_RNFLTD_2D_HEIDELBERG",
									height = "912"
								) #%>%
								#	withSpinner( 1, "blue" )
							)
						)
					),
						
					tabItem(
						tabName = "TAB_RNFLTD_2D",
						#"RNFLTD OS - OD 2D",
						fluidRow(
							box(
								title = "RNFLT Differences OS - OD Plot",
								width = 12,
								height = "auto",
								plotlyOutput(
									"PLOT_RNFLTD_2D",
									height = "912"
								) #%>%
								#	withSpinner( 1, "blue" )
							)
						)
					),
					
					tabItem(
						tabName = "TAB_RNFLTD_3D_RDA",
						fluidRow(
							column(
								box(
									sliderInput(
										"ID_SI_OPACITY_RDA",
										"opacity",
										min = 0,
										max = 1,
										step = .01,
										value = .75
									),
									width = 12
									),
								width = 2
							),
							
							box(
								title = "RNFLT Differences OS - OD 3D Percentiles Plot over Radius Difference and Angle",
								width = 10,
								height = "auto",
								withSpinner(
									plotlyOutput(
										"PLOT_RNFLTD_3D_RDA",
										height = "912"
									),
									1,
									"blue"
								)
							)
						)
					),
					
					tabItem(
						tabName = "TAB_RNFLTD_3D_AA",
						fluidRow(
							column(
								box(
									sliderInput(
										"ID_SI_OPACITY_AA",
										"opacity",
										min = 0,
										max = 1,
										step = .01,
										value = .75
									),
									width = 12
								),
								width = 2
							),
							box(
								width = 10,
								title = "RNFLT Differences OS - OD 3D Percentiles Plot over Age and Angle",
								height = "auto",
								withSpinner(
									plotlyOutput(
										"PLOT_RNFLTD_3D_AA",
										height = "912"
									),
									1,
									"blue"
								)
							)
						)
					),
					
					tabItem(
						tabName = "TAB_WELCOME",
						box(
							width = 12,
							height = "auto",
							title = "Welcome",
							footer = "Have Fun!",
							includeMarkdown( "www/welcome.md" )
						)
					),
					
					tabItem(
						tabName = "TAB_VISITOR",
						box(
							title = "Visitor",
							width = 12,
							box(
								title = "Visitor Age and Radius Difference",
								width = 12,
									# rHandsontableOutput(
								# 	outputId = "ID_TABLE_VISITOR",
								# 	width = '100%'
								# )
								valueBoxOutput( "VBOX_AGE" ),
								valueBoxOutput( "VBOX_RDIFF" )
							),
							tags$hr( ),
							box(
								title = "Visitor Table",
								width = 12,
								DT::dataTableOutput( "ID_TABLE_VISITOR" )
							)
						)
					),
					
					tabItem(
						tabName = "TAB_METHODS",
						box(
							width = 12,
							height = "auto",
							title = "Methods",
							includeMarkdown( "www/methods.md" )
						)
					),
					
					tabItem(
						tabName = "TAB_TEAM",
						fluidRow(
							vip.card( "Prof. Dr. Markus Löffler", "PI",                           "http://www.imise.uni-leipzig.de/en/Staffmember/Markus.Loeffler.jsp",           "+49 341 97 16 100", "markus.loeffler@imise.uni-leipzig.de" ),
							vip.card( "Prof. Dr. Markus Scholz",  "PI",                           "http://www.imise.uni-leipzig.de/en/Staffmember/Markus.Scholz.jsp",             "+49 341 97 16 190", "markus.scholz@imise.uni-leipzig.de" ),
							vip.card( "Prof. Dr. Toralf Kirsten", "PI",                           "http://www.imise.uni-leipzig.de/en/Staffmember/Kirsten%40izbi.jsp",            "+49 341 97 16 704", "kirsten@izbi.uni-leipzig.de" ),
							vip.card( "PhD. Tobias Elze",         "Data Analysis",                "https://www.masseyeandear.org/research/investigators/e/elze-tobias",           "+617-912-2533",    "tobias-elze@tobias-elze.de" ),
							vip.card( "PhD. Franziska Rauscher",  "Organisation/Consultation",    "http://www.imise.uni-leipzig.de/Mitarbeiter/Franziska.Rauscher%40medizin.jsp", "+49 341 97 16195", "frauscher@imise.uni-leipzig.de" ),
							vip.card( "Thomas Peschel",           "Shiny",                        "http://www.imise.uni-leipzig.de/en/Staffmember/Thomas.Peschel.jsp",            "+49 97 16 101",    "tpeschel@imise.uni-leipzig.de" )
						)
					),
					
					# tabItem(
					# 	tabName = "TAB_TABLE_PERCECNTILES",
					# 	fluidRow(
					# 		box(
					# 			tableOutput( "TABLE_PERCENTILES_1" ),
					# 			width = 12
					# 		),
					# 		box(
					# 			tableOutput( "TABLE_PERCENTILES_2" ),
					# 			width = 12
					# 		)
					# 	)
					# ),
					
					tabItem(
						tabName = "TAB_REFS",
						box(
							width = 12,
							height = "auto",
							title = "Welcome",
							footer = "Have Fun!",
							includeMarkdown( "www/references.md" )
						)
					),
					
					tabItem(
						tabName = "TAB_HELP",
						box(
							width = 12,
							height = "auto",
							title = "Welcome",
							footer = "Have Fun!",
							includeMarkdown( "www/help.md" )
						)
					)#,
					
					# tabItem(
					# 	tabName = "TAB_SETUP",
					# 	box(
					# 		sliderInput(
					# 			"ID_SI_SCREEN_HEIGHT",
					# 			label = "SCREEN HEIGHT",
					# 			min = 600,
					# 			max = 2400,
					# 			ticks = 1,
					# 			value = 600,
					# 			round = T
					# 		)
					# 	),
					# 	fluidRow( 
					# 		column( 
					# 			width = 12,
					# 			textOutput(
					# 				"height"
					# 			),
					# 			fluidRow("Topleft", style = paste0( textOutput( "height" ), "background-color: yellow;") ),
					# 			fluidRow("Bottomleft", style = "height:200px; background-color: green;")
					# 		)
					# 	)
					# )
				)
			)
	)
				
