source( "sources.R" )
source( "funbox.R" )

angles <- seq( 0, 360, length.out = 768 )
angle.names <- as.character( round( angles, 2 ) )
ages   <- c( 20 : 80 )
rdiffs <- seq( -.1, +.1, length.out = length( ages ) )
rdiffs.names <- as.character( round( rdiffs, 3 ) )
empty.row <- data.frame( SDS = NaN, CNT = NaN )

server <-
	function( input, output, session ) {
		
		rv <- reactiveValues( )

		get.cnt      <- function( i ) { rv [[ "TABLE_PERCENTILES" ]] [[ "CNT" ]] [ 1 + i ] }
		get.cnts     <- function( ) { na.omit( rv [[ "TABLE_PERCENTILES" ]] [[ "CNT" ]] ) }
		get.cnts.len <- function( ) { length( get.cnts( ) ) }
		cnts.exist   <- function( ) { 0 < get.cnts.len( ) }

		updateSliderInput( session = session, inputId = "ID_SI_AGE",     value = visitor [[ "age" ]] )
		updateSliderInput( session = session, inputId = "ID_SI_RADDIFF", value = visitor [[ "radiusdiff" ]] )
		
		output$VBOX_AGE <-
			renderValueBox( { valueBox( "Age", visitor [[ "age" ]] ) } )

		output$VBOX_RDIFF <-
			renderValueBox( { valueBox( "Radius Difference", visitor [[ "radiusdiff" ]] ) } )
		
		rv [[ "TABLE_PERCENTILES" ]] <- 
			data.frame(
				SDS = c( NaN, c( -3 : 3 ),NaN ),
				CNT = c( NaN, 100. * round( pnorm( c( -3 : 3 ) ), 4 ), NaN )
			)
		
		#	qn = sapply(c(0.01, 0.05, 0.5, 0.95, 0.99), function(x) qnorm(x, norms$mu, norms$sigma))
		norms = calculate.normative.distribution( visitor [[ "age" ]], visitor [[ "radiusdiff" ]], smoothing = smooth.spline, norms = rnfltdiffnorms )

		qn <- 100 * round( pnorm( unlist( visitor[ -c( 1,2 ) ] ), norms$mu, norms$sigma ), 3 )
		
		rv [[ "TABLE_VISITOR" ]] <-
			data.frame( 
				"ANGLE"      = format( angles, digits = 1 ),
				"RNFLTD"     = unlist( visitor[ -c( 1,2 ) ] ),
				"PERCENTILE" = qn 
			)
		
		rv [[ "PERCENTILES_UPDATED" ]] <- F
		
		output.table.percentiles( "ID_TABLE_PERCENTILES", rv [[ "TABLE_PERCENTILES" ]], output )
		
		observeEvent(
			input$ID_CB_SET_TO_VISITOR_AGE,
			updateSliderInput(
				session = session,
				inputId = "ID_SI_AGE",
				value = visitor [[ "age" ]]
			)
		)
		
		observeEvent(
			input$ID_CB_SET_TO_VISITOR_RADDIFF,
			updateSliderInput(
				session = session,
				inputId = "ID_SI_RADDIFF",
				value = visitor [[ "radiusdiff" ]]
			)
		)
		
		output$ID_TABLE_VISITOR <-
			DT::renderDT( 
				{
					validate(
						need( 
							rv [[ "TABLE_VISITOR" ]],
							message = "no visitor data present"
						)
					)
					
					DT::datatable(
						format( rv [[ "TABLE_VISITOR" ]], digits = 4 ),
						extensions = c( 'Buttons' ),
						options = tableOptions,
						class = "display"
					)
				},
				server = F
			)
		
		#output.table.visitor( "ID_TABLE_VISITOR", rv [[ "TABLE_VISITOR" ]], output )
		
		observeEvent(
			
			eventExpr  = c( rv [[ "PERCENTILES_UPDATED" ]], input [[ "ID_SI_AGE" ]], input [[ "ID_SI_RADDIFF" ]] ),
			handlerExpr = {
			
				if( cnts.exist( ) ) {
					
					cents. <-
						lapply(
							.01 * na.omit( rv [[ "TABLE_PERCENTILES" ]] [[ "CNT" ]] ),
							function( cent ) {
								norm.dist <- calculate.normative.distribution( age = input$ID_SI_AGE, radiusdiff = input$ID_SI_RADDIFF, norms = rnfltdiffnorms )
								qnorm( p = cent, mean = norm.dist$mu, sd = norm.dist$sigma )
							}
						)
					
					names( cents. ) <- paste0( na.omit( rv [[ "TABLE_PERCENTILES" ]] [[ "CNT" ]] ), "%" )
				}
				else  {
					
					cents. <- list( )
				}
				
				rv [[ "dataAngle" ]] <-
					list( 
						angle = angles,
						cents = cents.
					)
				
				norms <- calculate.normative.distribution( visitor [[ "age" ]], visitor [[ "radiusdiff" ]], smoothing = smooth.spline, norms = rnfltdiffnorms )
				
				qn <- 100 * round( pnorm( unlist( visitor[ -c( 1,2 ) ] ), norms$mu, norms$sigma ), 3 )
				
				rv [[ "TABLE_VISITOR" ]] <-
					data.frame( 
						"ANGLE"      = format( angles, digits = 1 ),
						"RNFLTD"     = unlist( visitor[ -c( 1,2 ) ] ),
						"PERCENTILE" = qn 
					)
				
				if( cnts.exist( ) ) 
					rv [[ "TABLE_VISITOR" ]] <-
					cbind( 
						rv [[ "TABLE_VISITOR" ]],
						rv [[ "dataAngle" ]] [[ "cents" ]]
					)
			}
		)
		
		observeEvent(
			
			eventExpr  = c( rv [[ "PERCENTILES_UPDATED" ]], input [[ "ID_SI_AGE" ]], input [[ "ID_SI_RADDIFF" ]] ),
			handlerExpr = {

				if( 0 < length( na.omit( rv [[ "TABLE_PERCENTILES" ]] [[ "CNT" ]] ) ) ) {
					
					cents. <-
						lapply(
							.01 * na.omit( rv [[ "TABLE_PERCENTILES" ]] [[ "CNT" ]] ),
							function( cent ) {
								sapply(
									ages,
									function( age ) {
										norm.dist <- calculate.normative.distribution( age = age, radiusdiff = input$ID_SI_RADDIFF, norms = rnfltdiffnorms )
										qnorm( p = cent, mean = norm.dist$mu, sd = norm.dist$sigma )
									}
								)
							}
						)
					
					names( cents. ) <- paste0( na.omit( rv [[ "TABLE_PERCENTILES" ]] [[ "CNT" ]] ), "%" )
				}
				else  {
					
					cents. <- list( )
				}
				
				rv [[ "dataAgeAngle" ]] <-
					list( 
						age   = ages,
						angle = angles,
						cents = cents.
					)
			} 
		)
		
		observeEvent(
			
			eventExpr  = c( rv [[ "PERCENTILES_UPDATED" ]], input [[ "ID_SI_AGE" ]], input [[ "ID_SI_RADDIFF" ]] ),
			handlerExpr = {
				
				if( 0 < length( na.omit( rv [[ "TABLE_PERCENTILES" ]] [[ "CNT" ]] ) ) ) {
					
					cents. <-
						lapply(
							.01 * na.omit( rv [[ "TABLE_PERCENTILES" ]] [[ "CNT" ]] ),
							function ( cent ) {
								sapply(
									rdiffs,
									function( rdiff ) {
										norm.dist <- calculate.normative.distribution( age = input$ID_SI_AGE, radiusdiff = rdiff, norms = rnfltdiffnorms )
										qnorm( p = cent, mean = norm.dist$mu, sd = norm.dist$sigma )
									}
								)
							}
						)
					
					names( cents. ) <- paste0( na.omit( rv [[ "TABLE_PERCENTILES" ]] [[ "CNT" ]] ), "%" )
				}
				else  {
					
					cents. <- list( )
				}
				
				rv [[ "dataRaddiffAngle" ]] <-
					list(
						radius.difference = rdiffs,
						angle = angles,
						cents = cents.
					)
			}
		)

		output$CB_AA <-
			renderUI( 
				{
					checkboxGroupInput(
						"CB_AA",
						"select percentiles surface to show",
						choices = paste0( get.cnts( ), "%" ),
						selected = paste0( get.cnts( ), "%" )
					)
				}
			)
		
		output$CB_RDA <-
			renderUI( 
				{
					checkboxGroupInput(
						"CB_RDA",
						"select percentiles surface to show",
						choices = paste0( get.cnts( ), "%" ),
						selected = paste0( get.cnts( ), "%" )
					)
				}
			)
		
		output$PLOT_RNFLTD_2D_HEIDELBERG <-
			renderPlot( {
				difference.colorplot( visitor[ -c( 1, 2 ) ], input$ID_SI_AGE, input$ID_SI_RADDIFF )
#				difference.colorplot( visitor[ -c( 1, 2 ) ], visitor [[ "age" ]], visitor [[ "radiusdiff" ]] )
			} )
		
		output$PLOT_RNFLTD_2D <-
			renderPlotly( {
				
				# validate(
				# 	need(
				# 		0 < length( get.cnts( ) ),
				# 		"no percentiles given"
				# 		)
				# 	)
				
				plt <-
					plot_ly( ) %>%
						layout(
							xaxis = list( title = "ang [°]", range = c( 0, 360 ) ),
							yaxis = list( title = "RNFLT OS-OD [µm]" )
						)
					
				#cp <- percentiles.color.palette( as.numeric( gsub( "%", "", names( rv [[ "dataAngle" ]] [[ "cents" ]] ) ) ) )
				
				hp <- heidelberg.palette( get.cnts( ) );
				
				print( get.cnts( ) )
				
				print( hp )
				
				if( 0 < length( rv [[ "dataAngle" ]] [[ "cents" ]] ) )
				for( i in 1 : length( rv [[ "dataAngle" ]] [[ "cents" ]] ) ) {
					
					cn <- rv [[ "dataAngle" ]]$cents[[ i ]]
					
					plt <- 
						add_lines(
							p = plt,
							x = rv [[ "dataAngle" ]]$angle,
							y = cn,
							name = names( rv [[ "dataAngle" ]] [[ "cents" ]] )[ i ],
							color = i,
							colors = hp
						)
				}

				plt <-
					add_trace(
						p = plt,
						type = "scatter",
						mode = "lines",
						x = rv [[ "TABLE_VISITOR" ]] [[ "ANGLE" ]],
						y = rv [[ "TABLE_VISITOR" ]] [[ "RNFLTD" ]],
						name = paste0( "visitor:\nage:", visitor [[ "age" ]], "\nrdiff: ", visitor [[ "radiusdiff" ]] ),
						color = I( "black" )
					)
				
				plt
			} )
		
		output$PLOT_RNFLTD_3D_RDA <-
			renderPlotly( {
				
				plt <- 
					plot_ly( 
						 
					) %>%
					layout(
						scene = list(
							xaxis = list( title = "rad dif [mm]" ),
							yaxis = list( title = "ang [°]" ),
							zaxis = list( title = "RNFLT OS-OD [µm]" )
						)
					)
				
				#cs <- clrscle( )

				if( 0 < length( rv [[ "dataRaddiffAngle" ]] [[ "cents" ]] ) )
				for( i in 1 : length( rv [[ "dataRaddiffAngle" ]] [[ "cents" ]] ) ) {

					if( names( rv [[ "dataRaddiffAngle" ]]$cents )[ i ] %in% input$CB_RDA ) {
						
						cn <- rv [[ "dataRaddiffAngle" ]]$cents[[ i ]]
						
						hc <- heidelberg.colorscale( i, get.cnts( ) )
						
						plt <-
							add_trace(
								p = plt,
								type = "surface",
								x = rv [[ "dataRaddiffAngle" ]]$radius.difference,
								y = rv [[ "dataRaddiffAngle" ]]$angle,
								z = cn, 
								name = names( rv [[ "dataRaddiffAngle" ]]$cents )[ i ], 
								showscale = i == 1,
								opacity = input$ID_SI_OPACITY_RDA,
								cmin = -80,#min( rv$data1$cents[[ 1 ]], na.rm = T ),
								cmax = +80,#max( rv$data1$cents[[ 3 ]], na.rm = T ),
								colorscale = hc
							)
					}
				}
				
				cp <- heidelberg.palette( get.cnts( ) )
				
				if( 0 < length( rv [[ "dataAngle" ]] [[ "cents" ]] ) )
				for( i in 1 : length( rv [[ "dataAngle" ]] [[ "cents" ]] ) ) {
					
					cn <- rv [[ "dataAngle" ]]$cents[[ i ]]
					
					plt <- 
						add_trace(
							p = plt,
							type = "scatter3d", 
							mode = "lines",
							x = rep( input$ID_SI_RADDIFF, ncol( visitor ) - 2 ),
							y = rv [[ "dataAngle" ]]$angle,
							z = cn,
							name = names( rv [[ "dataAngle" ]] [[ "cents" ]] )[ i ],
							line = list( width = 10, color = cp[ i ] )
						)
				}
				
				plt <-
					add_trace(
						p = plt,
						type = "scatter3d",
						mode = "lines",
						x = rep( visitor$radiusdiff, ncol( visitor ) - 2 ),
						y = rv [[ "TABLE_VISITOR" ]] [[ "ANGLE" ]],
						z = rv [[ "TABLE_VISITOR" ]] [[ "RNFLTD" ]],
						name = paste0( "visitor:\nage:", visitor [[ "age" ]], "\nrdiff: ", visitor [[ "radiusdiff" ]] ),
						line = list( width = 15, color = "black" )
					)

				plt
			} )
		
		output$PLOT_RNFLTD_3D_AA <-
			renderPlotly( {
				
				plt <- 
					plot_ly( 
						
					) %>%
					layout(
						scene = list(
							xaxis = list( title = "age [y]" ),
							yaxis = list( title = "ang [°]" ),
							zaxis = list( title = "RNFLT OS-OD [µm]" )
						)
					)

				if( 0 < length( rv [[ "dataAgeAngle" ]] [[ "cents" ]] ) )
				for( i in 1 : length( rv [[ "dataAgeAngle" ]] [[ "cents" ]] ) ) {
					
					if( names( rv [[ "dataAgeAngle" ]]$cents )[ i ] %in% input$CB_AA ) {
					
						cn <- rv [[ "dataAgeAngle" ]]$cents[[ i ]]
						
						hc <- heidelberg.colorscale( i, get.cnts( ) )
						
						plt <- 
							add_trace( 
								p = plt, 
								showscale = F,#i == 1, 
								type = "surface", 
								x = rv [[ "dataAgeAngle" ]]$age, 
								y = rv [[ "dataAgeAngle" ]]$angle,
								z = cn, 
								name = names( rv [[ "dataAgeAngle" ]]$cents )[ i ], 
								opacity = input$ID_SI_OPACITY_AA,
								cmin = -80,#min( rv$data2$cents[[ 1 ]], na.rm = T ),
								cmax = +80,#max( rv$data2$cents[[ 3 ]], na.rm = T ), 
								colorscale = hc
							)
					}
				}

				cp <- heidelberg.palette( get.cnts( ) )

				if( 0 < length( rv [[ "dataAngle" ]] [[ "cents" ]] ) )
				for( i in 1 : length( rv [[ "dataAngle" ]] [[ "cents" ]] ) ) {
					
					cn <- rv [[ "dataAngle" ]]$cents[[ i ]]
					
					plt <- 
						add_trace(
							p = plt,
							type = "scatter3d", 
							mode = "lines",
							x = rep( input$ID_SI_AGE, ncol( visitor ) - 2 ),
							y = rv [[ "dataAngle" ]]$angle,
							z = cn,
							name = names( rv [[ "dataAngle" ]] [[ "cents" ]] )[ i ],
							line = list( width = 10, color = cp[ i ] )
						)
				}
				
				plt <-
					add_trace(
						p = plt,
						type = "scatter3d", 
						mode = "lines",
						x = rep( visitor$age, ncol( visitor ) - 2 ),
						y = rv [[ "TABLE_VISITOR" ]] [[ "ANGLE" ]],
						z = rv [[ "TABLE_VISITOR" ]] [[ "RNFLTD" ]],
						name = paste0( "visitor:\nage:", visitor [[ "age" ]], "\nrdiff: ", visitor [[ "radiusdiff" ]] ),
						line = list( width = 15, color = "black" )
					)
				
				plt
				
			} )
		
		# observe(
		# 	output$TABLE_PERCENTILES_1 <-
		# 		renderTable( {
		# 			
		# 			rv$data1$cents$'50%'	
		# 		},
		# 		colnames = T,
		# 		rownames = T )
		# )
		# 
		# observe(
		# 	output$TABLE_PERCENTILES_2 <-
		# 		renderTable( {
		# 			
		# 			rv$data2$cents$'50%'
		# 		},
		# 		colnames = T,
		# 		rownames = T )
		# )
		
		# observe( {
		# 	
		# 	print( paste0( "height:", input$ID_SI_SCREEN_HEIGHT, "px;" ) )
		# 	
		# 	output$height <- renderText( paste0( "height:", input$ID_SI_SCREEN_HEIGHT, "px;" ) )
		# } )
		
		observeEvent(
			eventExpr = input [[ "ID_TABLE_PERCENTILES" ]]$change$change,
			handlerExpr = {
				
				rv [[ "PERCENTILES_UPDATED" ]] <- F

				if( is.null( input [[ "ID_TABLE_PERCENTILES" ]] ) ) {

					tb <- rv [[ "TABLE_PERCENTILES" ]]
				}
				else {

					tb <- hot_to_r( input [[ "ID_TABLE_PERCENTILES" ]] )

					i <- input [[ "ID_TABLE_PERCENTILES" ]]$changes$changes[[ 1 ]]

					names( i ) <- c( "row", "col", "old.val", "new.val" )

					if( i$col < 1 ) {

						if( is.null( i$new.val ) ) {

							tb$CNT[ i$row + 1 ] <- NaN
						}
						else {

							tb$CNT[ i$row + 1 ] <- 100 * pnorm( tb$SDS[ i$row + 1 ] )
						}
					}
					else {

						if( is.null( i$new.val ) ) {

							tb$SDS[ i$row + 1 ] <- NaN
						}
						else {

							tb$SDS[ i$row + 1 ] <- qnorm( .01 * tb$CNT[ i$row + 1 ] )
						}
					}

					tb <- unique( round( tb, 3 ) )

					tb <- na.omit( tb )

					tb <- tb[ order( tb$CNT ), ]

					tb <- rbind( empty.row, tb, empty.row )

					rv [[ "TABLE_PERCENTILES" ]] <- tb
					
					rv [[ "PERCENTILES_UPDATED" ]] <- T
					
					output.table.percentiles( "ID_TABLE_PERCENTILES", rv [[ "TABLE_PERCENTILES" ]], output )
				}
			}
		)

	}
