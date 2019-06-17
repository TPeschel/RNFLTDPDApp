rm( list = ls( ) )

library ( gamlss)
library ( shiny )
library ( plotly )
library ( shinycssloaders )
library ( magrittr )

#setwd("tobias/RNFLT/")

rnfltdiffnorms <- read.csv( "data/rnflt_difference_norms.csv" )
sector.abs.rnfltdiffnorms <- read.csv( "data/sector_abs_rnflt_difference_norms.csv", row.names = 1 )

#color.list <- rgb( red = runif( 20, min = 0, max = 1 ), green = runif( 20, min = 0, max = 1 ), blue = runif( 20, min = 0, max = 1 ) )
color.list <- colorRampPalette( c( "black", "red", "yellow", "green", "yellow", "red", "black" ) )( 32 )

#source( "TobiasElze/r/sources.R" ) # load the functions

#meas = read.csv( "data/rnfltdiff_example.csv" ) # load the example

#rnfltdiff = meas[ -c( 1, 2 ) ]

calculate.normative.distribution <- function( age, radiusdiff = 0, angles = seq( 0, 360, length = 768 ), smoothing = smooth.spline, norms = rnfltdiffnorms )
	# calculate normative distribution of RNFLT OS - OD 
	# for given angles (vector)
	# given a specific age and radius difference
	# smoothing: smoothing function
	#   default: smooth.spline
	#   if NULL: no smoothing
	#
	# returns a data frame with one row for each angle
{
	mus = norms$mu.intercept + norms$mu.age*age + norms$mu.age2*age^2 + norms$mu.age3*age^3 + norms$mu.rad*radiusdiff + norms$mu.rad2*radiusdiff^2 + norms$mu.rad3*radiusdiff^3
	sigmas = exp(norms$sigma.intercept + norms$sigma.age*age + norms$sigma.age2*age^2 + norms$sigma.age3*age^3 + norms$sigma.rad*radiusdiff + norms$sigma.rad2*radiusdiff^2 + norms$sigma.rad3*radiusdiff^3)
	
	if(!is.null(smoothing))
	{
		mus <- smoothing(norms$angle, mus)$y
		sigmas <- smoothing(norms$angle, sigmas)$y
	}
	
	mufun = approxfun(norms$angle, mus)
	sigmafun = approxfun(norms$angle, sigmas)
	data.frame(angle=angles, mu=mufun(angles), sigma=sigmafun(angles))
}
# 
# norm.dist <- calculate.normative.distribution( age = 10,radiusdiff = 0,norms = rnfltdiffnorms )
# nrms <- pNO( q = .5, mu = norm.dist$mu, sigma = norm.dist$sigma )
# 
# raddif <- seq( -.3, +.3, length.out = 100 )
# ages  <- c( 20 : 80 )
# 
# (
# 	z <-
# 		sapply(
# 			ages,
# 			function( age ) {
# 				norm.dist <- calculate.normative.distribution( age = age, radiusdiff = rd, norms = rnfltdiffnorms )
# 				pnorm( q = .5, mean = norm.dist$mu, sd = norm.dist$sigma )
# 			}
# 		)		
# )
# 
# 
# plot_ly( type = "surface", x = ~ages, y = ~norm.dist$angle, z = ~z )
# 

ui <-
	fluidPage(
		fluidRow(
			column(
				6,
				sliderInput( "ID_SI_AGE", "Age[y]", 20, 80, 80, animate = T )
			),
			column(
				6,
				sliderInput( "ID_SI_RAD", "Rad[mm]", -.1, .1, .1, .01, animate = T )
			)
		),
		fluidRow(
			column(
				6,
				plotlyOutput( "ID_PLOT1", height = "800" )# %>%
				#withSpinner( color = "red", color.background = "white", type = 6 ) 
			),
			column(
				6,
				plotlyOutput( "ID_PLOT2", height = "800" )# %>%
				#withSpinner( color = "red", color.background = "white", type = 6 ) 
			)
		)
	)

cents <- c( .025, .25, .500, .75, .975 )
angles <- seq( 0, 360, length.out = 768 )
ages  <- c( 20 : 80 )
rdiffs <- seq( -.1, +.1, length.out = length( ages ) )

server <-
	function( input, output, session ) {
		
		rv <- reactiveValues( )

		observe( {
			
			cents. <-
				lapply(
					cents,
					function( cent ) {
						sapply(
							ages,
							function( age ) {
								norm.dist <- calculate.normative.distribution( age = age, radiusdiff = input$ID_SI_RAD, norms = rnfltdiffnorms )
								qnorm( p = cent, mean = norm.dist$mu, sd = norm.dist$sigma )
							}
						)
					}
				)
			
			names( cents. ) <- paste0( 100 * cents, "%" )
			
			rv$data2 <-
				list( 
					age   = ages,
					angle = angles,
					cents = cents.
				)
		} )
		
		observe( {

			cents. <-
				lapply(
					cents,
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
			
			names( cents. ) <- paste0( 100 * cents, "%" )

			rv$data1 <-
				list(
					radius.difference = rdiffs,
					angle = angles,
					cents = cents.
				)
		} )
		
		output$ID_PLOT1 <-
			renderPlotly( {
				
				plt <- plot_ly( 
					type = "surface", 
					x = ~rv$data1$radius.difference, 
					y = ~rv$data1$angle, 
					cmin = -80,#min( rv$data1$cents[[ 1 ]], na.rm = T ),
					cmax = +80,#max( rv$data1$cents[[ 3 ]], na.rm = T ), 
					colorscale = list( c( 0, cents, 1 ), color.list[ 1 : ( length( cents ) + 2 ) ] ) )
				
				i <- 0
				
				for( cn in rv$data1$cents ) {
					
					print( i <- i + 1 )
				
					plt <- add_trace( p = plt, z = cn, name = paste0( 100 * cents[ i ], "%" ), showscale = i == 1, opacity = 1. )
				}
				
				plt
			} )

		output$ID_PLOT2 <-
			renderPlotly( {
				
				plt <- 
					plot_ly( 
						type = "surface", 
						x = ~rv$data2$age, 
						y = ~rv$data2$angle, 
						cmin = -80,#min( rv$data2$cents[[ 1 ]], na.rm = T ),
						cmax = +80,#max( rv$data2$cents[[ 3 ]], na.rm = T ), 
						colorscale = list( c( 0, cents, 1 ), color.list[ 1 : ( length( cents ) + 2 ) ] ) )
				
					
				i <- 0
				
				for( cn in rv$data2$cents ) {
					
					print( i <- i + 1 )
					
					plt <- add_trace( p = plt, z = cn, name = paste0( 100 * cents[ i ], "%" ), showscale = i == 1 )
				}

				plt
				# add_trace( type = "surface", x = ~rv$data2$age, y = ~rv$data2$angle, z = ~rv$data2$cents[[ 1 ]], cmin = min( rv$data2$cents[[ 1 ]], na.rm = T ), cmax = max( rv$data2$cents[[ 3 ]], na.rm = T ), name = paste0( 100 * cents[ 1 ], "%" ), colorscale = list( c( 0, .5, 1 ), c( "rgb(255,0,0)", "rgb(0,255,0)","rgb(255,0,0)" ) ) ) %>%
				# 	add_trace( type = "surface", x = ~rv$data2$age, y = ~rv$data2$angle, z = ~rv$data2$cents[[ 2 ]], cmin = min( rv$data2$cents[[ 1 ]], na.rm = T ), cmax = max( rv$data2$cents[[ 3 ]], na.rm = T ), name = paste0( 100 * cents[ 2 ], "%" ), colorscale = list( c( 0, .5, 1 ), c( "rgb(255,0,0)", "rgb(0,255,0)","rgb(255,0,0)" ) ), showscale = F ) %>%
				# 	add_trace( type = "surface", x = ~rv$data2$age, y = ~rv$data2$angle, z = ~rv$data2$cents[[ 3 ]], cmin = min( rv$data2$cents[[ 1 ]], na.rm = T ), cmax = max( rv$data2$cents[[ 3 ]], na.rm = T ), name = paste0( 100 * cents[ 3 ], "%" ), colorscale = list( c( 0, .5, 1 ), c( "rgb(255,0,0)", "rgb(0,255,0)","rgb(255,0,0)" ) ), showscale = F )
			} )
	}

shinyApp( ui, server )
