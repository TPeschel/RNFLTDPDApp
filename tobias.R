# rm( list =  ls( ) )
# 
###
require( "gamlss.dist" )
require( "plotrix" )

###
# ply( x, c( c1, c2, c3 ), c0 ) 
# ret = c0 + c1 * x + c2 * x^2 + c3 * x^3
# ret = x * ( x * ( x * c3 + c2 ) + c1 ) + c0
## 
ply <-
	function( x, coefficients, intercept = 0 ) {
		
		s <- 0
		
		for( c in rev( coefficients ) ) { 
			
			s <- x * ( s + c ) 
		}
		
		s + intercept
	}

# return TRUE for all measurement locations that belong to the sector
# given by sector.label
#
# sector.label: one of "rnfltMeanT", "rnfltMeanTS", "rnfltMeanTI", 
#   "rnfltMeanN", "rnfltMeanNS", "rnfltMeanNI", "rnfltMeanG"
# n: number of A-scans, i.e. measurement locations on the circle
sector.indices <-
	function( sector.label, n = 768 ) {
		
		len       <- round( n / 8 )
		
		sec       <- 1 : len
		
		( 1 : n ) %in%
			switch(
				sector.label,
				rnfltMeanT  = c( sec, ( n + 1 ) - sec ),
				rnfltMeanTS = sec + len,
				rnfltMeanNS = sec + 2 * len,
				rnfltMeanN  = ( 1 : ( 2 * len ) ) + 3 * len,
				rnfltMeanNI = sec + 5 * len,
				rnfltMeanTI = sec + 6 * len,
				1 : n 
			)
	}

# calculate normative distribution of RNFLT OS - OD 
# for given angles (vector)
# given a specific age and radius difference
# smoothing: smoothing function
#   default: smooth.spline
#   if NULL: no smoothing
#
# returns a data frame with one row for each angle
calculate.normative.distribution <- 
	function( age, rdf = 0, angles = constants$angles, smoothing = smooth.spline, params ) {#     = rnfltdiffnorms ) {
	
		# mus    <-
		# 	params$mu.intercept +
		# 	age * ( params$mu.age + age * ( params$mu.age2 + age * params$mu.age3 ) ) +
		# 	rdf * ( params$mu.rad + rdf * ( params$mu.rad2 + rdf * params$mu.rad3 ) )
		# 
		# sigmas <-
		# 	exp(
		# 		params$sigma.intercept +
		# 		age * ( params$sigma.age + age * ( params$sigma.age2 + age * params$sigma.age3 ) ) +
		# 		rdf * ( params$sigma.rad + rdf * ( params$sigma.rad2 + rdf * params$sigma.rad3 ) )
		# 	)
		
		params.names <- names( params )
	
		mus    <- 
			params$mu.intercept +
			ply( age, params[ , grep( "mu.age", params.names ) ] ) +
			ply( rdf, params[ , grep( "mu.rad", params.names ) ] )
		
		sigmas <-
			exp( 
				params$sigma.intercept +
				ply( age, params[ , grep( "sigma.age", params.names ) ] ) +
				ply( rdf, params[ , grep( "sigma.rad", params.names ) ] )
			)
		
		if( ! is.null( smoothing ) ) {
				
			mus    <- smoothing( params$angle, mus )$y
			sigmas <- smoothing( params$angle, sigmas )$y
		}
		
		mufun    <- approxfun( params$angle, mus )
		sigmafun <- approxfun( params$angle, sigmas )
		
		data.frame(
			angle = angles,
			mu    = mufun( angles ),
			sigma = sigmafun( angles ) 
		)
	}

# like calculate.normative.distribution, but for sectors instead
# of angles, and with absolute RNFLT differences instead of
# RNFLT differences, and a Box-Cox t distribution
# 
# sectors: which sector(s), specified as rnfltMeanX, with X:
#   G: global mean
#   T: Temporal mean
#   TS: supero-temporal mean
#   TI: infero-temporal mean
#   N: nasal mean
#   NS: supero-nasal mean
#   NI: infero-nasal mean
# (The sectoring scheme refers to the Spectralis printout.)
# 
# returns a data frame with one row for each sector
calculate.normative.distribution.sectors.absdiff <-
	function(
		age, 
		rdf      = 0,
		sectors  = constants$sectors, 
		params ) {#     = sector.abs.rnfltdiffnorms ) {
			
		params.names <- names( params )
		
		mus    <- 
			params$mu.intercept +
			ply( age, params[ , grep( "mu.age", params.names ) ] ) +
			ply( rdf, params[ , grep( "mu.rad", params.names ) ] )
		
		sigmas <-
			exp( 
				params$sigma.intercept +
					ply( age, params[ , grep( "sigma.age", params.names ) ] ) +
					ply( rdf, params[ , grep( "sigma.rad", params.names ) ] )
			)
		
		
		nus  <- params$nu.intercept
		
		taus <- params$tau.intercept
			
		selected <- params$sector.labels %in% sectors
			
		data.frame(
			mu    = mus[ selected ],
			sigma = sigmas[ selected ],
			nu    = nus[ selected ],
			tau   = taus[ selected ],
			row.names = sectors
		)
	}

# plot RNFLT differences (OS - OD) together with population based norms
# specific to age and scanning radius difference
#
# rnfltdiff: RNFLT difference, arranged in a vector of length n 
#   representing measurements at equal distances, spanning
#   from 0 degree (temporal, 1st element) to superior to nasal to inferior
#   back to temporal orientation around ONH (360 degree last element)
# age: age in years
# radiusdiff: radius difference in mm
# ...: further elements for plot
plot.rnflt.diff.with.norms <-
	function( 
		rnfltdiff, 
		age, 
		radiusdiff = 0, 
		angles     = constants$angles,
		cents      = constants$cents,
		smoothing  = smooth.spline,
		params ) {
	
		prms <- calculate.normative.distribution( age = age, angles, rdf = radiusdiff, smoothing = smooth.spline, params = params )
	
		#	qn = sapply(c(0.01, 0.05, 0.5, 0.95, 0.99), function(x) qnorm(x, norms$mu, norms$sigma))
	
		qn <- sapply( cents, function( x ) qnorm( x, prms$mu, prms$sigma ) )
	
		r  <- 1.1 * range( qn )
		
		plot(
			angles,
			angles,
			type="n",
			xlab="Angle (degree)", 
			ylab=expression(paste("RNFLT OS-OD (", mu, "m)")), 
			ylim=r,
			xaxs="i", yaxs="i"
		)
		
		rect( 0, r[ 1 ], 360, r[ 2 ], col = "palegreen", border = NA )
		
		xpoly <- c( angles[ 1 ], angles, angles[ 768 ] )
		
		polygon( xpoly, c( r[ 1 ], qn[ , 2 ], r[ 1 ] ), col = "khaki1", border = NA )
		
		polygon( xpoly, c( r[ 2 ], qn[ , 4 ], r[ 2 ] ), col = "khaki1", border = NA )
		
		polygon( xpoly, c( r[ 1 ], qn[ , 1 ], r[ 1 ] ), col = "indianred1", border = NA )
		
		polygon( xpoly, c( r[ 2 ], qn[ , 5 ], r[ 2 ] ), col = "indianred1", border = NA )
		
		abline( v = seq( 0, 270, by = 90 ) + 45, lty = "dotted", col = "gray20" )
		
		abline( v = c( 90, 270 ), lty = "dotted", col = "gray20" )
		
		plot.label <-
			function( xpos, l ) {
		
			text( xpos, r[ 1 ], l, pos = 3 )
		}
		
		mapply( 
			plot.label, 
			c( 22.5, 67.5, 112.5, 180, 360 - 112.5, 360 - 67.5, 360 - 22.5 ), 
			c( "T", "TS", "NS", "N", "NI", "TI", "T" ) 
		)
		
		abline( h = 0, lty = "dotted", col = "gray20" )
		
		lines( angles, qn[ , 3 ], col = "limegreen", lwd = 3 )
		
		xx<- seq( 0, 360, length = length( rnfltdiff ) )
		
		lines( xx, rnfltdiff, lwd = 3 )
		
		legend(
			"top",
			horiz  = F,
			col    = c( "palegreen", "khaki1", "indianred1" ),
			legend = c( "5%-95%", "1%-5% / 95%-99%", "<1% / >99%" ),
			pch    = 15,
			bg     = "white"
		)
	}

# analogous to plot.rnflt.diff.with.norms, but for the sectors (and global mean)
# and with absolute RNFLT differences
#
# abs.rnfltdiff.sectors: list with absolute RNFLT difference for each sector
#   list elements: "rnfltMeanG", "rnfltMeanT", "rnfltMeanTS", "rnfltMeanTI", 
#   "rnfltMeanN", "rnfltMeanNS", "rnfltMeanNI"
plot.rnflt.diff.sectors.with.norms <- 
	function( rnfltdiff, age, radiusdiff = 0, sector.labels = constants$sectors, params ) {

		calculate.median.abs.rnflt.diff <- 
			function( sector ) {
			
				mean( as.numeric( abs( rnfltdiff[ sector.indices( sector ) ] ) ), na.rm = T )
			}
	
		abs.mean.diffs <- sapply( constants$sectors, calculate.median.abs.rnflt.diff )
	
		prms <- calculate.normative.distribution.sectors.absdiff( age = age, rdf = radiusdiff, params = params )
	
		qn <- sapply(c(0.5, 0.95, 0.99), function( x ) qBCT( x, prms$mu, prms$sigma, prms$nu, prms$tau ) )
		
		m  <- as.data.frame( cbind( as.numeric( abs.mean.diffs ), qn ) )
		
		colnames( m ) <- c( "m", "median", "q95", "q99" )
		
		rownames( m ) <- sector.labels
		
		cols <- ifelse( m$m >= m$q99, "indianred1", ifelse( m$m >= m$q95, "khaki1", "palegreen" ) )
		
		names( cols ) <- sector.labels
		
		bw <- 0.05	# border width
		
		op <- par( mar = rep( 0.1, 4 ) )
		
		plot( 1, 1, xlim = c( -3, 3 ), ylim = c( -5, 4 ), type = "n", bty = "n", xaxt = "n", yaxt = "n", asp = 1, xlab = "", ylab = "" )
		
		draw.circle( 0, 0, 1 - bw, col = cols[ "rnfltMeanG" ], border = NA )
		
		draw.sec <- 
			function( angle1, angle2, ... ) {
				drawSectorAnnulus( 
					( angle1 - 2 ) * pi / 180, 
					( angle2 + 2 ) * pi / 180, 
					radius1 = 1 + bw, 
					radius2 = 3,
					... )
			}
		
		draw.sec( 180 + 45, 180 - 45, col = cols[ "rnfltMeanT" ] )
		
		draw.sec( 180 - 45,  90,      col = cols[ "rnfltMeanTS" ] )
		
		draw.sec(  90,       45,      col = cols[ "rnfltMeanNS" ] )
		
		draw.sec(  45,      -45,      col = cols[ "rnfltMeanN" ] )
		
		draw.sec( -45,      -90,      col = cols[ "rnfltMeanNI" ] )
		
		draw.sec( -90,     -135,      col = cols[ "rnfltMeanTI" ] )
		
		draw.circle( 0, 0, 3 )
		
		add.label <-
			function( sec = "rnfltMeanG" ) {
				
				label <- sub( "rnfltMean(.+)$", "\\1", sec )
			
				r <- ifelse( label == "G", 0, 2 )
			
				phi <-
					switch(
						label,
						T  = 180,
						TS = 112.5,
						NS = 67.5,
						N  = 0,
						NI = -67.5,
						TI = -112.5,
						0
					)
				
				x <- r * cos( phi * pi / 180 )
				
				y <- r * sin( phi * pi / 180 )
			
				text( x, y, paste( label, round( abs.mean.diffs[ sec ] ), "", sep = "\n" ) )
			
				text( x, y, sprintf( "\n\n(%.0f)", m[ sec, "median" ] ), col = "limegreen" )
			}
		
		lapply( constants$sectors, add.label )
		
		text( 0, 4, "Mean absolute RNFLT\ndifference |OS-OD|", font = 2, cex = 1.2 )
		
		y <- textbox( c( 0, 3 ),  -4,    "<95%",      justify = "c", margin = 0.1, fill = "palegreen" )
		
		y <- textbox( c( 0, 3 ), y[ 2 ], "95% - 99%", justify = "c", margin = 0.1, fill = "khaki1" )
		
		y <- textbox( c( 0, 3 ), y[ 2 ], ">99%",      justify = "c", margin = 0.1, fill = "indianred1" )
		
		par( op )
	}

# combines plot.rnflt.diff.sectors.with.norms and plot.rnflt.diff.with.norms
# in single plot
difference.colorplot <- 
	function( rnfltdiff, age, radiusdiff = 0, smoothing = smooth.spline, params.rdiff, params.absrdiff ) {
	
		layout( matrix( 1 : 2, nrow = 1 ), widths = c( 0.75, 0.25 ) )
		
		plot.rnflt.diff.with.norms( rnfltdiff = rnfltdiff, age = age, radiusdiff = radiusdiff, smoothing = smoothing, params = params.rdiff )
		
		plot.rnflt.diff.sectors.with.norms( rnfltdiff = rnfltdiff, age = age, radiusdiff = radiusdiff, params = params.absrdiff )
	}

difference.colorplot.wrapper <-
	function( visitor, smoothing = smooth.spline, params.rdiff, params.absrdiff ) {
		
		difference.colorplot( rnfltdiff = visitor[ -c( 1, 2 ) ], age = visitor [[ "age" ]], radiusdiff = visitor [[ "radiusdiff" ]], smoothing = smoothing, params.rdiff = params.rdiff, params.absrdiff = params.absrdiff )
	}

constants <<-
	list( 
		angles  = seq( 0, 360, length = 768 ),
		cents   = c( 0.01, 0.05, 0.5, 0.95, 0.99 ),
		sectors = c( "rnfltMeanG", "rnfltMeanT", "rnfltMeanTS", "rnfltMeanTI", "rnfltMeanN", "rnfltMeanNS", "rnfltMeanNI" )
	)

load( "data/examples.RData" )

load( "data/parameters.RData" )

# difference.colorplot.wrapper( examples$rnfltdiff_example, params.rdiff = parameters$rnflt_diff_params, params.absrdiff = parameters$sector_abs_rnflt_diff_params )
# 
# calculate.normative.distribution( 
# 	age       = examples$rnfltdiff_example$age,
# 	rdf       = examples$rnfltdiff_example$radiusdiff,
# 	angles    = constants$angles,
# 	smoothing = smooth.spline,
# 	params    = parameters$rnflt_diff_params )
# 
# calculate.normative.distribution.sectors.absdiff(
# 	age     = examples$rnfltdiff_example$age,
# 	rdf     = examples$rnfltdiff_example$radiusdiff,
# 	sectors = constants$sectors,
# 	params  = parameters$sector_abs_rnflt_diff_params
# )
# 
# plot.rnflt.diff.with.norms( 
# 	rnfltdiff  = examples$rnfltdiff_example[ , -c( 1, 2 ) ],
# 	age        = examples$rnfltdiff_example$age,
# 	radiusdiff = examples$rnfltdiff_example$radiusdiff,
# 	angles     = constants$angles,
# 	cents      = constants$cents,
# 	smoothing  = smooth.spline,
# 	params     = parameters$rnflt_diff_params )
# 
# plot.rnflt.diff.sectors.with.norms(
# 	rnfltdiff  = examples$rnfltdiff_example[ , -c( 1, 2 ) ],
# 	age        = examples$rnfltdiff_example$age,
# 	radiusdiff = examples$rnfltdiff_example$radiusdiff,
# 	params     = parameters$sector_abs_rnflt_diff_params 
# )
