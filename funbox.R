output.table.percentiles <-
	function( table.name, data, output ) {
		
		output[[ table.name ]] <-
			rhandsontable::renderRHandsontable( {
				rhandsontable(
					data,
					useTypes = T,
					rowHeaders = F,
					stretchH = "all" ) %>%
					hot_validate_numeric( cols = "CNT", min = .001, max = 99.999, allowInvalid = F ) %>%
					hot_col( format = "0.000", col = "CNT", allowInvalid = F ) %>%
					hot_validate_numeric( cols = "SDS", min = -4.5, max = +4.5, allowInvalid = F ) %>%
					hot_col( format = "+0.000", col = "SDS", allowInvalid = F ) %>%
					hot_cols(
						renderer = "
						function ( instance, td, row, col, prop, value, cellProperties ) {
						Handsontable.renderers.NumericRenderer.apply( this, arguments );
						if ( row == 0 || row == instance.countRows( ) - 1 ) {
						td.style.background = '#f0f0e0';
						td.style.color = 'black';
						} else {
						td.style.background = 'lighgray';
						td.style.color = 'black';
						}
						}"
					)
			} )
	}

output.table.visitor <-
	function( table.name, data, output ) {
		
		output[[ table.name ]] <-
			rhandsontable::renderRHandsontable( {
				print( data )
				rhandsontable(
					data,
					useTypes = T,
					rowHeaders = F,
					stretchH = "all" ) %>%
					hot_validate_numeric( cols = "RADIUS.DIFFERENCE", min = -100, max = 100, allowInvalid = F ) %>%
					hot_col( format = "0.000", col = "RADIUS.DIFFERENCE", allowInvalid = F ) %>%
					hot_validate_numeric( cols = "ANGLE", min = 0, max = 360, allowInvalid = F ) %>%
					hot_col( format = "0.000", col = "ANGLE", allowInvalid = F ) #%>%
					# hot_cols(
					# 	renderer = "
					# 	function ( instance, td, row, col, prop, value, cellProperties ) {
					# 	Handsontable.renderers.NumericRenderer.apply( this, arguments );
					# 	if ( row == 0 || row == instance.countRows( ) - 1 ) {
					# 	td.style.background = '#f0f0e0';
					# 	td.style.color = 'black';
					# 	} else {
					# 	td.style.background = 'lighgray';
					# 	td.style.color = 'black';
					# 	}
					# 	}"
					# )
			} )
	}


tableOptions <-
	list(
		paging = TRUE,
		pageLength = 10,
		searching = TRUE,
		fixedColumns = TRUE,
		autoWidth = FALSE,
		ordering = TRUE,
		dom = 'Bpfrtip', 
		buttons = c('copy', 'excel', 'csv', 'print', 'colvis' )
	)

default.color.vector <-
	c( "darkblue", "yellow", "green", "yellow", "darkred" )

clrscl <-
	function( col.vec = default.color.vector, len = length( col.vec ) ) {
		
		if( length( col.vec ) == len ) {
			list( 
				c( seq( 0, 1, length.out = len ) ), 
				c( col.vec )
			)
		}
		else {
			list(
				c( seq( 0, 1, length.out = len ) ),
				colorRampPalette( col.vec )( len )
			)
		}
	}

clrscl.monochrome <-
	function( value.between.0.and.zero, col.vec = default.color.vector ) {
		vl <- 1 + 100 * value.between.0.and.zero
		cl <- colorRampPalette( col.vec )( 101 )
		list( 
			c( 0, 1 ),
			c( cl[ vl ], cl[ vl ] )
		)
	}
#clrscl.monochrome( .4, default.color.vector)
clrspal <-
	function( length = 5, col.vec = default.color.vector ) {
		colorRampPalette( col.vec )( length )
	}

percentiles.color.palette <-
	function( perc.vec, col.vec = default.color.vector ) {
		
		cp <- colorRampPalette( col.vec )( 101 )
		
		cp[ 1 + perc.vec ]
	}

# heidelberg.palette <-
# 	function( perc.vec ) {
# 		
# 		n  <- length( perc.vec )
# 		cp <- rep_len( "indianred1", n )
# 		cp[ abs( perc.vec - 50 ) < 49 ] <- "khaki1"
# 		cp[ abs( perc.vec - 50 ) < 45 ] <- "palegreen"
# 		cp
# 	}

heidelberg.palette <-
	function( perc.vec ) {
		
		n  <- length( perc.vec )
		cp <- rep_len( "#FF6A6A", n )
		cp[ abs( perc.vec - 50 ) < 49 ] <- "#FFF68F"
		cp[ abs( perc.vec - 50 ) < 45 ] <- "#98FB98"
		cp[ abs( perc.vec - 50 ) < 1 ]  <- "#32CD32"
		cp
	}

heidelberg.colorscale <-
	function( i, perc.vec ) {
		
		hp <- heidelberg.palette( perc.vec = perc.vec )
		
		list(
			c( 0, 1 ),
			c( hp[ i ], hp[ i ] )
		)
	}

add.pdf.format <-
	function(
		doc.type    = "spectralis",
		dpi         = 312, 
		os.xleft    = 280,
		od.xleft    = 1530,
		width       = 768,
		bottom      = 2265,
		height      = 257,
		plot.height = 300 ) {
		
		if( ! exists( "pdf.formats" ) ) {
			
			pdf.formats <<- NULL
		}
		
		pdf.formats[[ doc.type ]] <<- 
			list( 
				doc.type    = doc.type,
				dpi         = dpi,
				os.xleft    = os.xleft,
				od.xleft    = od.xleft,
				width       = width,
				bottom      = bottom,
				height      = height,
				plot.height = plot.height
			)
	}

add.pdf.format( )

add.pdf.format( "default", 312, 280, 1530, 768, 2265, 257, 300 )

pdf.formats

xtrct.plot.from.pdf <-
	function( fname, doc.type = "spectralis" ) {
		
		##########################################################################################################
		# some functions
		##########################################################################################################
		graph         <- function( a, d, oc = "os" ) a[ ( d [[ "bottom" ]] - d [[ "height" ]] ) : d [[ "bottom" ]],  d [[ paste0( oc, ".xleft" ) ]] : ( d [[ paste0( oc, ".xleft" ) ]] + d [[ "width" ]] ), ]
		
		black.pix     <- function( oc ) oc[ , , 1 ] == 0 & oc[ , , 2 ] == 0 & oc[ , , 3 ] == 0
		
		pos.of.blacks <- function( v ) median( which( v ) )
		
		get.y         <- function( doc, onz, nr = nr ) doc [[ "plot.height" ]] * ( nr -  apply( onz, 2, pos.of.blacks ) ) / nr
		
		thcknss       <- function( content, doc.info, oculus, nrows ) {
			
			y  <- get.y( doc.info, black.pix( graph( content, doc.info, oculus ) ), nrows )
			
			if( oculus == "od" && doc.type %in% c( "default", "spectralis" ) ) {
				
				y[ 1 ] <- y[ 768 ]
			}
			
			y
		}
		
		##########################################################################################################
		# use the functions
		##########################################################################################################
		
		doc.info     <- pdf.formats[[ doc.type ]]
		
		content      <- aperm( pdf_render_page( fname, dpi = doc.info [[ "dpi" ]] ), c( 3, 2, 1 ) )
		
		nr           <- nrow( content )
		
		osy          <- thcknss( content, doc.info, "os", nr )
		
		ody          <- thcknss( content, doc.info, "od", nr )
		
		angle        <- seq( 0, 360, length = length( osy ) )
		
		ret          <- as.data.frame( cbind( angle, osy, ody ) )
		
		names( ret ) <- c( "angle", "os", "od" )
		
		ret
	}
