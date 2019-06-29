###
# PDF 
###

require( "pdftools" )

add.pdf.format <-
	function(
		doc.type    = "default",
		dpi         = 312, 
		od.xleft    = 280,
		os.xleft    = 1530,
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
				od.xleft    = od.xleft,
				os.xleft    = os.xleft,
				width       = width,
				bottom      = bottom,
				height      = height,
				plot.height = plot.height
			)
	}

xtrct.plot.from.pdf <-
	function( fname, doc.type = "default" ) {
		
		fname = "visitor/LI01274671_Beispiel.pdf"
		
		##########################################################################################################
		# some functions
		##########################################################################################################
		graph          <- function( a, d, oc = "os" ) a[ ( d [[ "bottom" ]] - d [[ "height" ]] ) : d [[ "bottom" ]], d [[ paste0( oc, ".xleft" ) ]] : ( d [[ paste0( oc, ".xleft" ) ]] + d [[ "width" ]] ), ]
		
		mask.black.pix <- function( oc ) oc[ , , 1 ] == 0 & oc[ , , 2 ] == 0 & oc[ , , 3 ] == 0
		
		pos.of.blacks  <- function( blcks ) median( which( blcks ) )
		
		get.y          <- function( doc, blck.px ) { nrows = nrow( blck.px ); doc [[ "plot.height" ]] * ( nrows -  apply( blck.px, 2, pos.of.blacks ) ) / nrows }

		thcknss        <- function( content, doc.info, oculus ) {
			
			g <- graph( content, doc.info, oculus )
			
			m <- mask.black.pix( g )
			
			y  <- get.y( doc.info, m )
			
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
		
		osy          <- thcknss( content, doc.info, "os" )
		
		ody          <- thcknss( content, doc.info, "od" )
		
		angle        <- seq( 0, 360, length = length( osy ) )
		
		ret          <- as.data.frame( cbind( angle, osy, ody ) )
		
		names( ret ) <- c( "angle", "os", "od" )
		
		ret
	}

add.pdf.format( )

add.pdf.format( "spectralis", 312, 280, 1530, 768, 2265, 257, 300 )

pdf.formats

