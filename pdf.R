###
# PDF 
###

require( "pdftools" )

add.pdf.format <-
	function(
		doc.type       = "default",
		dpi            = 312, 
		od.xleft       = 280,
		os.xleft       = 1530,
		width          = 768,
		bottom         = 2265,
		height         = 257,
		plot.height    = 300
	) {
		
		if( ! exists( "pdf.formats" ) ) {
			
			pdf.formats <<- NULL
		}
		
		pdf.formats[[ doc.type ]] <<- 
			list( 
				doc.type       = doc.type,
				dpi            = dpi,
				od.xleft       = od.xleft,
				os.xleft       = os.xleft,
				width          = width,
				bottom         = bottom,
				height         = height,
				plot.height    = plot.height
			)
	}

xtrct.plot.from.pdf <-
	function( fname, doc.type = "default" ) {
		
		##########################################################################################################
		# some functions
		##########################################################################################################
		graph          <- function( a, d, o ) a[ ( d [[ "bottom" ]] - d [[ "height" ]] ) : d [[ "bottom" ]], d [[ paste0( o, ".xleft" ) ]] : ( d [[ paste0( o, ".xleft" ) ]] + ( d [[ "width" ]] - 1 ) ), ]
		
		mask.black.pix <- function( o ) o[ , , 1 ] == 0 & o[ , , 2 ] == 0 & o[ , , 3 ] == 0
		
		pos.of.blacks  <- function( blcks ) median( which( blcks ) )
		
		get.y          <- function( doc, blck.px ) { nrows = nrow( blck.px ); doc [[ "plot.height" ]] * ( nrows - apply( blck.px, 2, pos.of.blacks ) ) / nrows - 0 }

		thcknss        <- function( content, doc.info, oculus ) {
			
			g <- graph( content, doc.info, oculus )
			
			m <- mask.black.pix( g )
			
			y <- get.y( doc.info, m )
			
			#if( oculus == "od" && doc.type %in% c( "default", "spectralis" ) ) {
			if( doc.type %in% c( "default", "spectralis" ) ) {
					
				print( "impute one value" )
				
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
		
		ret          <- as.data.frame( cbind( angle, ody, osy ) )
		
		names( ret ) <- c( "angle", "od", "os" )
		
		attr( ret, "ID" ) <- fname
		
		ret
	}

xtrct.text.from.pdf <-
	function( fname, doc.type = "default" ) {
		
		txt <- pdf_text( fname )
		
		if( doc.type %in% c( "default", "spectralis" ) ) {
			
			list(
				birth = b<-as.Date( stringr::str_extract( stringr::str_extract( txt, "DOB:.*[0-9]{4}" ),     "[0-9].*" ), format = "%d.%b.%Y" ),
				exam  = e<-as.Date( stringr::str_extract( stringr::str_extract( txt, "Exam\\.:.*[0-9]{4}" ), "[0-9].*" ), format = "%d.%b.%Y" ),
				age   = round( as.double( difftime( e, b, "days" ) ) / 365.25, 1 ),
				sex   = c( "male", "female" )[ match( stringr::str_extract( stringr::str_extract( txt, "Sex:.*[FM]" ), "[FM]" ), c( "M", "F" ) ) ],
				fname = fname,
				id    = stringr::str_extract( stringr::str_extract( txt, "Patient ID:.*LI[0-9]{7}[0-9Xx]" ), "LI.*" )
			)
		}
	}


add.pdf.format( )

add.pdf.format( "spectralis", 312, 280, 1530, 768, 2265, 257, 300 )

pdf.formats

#xtrct.plot.from.pdf( "visitor/LI01316794_RNFLT.pdf" )
