rm( list = ls( ) )
# extracts RNFLT curves from Spectralis printouts
require( "pdftools" )
require( "png" )

# opens a pdf file and renders it with dpi dots per inch into a matrix of color channels
pdf2mat <-
	function( filename, dpi = 312 ) {
		# maybe the following works easier somehow?
		readPNG( writePNG( pdf_render_page( filename, dpi = dpi ) ) )
	}

# xleft is 280 for OD and 1530 for OS
get.thickness.plot <-
	function( xleft, ybottom = 2265, height = 257, mat ) {
		mat[ ( ybottom - height ) : ybottom, xleft : ( xleft + 768 ), ]
	}

# takes the thickness plot as 4-D matrix and extracts 
# the median of the black line, column by column;
# mat needs to contain the precise image patch with the plot,
# and the y-coordinates need to be (0, 300)
matrix2thickness <- 
	function( mat ) {
		# black pixels: all 3 color channels are 0:
		m <- mat[,,1] == 0 & mat[,,2] == 0 & mat[,,3] == 0
		nr <- nrow( m )
		300 * ( nr -  apply( m, 2, function( v ) median( which( v ) ) ) ) / nr
	}

extract.thickness <-
	function( xleft, mat ) {
		matrix2thickness( get.thickness.plot( xleft, mat = mat ) )
	}

# reads a pdf file of the standard Spectralis circle scan printout
# and extracts thicknesses of OS and OD from the thickness curves
#
# filename: file name of the pdf file
spectralis.pdf.to.thicknesses <- 
	function( filename ) {
		
		mat <- pdf2mat( filename )
		
		os  <- extract.thickness(  280, mat = mat )
		od  <- extract.thickness( 1530, mat = mat )
		
		# the OD thickness plot is one pixel "shorter":
		
		od[ 1 ] <- od[ 768 ]
		
		angle <- seq( 0, 360, length = length( os ) )
		
		as.data.frame( cbind( angle, os, od ) )
	}

require( "plotly" )

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
	
		d <- pdf.formats[[ doc.type ]]
		
		a <- aperm( pdf_render_page( fname, dpi = d [[ "dpi" ]] ), c( 3, 2, 1 ) )
		
		os <- a[ ( d [[ "bottom" ]] - d [[ "height" ]] ) : d [[ "bottom" ]],  d [[ "os.xleft" ]] : ( d [[ "os.xleft" ]] + d [[ "width" ]] ), ]
		
		od <- a[ ( d [[ "bottom" ]] - d [[ "height" ]] ) : d [[ "bottom" ]],  d [[ "od.xleft" ]] : ( d [[ "od.xleft" ]] + d [[ "width" ]] ), ]

		osnz <- os[ , , 1 ] == 0 & os[ , , 2 ] == 0 & os[ , , 3 ] == 0
		
		odnz <- od[ , , 1 ] == 0 & od[ , , 2 ] == 0 & od[ , , 3 ] == 0
		
		nr <- nrow( a )
		
		osy <- d [[ "plot.height" ]] * ( nr -  apply( osnz, 2, function( v ) median( which( v ) ) ) ) / nr
		
		ody <- d [[ "plot.height" ]] * ( nr -  apply( odnz, 2, function( v ) median( which( v ) ) ) ) / nr
		
		ody[ 1 ] <- ody[ 768 ]
	
		angle = seq( 0, 360, length = length( osy ) )
		
		ret <- as.data.frame( cbind( angle, osy, ody ) )
		
		names( ret ) <- c( "angle", "os", "od" )
		
		ret
	}


start.a <- Sys.time( )
s1 <- spectralis.pdf.to.thicknesses( "visitor/LI01274671_Beispiel.pdf" )
stop.a <- Sys.time( )
stop.a - start.a

start.b <- Sys.time( )
s2 <- xtrct.plot.from.pdf( fname = "visitor/LI01274671_Beispiel.pdf", doc.type = "spectralis" )
stop.b <- Sys.time( )
stop.b - start.b

subplot(
	plot_ly( s1, x = ~ angle, y = ~ os, type = "scatter", mode = "line", name = "OS" ),
	plot_ly( s1, x = ~ angle, y = ~ od, type = "scatter", mode = "line", name = "OD" ),
	plot_ly( s2, x = ~ angle, y = ~ os, type = "scatter", mode = "line", name = "OS" ),
	plot_ly( s2, x = ~ angle, y = ~ od, type = "scatter", mode = "line", name = "OD" ),
	nrows = 2
)
