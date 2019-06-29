rm( list = ls( ) )
# 
# # extracts RNFLT curves from Spectralis printouts
# require( "pdftools" )
# require( "png" )
# 
# # opens a pdf file and renders it with dpi dots per inch into a matrix of color channels
# pdf2mat <-
# 	function( filename, dpi = 312 ) {
# 		# maybe the following works easier somehow?
# 		readPNG( writePNG( pdf_render_page( filename, dpi = dpi ) ) )
# 	}
# 
# # xleft is 280 for OD and 1530 for OS
# get.thickness.plot <-
# 	function( xleft, ybottom = 2265, height = 257, mat ) {
# 		mat[ ( ybottom - height ) : ybottom, xleft : ( xleft + 768 ), ]
# 	}
# 
# # takes the thickness plot as 4-D matrix and extracts
# # the median of the black line, column by column;
# # mat needs to contain the precise image patch with the plot,
# # and the y-coordinates need to be (0, 300)
# matrix2thickness <-
# 	function( mat ) {
# 		# black pixels: all 3 color channels are 0:
# 		m <- mat[,,1] == 0 & mat[,,2] == 0 & mat[,,3] == 0
# 		nr <- nrow( m )
# 		300 * ( nr -  apply( m, 2, function( v ) median( which( v ) ) ) ) / nr
# 	}
# 
# extract.thickness <-
# 	function( xleft, mat ) {
# 		matrix2thickness( get.thickness.plot( xleft, mat = mat ) )
# 	}
# 
# # reads a pdf file of the standard Spectralis circle scan printout
# # and extracts thicknesses of OS and OD from the thickness curves
# #
# # filename: file name of the pdf file
# spectralis.pdf.to.thicknesses <-
# 	function( filename ) {
# 
# 		mat <- pdf2mat( filename )
# 
# 		os  <- extract.thickness(  280, mat = mat )
# 		od  <- extract.thickness( 1530, mat = mat )
# 
# 		# the OD thickness plot is one pixel "shorter":
# 
# 		od[ 1 ] <- od[ 768 ]
# 
# 		angle <- seq( 0, 360, length = length( os ) )
# 
# 		as.data.frame( cbind( angle, os, od ) )
# 	}

require( "plotly" )

source( "read_spectralis_pdfs.r" )
source( "pdf.R" )

start <- Sys.time( )
s1 <- spectralis.pdf.to.thicknesses( "visitor/LI01274671_Beispiel.pdf" )
Sys.time( ) - start

start <- Sys.time( )
s <- xtrct.plot.from.pdf( fname = "visitor/LI01274671_Beispiel.pdf", doc.type = "spectralis" )
Sys.time( ) - start

subplot(
	subplot(
		plot_ly( s, x = ~ angle, y = ~ os, type = "scatter", mode = "line", name = "OS" ),
		plot_ly( s, x = ~ angle, y = ~ od, type = "scatter", mode = "line", name = "OD" ),
		plot_ly( s1, x = ~ angle, y = ~ os, type = "scatter", mode = "line", name = "OS" ),
		plot_ly( s1, x = ~ angle, y = ~ od, type = "scatter", mode = "line", name = "OD" ),
		nrows = 2
	),
	subplot(
		plot_ly(   s, x = ~ angle, y = ~ os, type = "scatter", mode = "line", name = "OS" ) %>%
			add_trace( s, x = ~ angle, y = ~ od, type = "scatter", mode = "line", name = "OD" ),
		plot_ly(   s, x = ~ angle, y = ~ os - od, type = "scatter", mode = "line", name = "OS - OD" ) %>%
			add_trace( s, x = ~ angle, y = ~ abs( os - od ), type = "scatter", mode = "line", name = "| OS - OD |" ),
		nrows = 2
	),
	nrows = 2
)

start <- Sys.time( )
s <- xtrct.plot.from.pdf( fname = "visitor/LI02227755_Beispiel.pdf", doc.type = "spectralis" )
Sys.time( ) - start

subplot(
	plot_ly(   s, x = ~ angle, y = ~ os, type = "scatter", mode = "line", name = "OS" ) %>%
		add_trace( s, x = ~ angle, y = ~ od, type = "scatter", mode = "line", name = "OD" ),
	plot_ly(   s, x = ~ angle, y = ~ os - od, type = "scatter", mode = "line", name = "OS - OD", fill = "tozeroy" ) %>%
		add_trace( s, x = ~ angle, y = ~ abs( os - od ), type = "scatter", mode = "line", name = "| OS - OD |", fill = "tozeroy" ),
	nrows = 2
)

