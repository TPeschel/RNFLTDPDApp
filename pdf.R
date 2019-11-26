###
# PDF 
###

require( "pdftools" )

LIST <-
	function( ..., pre = NULL, post = NULL ) {
		
		v <- as.list( ... )
		
		names( v ) <- paste0( pre, v, post )
		
		v
	}
# 
# b <- 8.27 * 2.54
# h <-11.69 * 2.54
# b. <- 8.27 * 312
# h. <-11.69 * 312
# 
# pdf.type.default <-
# 	list(
# 		doc.type       = "default",
# 		dpi            = 312,
# 		height         = 257
# 		width          = 768
# 	)

# add.pdf.type <-
# 	function(
# 		doc.type       = "default",
# 		dpi            = 312, 
# 		od.xleft       = 280,
# 		os.xleft       = 1530,
# 		width          = 768,
# 		bottom         = 2265,
# 		height         = 257,
# 		plot.height    = 300
# 	) {
# 		
# 		if( ! exists( "pdf.types" ) ) {
# 			
# 			pdf.types <<- NULL
# 		}
# 		
# 		pdf.types[[ doc.type ]] <<- 
# 			list( 
# 				doc.type       = doc.type,
# 				dpi            = dpi,
# 				od.xleft       = od.xleft,
# 				os.xleft       = os.xleft,
# 				width          = width,
# 				bottom         = bottom,
# 				height         = height,
# 				plot.height    = plot.height
# 			)
# 	}
# 
# xtrct.plot.from.pdf <-
# 	function( fname, doc.type = "default" ) {
# 		
# 		##########################################################################################################
# 		# some functions
# 		##########################################################################################################
# 		graph          <- function( a, d, o ) a[ ( d [[ "bottom" ]] - d [[ "height" ]] ) : d [[ "bottom" ]], d [[ paste0( o, ".xleft" ) ]] : ( d [[ paste0( o, ".xleft" ) ]] + ( d [[ "width" ]] - 1 ) ), ]
# 		
# 		mask.black.pix <- function( o ) o[ , , 1 ] == 0 & o[ , , 2 ] == 0 & o[ , , 3 ] == 0
# 		
# 		pos.of.blacks  <- function( blcks ) median( which( blcks ) )
# 		
# 		get.y          <- function( doc, blck.px ) { nrows = nrow( blck.px ); doc [[ "plot.height" ]] * ( nrows - apply( blck.px, 2, pos.of.blacks ) ) / nrows - 0 }
# 
# 		thcknss        <- function( content, doc.info, oculus ) {
# 			
# 			g <- graph( content, doc.info, oculus )
# 			
# 			m <- mask.black.pix( g )
# 			
# 			y <- get.y( doc.info, m )
# 			
# 			#if( oculus == "od" && doc.type %in% c( "default", "spectralis" ) ) {
# 			if( doc.type %in% c( "default", "spectralis" ) ) {
# 					
# 				print( "impute one value" )
# 				
# 				y[ 1 ] <- y[ 768 ]
# 			}
# 			
# 			y
# 		}
# 		
# 		##########################################################################################################
# 		# use the functions
# 		##########################################################################################################
# 		
# 		doc.info     <- pdf.types[[ doc.type ]]
# 		
# 		content      <- aperm( pdf_render_page( fname, dpi = doc.info [[ "dpi" ]] ), c( 3, 2, 1 ) )
# 		
# 		osy          <- thcknss( content, doc.info, "os" )
# 		
# 		ody          <- thcknss( content, doc.info, "od" )
# 		
# 		angle        <- seq( 0, 360, length = length( osy ) )
# 		
# 		ret          <- as.data.frame( cbind( angle, ody, osy ) )
# 		
# 		names( ret ) <- c( "angle", "od", "os" )
# 		
# 		attr( ret, "ID" ) <- fname
# 		
# 		ret
# 	}
# 
# xtrct.text.from.pdf <-
# 	function( fname, doc.type = "default" ) {
# 		
# 		txt <- pdf_text( fname )
# 		
# 		if( doc.type %in% c( "default", "spectralis" ) ) {
# 			
# 			list(
# 				birth = b<-as.Date( stringr::str_extract( stringr::str_extract( txt, "DOB:.*[0-9]{4}" ),     "[0-9].*" ), format = "%d.%b.%Y" ),
# 				exam  = e<-as.Date( stringr::str_extract( stringr::str_extract( txt, "Exam\\.:.*[0-9]{4}" ), "[0-9].*" ), format = "%d.%b.%Y" ),
# 				age   = round( as.double( difftime( e, b, "days" ) ) / 365.25, 1 ),
# 				sex   = c( "male", "female" )[ match( stringr::str_extract( stringr::str_extract( txt, "Sex:.*[FM]" ), "[FM]" ), c( "M", "F" ) ) ],
# 				fname = fname,
# 				id    = stringr::str_extract( stringr::str_extract( txt, "Patient ID:.*LI[0-9]{7}[0-9Xx]" ), "LI.*" )
# 			)
# 		}
# 		else if( doc.type %in% c( "glaucoma overview" ) ) {
# 			
# 			list(
# 				birth = b<-as.Date( stringr::str_extract( stringr::str_extract( txt, "DOB:.*[0-9]{4}" ),     "[0-9].*" ), format = "%d.%b.%Y" ),
# 				exam  = e<-as.Date( stringr::str_extract( stringr::str_extract( txt, "Exam\\.:.*[0-9]{4}" ), "[0-9].*" ), format = "%d.%b.%Y" ),
# 				age   = round( as.double( difftime( e, b, "days" ) ) / 365.25, 1 ),
# 				sex   = c( "male", "female" )[ match( stringr::str_extract( stringr::str_extract( txt, "Sex:.*[FM]" ), "[FM]" ), c( "M", "F" ) ) ],
# 				fname = fname,
# 				id    = stringr::str_extract( stringr::str_extract( txt, "Patient ID:.*LI[0-9]{7}[0-9Xx]" ), "LI.*" )
# 			)
# 		}
# 		
# 	}
# 
# 
# add.pdf.type( )
# 
# add.pdf.type( "spectralis", 312, 280, 1530, 768, 2265, 257, 300 )
# 
# pdf.types

get.names.of.all.pdfs.in.directory <- function( path = "." ) {
	
	dir( path = path, pattern = ".pdf$" )
}
#get.names.of.all.pdfs.in.directory("visitor/allpdfs/")
extract.header <- function( file.names, path = getwd( ) ) {
	
	Reduce( 
		rbind,
		lapply(
			LIST( file.names ),
			function( fname ) {
				
				txt <- pdf_text( paste0( path, fname ) )
				
				data.frame(
					path   = path,
					fname  = fname,
					header = gsub( "[ \\\n]", "", stringr::str_extract( txt, ".*\n" ) )
				)
			}
		)
	)
}

xtrct.lines <- function( text ) {
	
	lines <- stringr::str_extract_all( text, ".*\n" )
	
	lines <-
		lapply(
			lines[[ 1 ]],
			function( l ) {
				
				s <- strsplit( l, " +" )
				s <- s[[ 1 ]][ 0 < nchar( s[[ 1 ]] ) ]
				sl <- s[ length( s ) ]
				s[ length( s ) ] <- substr( sl, 1, nchar( sl ) - 1 )
				s
			}
		)
	lines
}

interprete.date.string <- function( text ) {

	d <- lubridate::dmy( text, locale = "de_DE.utf8" )
	
	if( is.na( d ) ) {
	
		d<-lubridate::dmy( text, locale = "it_IT.utf8" )
	} else if( is.na( d ) ) {
		
		d<-lubridate::dmy( text, locale = "en_EN.utf8" )
	}
	
	d
}

render.pdf <- function( pdf.filename, dpi = 1000 ) {
	( patient.pdf.gfx <- aperm(
		pdf_render_page(
			pdf.filename,
			dpi = dpi
		),
		c( 3, 2, 1 )
	) )[ , , 1 : 3 ]
	
}

xtrct.graph <- function( pdf.gfx, graph.rect ) {
	
	pdf.gfx[ graph.rect$Y.MIN : graph.rect$Y.MAX, graph.rect$X.MIN : graph.rect$X.MAX, 1 : 3 ]
}

compute.gradient <- function( graph.rgb, improve = T ) {
	
	y.len <- dim( graph.rgb )[ 1 ]
	x.len <- dim( graph.rgb )[ 2 ]
	
	gfx.rgb  <- array( data = as.numeric( graph.rgb ), dim = dim( graph.rgb ) )
	rgb.grad <- ( gfx.rgb[ 3 : y.len, , ] - gfx.rgb[ 1 : ( y.len - 2 ), , ] ) / 510.
	
	if( improve == T ) {
		
		rgb.grad <- ifelse( 0 < abs( rgb.grad ), 1, 0 )
	}
	
	rgb.grad
}

remove.horizontal.lines <- function( rgb.grad ) {
	
	y.len <- dim( rgb.grad )[ 1 ]
	x.len <- dim( rgb.grad )[ 2 ]

	for( i in 1 : x.len ) {
		
		j <- ( ( rgb.grad[ , i, 1 ] == rgb.grad[ , i, 2 ] ) & ( rgb.grad[ , i, 1 ] == rgb.grad[ , i, 3 ] ) )
		
		rgb.grad[ j, i, ] <- c( 0, 0, 0 )
	}
	
	rgb.grad
}

get.measurement.info.from.pdf <- function( text, pdf.path = "", pdf.name = "" ) {
	
	#delete!
	# text = pdf_text( patient.fn )
	# pdf.path = patient$path
	# pdf.name = patient$fname
	
	lines <- xtrct.lines( text )
	
	pdf.type <- Reduce( paste, lines[[ 1 ]] )
	
	lst. <- data.frame(
		PDF.TYPE          = pdf.type,
		PDF.NAME          = pdf.name,
		PDF.PATH          = pdf.path,
		MEASUREMENT.TYPE  = Reduce( paste, lines[[ 2 ]] ),
		DOE.ORIG          = doe.orig<-lines[[ 4 ]][ grep( "Exam.:", lines[[ 4 ]] ) + 1 ],
		DOE               = doe<-interprete.date.string( doe.orig ), #lubridate::dmy( lines[[ 4 ]][ 5 ], locale = "it_IT.utf8" ),
		OCULUS            = "",
		PATIENT.NAME      = Reduce( paste, lines[[ 3 ]][ ( grep( "Patient:", lines[[ 3 ]] ) + 1 ) : ( grep( "DOB:", lines[[ 3 ]] ) - 1 ) ] ),
		DOB.ORIG          = dob.orig<-lines[[ 3 ]][ grep( "DOB:", lines[[ 3 ]] ) + 1 ],
		DOB               = dob<-interprete.date.string( dob.orig ), #lubridate::dmy( lines[[ 3 ]][ 5 ], locale = "it_IT.utf8" ),
		AGE               = round( as.numeric( doe - dob ) / 365.25, 1 ),
		SEX               = c( "MALE", "FEMALE") [ match( lines[[ 3 ]][ grep( "Sex:", lines[[ 3 ]] ) + 1 ], c( "M", "F" ) ) ]
	)
	
	lst <- NULL
	
	if( pdf.type == "RNFL Single Exam Report OU" ) {
		
		lst <- rbind( lst., lst. )
		lst [[ "OCULUS" ]] <- c( "OD", "OS" )
	}
	else if( pdf.type == "RNFL Change Report, All Follow-Ups" ) {
		
		lst <- lst.
		lst [[ "OCULUS" ]] <- lines[[ 4 ]][ 4 ]
		lst [[ "DOE.ORIG" ]] <- lines[[ 32 ]][ 3 ]
		lst [[ "DOE" ]] <- interprete.date.string( lines[[ 32 ]][ 3 ] )
		lst [[ "AGE" ]] <- round( as.numeric( lst [[ "DOE" ]] - lst [[ "DOB" ]] ) / 365.25, 1 ) 
	}
	else {
		lst <- lst.
		lst [[ "OCULUS" ]] <- lines[[ 4 ]][ 6 ]
	}
	
	lst
}

rectangle <- function( x.min, y.min, x.max, y.max ) {
	
	list( 
		X.MIN = x.min, 
		Y.MIN = y.min, 
		X.MAX = x.max,
		Y.MAX = y.max
	)
}

plot.properties <- function( 
	name, 
	oculus, 
	graph.rect.pdf, 
	graph.rect.scan = graph.rect.pdf, 
	graph.rect.real = rectangle( 0, 0, 360, 300 ),
	array.size      = 768 ) {
	
	list( 
		NAME            = name, 
		OCULUS          = oculus, 
		GRAPH.RECT.PDF  = graph.rect.pdf, 
		GRAPH.RECT.SCAN = graph.rect.scan, 
		GRAPH.RECT.REAL = graph.rect.real, 
		ARRAY.SIZE      = array.size
	)
}

pdf.type <- function( doc.type, dpi = 1000, ... ) {
	
	list(
		DOC.TYPE = doc.type, 
		DPI = dpi, 
		PLOTS = list( ... )
	)
}

scale.dpi <- function( dpi.dest = 312, pdf.type ) {
	
	f <- dpi.dest / pdf.type$DPI
	
	pdf.type$DPI <- dpi.dest
	
	for( i in 1 : length( pdf.type$PLOTS ) ) {
		
		pdf.type$PLOTS[[ i ]]$GRAPH.RECT.PDF$X.MIN <- round( pdf.type$PLOTS[[ i ]]$GRAPH.RECT.PDF$X.MIN * f )
		pdf.type$PLOTS[[ i ]]$GRAPH.RECT.PDF$Y.MIN <- round( pdf.type$PLOTS[[ i ]]$GRAPH.RECT.PDF$Y.MIN * f )
		pdf.type$PLOTS[[ i ]]$GRAPH.RECT.PDF$X.MAX <- round( pdf.type$PLOTS[[ i ]]$GRAPH.RECT.PDF$X.MAX * f )
		pdf.type$PLOTS[[ i ]]$GRAPH.RECT.PDF$Y.MAX <- round( pdf.type$PLOTS[[ i ]]$GRAPH.RECT.PDF$Y.MAX * f )
	
		pdf.type$PLOTS[[ i ]]$GRAPH.RECT.SCAN$X.MIN <- round( pdf.type$PLOTS[[ i ]]$GRAPH.RECT.SCAN$X.MIN * f )
		pdf.type$PLOTS[[ i ]]$GRAPH.RECT.SCAN$Y.MIN <- round( pdf.type$PLOTS[[ i ]]$GRAPH.RECT.SCAN$Y.MIN * f )
		pdf.type$PLOTS[[ i ]]$GRAPH.RECT.SCAN$X.MAX <- round( pdf.type$PLOTS[[ i ]]$GRAPH.RECT.SCAN$X.MAX * f )
		pdf.type$PLOTS[[ i ]]$GRAPH.RECT.SCAN$Y.MAX <- round( pdf.type$PLOTS[[ i ]]$GRAPH.RECT.SCAN$Y.MAX * f )
	}
	
	pdf.type
}

add.pdf.type <- function( pdf.type, pdf.type.name = NULL ) {
	
	if( ! exists( "pdf.types" ) ) pdf.types <<- list( )
	
	if( is.null( pdf.type.name ) ) pdf.type.name <- as.character( length( pdf.types ) + 1 )

	pdf.types[[ pdf.type.name ]] <<- pdf.type
}

#rm( "pdf.types" )

# ( pp <- plot.properties(
# 	name            = "OCT",
# 	oculus          = "OS",
# 	graph.rect.pdf  = rectangle( 280, 2008, 1048, 2265 ),
# 	graph.rect.scan = rectangle( 282, 2010, 1046, 2262 ),
# 	graph.rect.real = rectangle( 0, 0, 360, 300 ),
# 	array.size      = 768
# ) )

# ( pdf.type.rnflt.312 <- pdf.type( 
# 	doc.type = "RNFLSingleExamReportOU",
# 	312,
# 	RNFLT.OD = plot.properties(
# 		"RNFLT 3.5mm",
# 		"OD",
# 		rectangle( 279, 2265, 1049, 2008 ),
# 		rectangle( 279+2, 2265-2, 1049-2, 2008+2 ),
# 		rectangle( 0, 0, 360, 300 ),
# 		768 
# 	),
# 	RNFLT.OS = plot.properties(
# 		"RNFLT 3.5mm",
# 		"OS",
# 		rectangle( 1530, 2265, 2300, 2008 ),
# 		rectangle( 1530+2, 2265-2, 2300-2, 2008+2 ),
# 		rectangle( 0, 0, 360, 300 ),
# 		768
# 	)
# ) )

# ( RNFLSingleExamReportOU <- pdf.type( 
# 	doc.type = "RNFLSingleExamReportOU",
# 	1000,
# 	RNFLT.OD = plot.properties(
# 		"RNFLT 3.5mm",
# 		"OD",
# 		rectangle( 895,      7261,      3363,      6437 ),
# 		rectangle( 895 + 10, 7261 - 10, 3363 - 10, 6437 + 10 ),
# 		rectangle( 0, 0, 360, 300 ),
# 		768 
# 	),
# 	RNFLT.OS = plot.properties(
# 		"RNFLT 3.5mm",
# 		"OS",
# 		rectangle( 4905,      7261,      7373,      6437 ),
# 		rectangle( 4905 + 10, 7261 - 10, 7373 - 10, 6437 + 10 ),
# 		rectangle( 0, 0, 360, 300 ),
# 		768
# 	)
# ) )

# "GlaucomaOverview", #configured
# "MinimumRimWidth&RNFLAnalysisSingleExamReport", #configured
# "RNFL&AsymmetryAnalysisSingleExamReport", #configured
# "RNFLSingleExamReportOU", #congigured
# "RNFLChangeReport,AllFollow-Ups",
# "RNFLChangeReport,RecentFollow-Ups" 

( type1 <- pdf.type( 
	doc.type = "GlaucomaOverview",
	1000,
	RNFLT = plot.properties(
		"RNFLT 3.5mm",
		"ANY",
		rectangle( 3780, 3357, 8048, 2703 ),
		rectangle( 3783, 3353, 8047, 2705 ),
		rectangle( 0, 0, 360, 300 ),
		768 
	),
	MIN.RIM.WIDTH = plot.properties(
		"MIN RIM WIDTH",
		"ANY",
		rectangle( 3780, 1879, 8048, 1224 ),
		rectangle( 3783, 1875, 8047, 1227 ),
		rectangle( 0, 0, 360, 300 ),
		768
	)
) )

( type2 <- pdf.type( 
	doc.type = "MinimumRimWidth&RNFLAnalysisSingleExamReport",
	1000,
	RIM.MIN.WIDTH = plot.properties(
		"RNFLT 3.5mm",
		"ANY",
		rectangle( 892, 8326, 3962, 7753 ),
		rectangle( 895, 8321, 3960, 7755 ),
		rectangle( 0, 0, 360, 300 ),
		768 
	),
	RNFLT = plot.properties(
		"RNFLT 3.5mm",
		"ANY",
		rectangle( 4305, 8326, 7375, 7753 ),
		rectangle( 4305, 8321, 7372, 7755 ),
		rectangle( 0, 0, 360, 300 ),
		768
	)
) )

( type3 <- pdf.type( 
	doc.type = "RNFL&AsymmetryAnalysisSingleExamReport",
	1000,
	RNFLT = plot.properties(
		"RNFLT 3.5mm",
		"ANY",
		rectangle( 917, 8028, 3962, 7229 ),
		rectangle( 920, 8023, 3960, 7232 ),
		rectangle( 0, 0, 360, 300 ),
		768 
	)
) )

( type4 <- pdf.type( 
	doc.type = "RNFLSingleExamReportOU",
	1000,
	RNFLT.OD = plot.properties(
		"RNFLT 3.5mm",
		"OD",
		rectangle( 895, 7260, 3363, 6437 ),
		rectangle( 897, 7256, 3362, 6440 ),
		rectangle( 0, 0, 360, 300 ),
		768 
	),
	RNFLT.OS = plot.properties(
		"RNFLT 3.5mm",
		"OS",
		rectangle( 4905, 7260, 7374, 6437 ),
		rectangle( 4905, 7256, 7370, 6440 ),
		rectangle( 0, 0, 360, 300 ),
		768
	)
) )

# "RNFLChangeReport,AllFollow-Ups",
( type5 <- pdf.type( 
	doc.type = "RNFLChangeReport,AllFollow-Ups",
	1000,
	RNFLT.BASELINE = plot.properties(
		"RNFLT 3.5mm",
		"ANY",
		rectangle( 1923, 3767, 4852, 2771 ),
		rectangle( 1926, 3763, 4852, 2773 ),
		rectangle( 0, 0, 360, 300 ),
		768 
	),
	RNFLT.ALL.FOLLOW.UP = plot.properties(
		"RNFLT 3.5mm",
		"ANY",
		rectangle( 1923, 7306, 4852, 6309 ),
		rectangle( 1926, 7301, 4852, 6312 ),
		rectangle( 0, 0, 360, 300 ),
		768
	)
) )
# "RNFLChangeReport,RecentFollow-Ups" 
( type6 <- pdf.type( 
	doc.type = "RNFLChangeReport,RecentFollow-Ups",
	1000,
	RNFLT.BASELINE = plot.properties(
		"RNFLT 3.5mm",
		"ANY",
		rectangle( 1923, 3767, 4852, 2771 ),
		rectangle( 1926, 3763, 4852, 2773 ),
		rectangle( 0, 0, 360, 300 ),
		768 
	),
	RNFLT.FOLLOW.UP.1 = plot.properties(
		"RNFLT 3.5mm",
		"ANY",
		rectangle( 1923, 7306, 4852, 6309 ),
		rectangle( 1926, 7301, 4852, 6312 ),
		rectangle( 0, 0, 360, 300 ),
		768
	)
) )


#( pdf.312.dpi <- scale.dpi( 312, pdf.type.rnflt.1000 ) )

#add.pdf.type( pdf.type.rnflt.312, "rnflt.312" )
add.pdf.type( type1, type1$DOC.TYPE )
add.pdf.type( type2, type2$DOC.TYPE )
add.pdf.type( type3, type3$DOC.TYPE )
add.pdf.type( type4, type4$DOC.TYPE )
add.pdf.type( type5, type5$DOC.TYPE )
add.pdf.type( type6, type6$DOC.TYPE )

pdf.types

#RNFLSingleExamReportOU.312DPI <- scale.dpi( 312, RNFLSingleExamReportOU )
#xtrct.plot.from.pdf( "visitor/RNFL_PDF/CP_OD_Glaucoma Overview.pdf" )
#xtrct.text.from.pdf( "visitor/RNFL_PDF/CP_OD_Glaucoma Overview.pdf" )

# content <- aperm( pdf_render_page( "visitor/RNFL_PDF/CP_OD_Glaucoma Overview.pdf", dpi = 312 ), c( 3, 2, 1 ) )
# content

# xtrct.perc <- function( rgb.grad, condition ) {
# 	
# 	l <- apply(
# 		rgb.grad[ floor( .01 * dim( rgb.grad )[ 1 ] ) : floor( 1. * dim( rgb.grad )[ 1 ] ), , ],
# 		2,
# 		function( d ) {
# 			median( which( condition( d ) ) )
# 		}
# 	)
# 	
# 	l <- l + floor( .01 * dim( rgb.grad )[ 1 ] - 1 )
# 	
# 	l[ 1 : 3 ] <- NA
# 	
# 	l[ ( length( l ) - 2 ) : length( l ) ] <- NA
# 	
# 	l
# }

xtrct.perc <- function( rgb.grad, condition ) {
	
	# rgb.grad = os.rgb.grad
	# condition = cond95
	# rect.pdf = pdf.type.rnflt.1000$PLOTS$RNFLT.OD$GRAPH.RECT.PDF
	# rect.scan = pdf.type.rnflt.1000$PLOTS$RNFLT.OD$GRAPH.RECT.SCAN
	
	apply(
		#		rgb.grad[ floor( .01 * dim( rgb.grad )[ 1 ] ) : floor( 1. * dim( rgb.grad )[ 1 ] ), , ],
		rgb.grad,
		2,
		function( d ) {
			median( which( condition( d ) ) )
		}
	)
}

cond95 <- function( d ) 0 <  d[ , 1 ] & 0 == d[ , 2 ] & 0 <  d[ , 3 ]
cond05 <- function( d ) 0 <  d[ , 1 ] & 0 == d[ , 2 ] & 0 == d[ , 3 ]
cond01 <- function( d ) 0 == d[ , 1 ] & 0 <  d[ , 2 ] & 0 <  d[ , 3 ]

find.missings <- function( perc ) {
	
	( perc.yna <- which( ! is.na( perc[ 1 : ( length( perc ) - 1 ) ] ) &   is.na( perc[ 2 : length( perc ) ] ) ) )
	( perc.nay <- which(   is.na( perc[ 1 : ( length( perc ) - 1 ) ] ) & ! is.na( perc[ 2 : length( perc ) ] ) ) )
	
	if( perc.nay[ 1 ] < perc.yna[ 1 ] ) {
		
		perc.nay <- perc.nay[ -1 ]
	}
	
	mlen <- min( c( length( perc.yna ), length( perc.nay ) ) )
	
	intrvl.perc <- data.frame( 
		yna = perc.yna[ 1 : mlen ], 
		nay = perc.nay[ 1 : mlen ] + 1 )
	
	intrvl.perc
}

interpolate.missings <- function( perc, missings ) {
	
	perc.interpol <- rep( NA, length( perc ) )
	
	for( i in 1 : nrow( missings ) ) {
		
		v0 <- perc[ missings$yna[ i ] ]
		v1 <- perc[ missings$nay[ i ] ]
		m  <- ( v1 - v0 ) / ( missings$nay[ i ] - missings$yna[ i ] )
		
		for( j in ( missings$yna[ i ] + 1 ) : ( missings$nay[ i ] - 1 ) ) {
			
			perc.interpol[ j ] <- v0 + m * ( j - missings$yna[ i ] )
		}
	}
	
	perc.interpol
}

interpolate.x.borders <- function( perc.m, perc.i, left.border, right.border ) {
	
	#delete!
	# perc.m <- p05
	# perc.i <- i05

	first.y <- min( which( ! is.na( perc.m ) ) )
	last.y  <- max( which( ! is.na( perc.m ) ) )
	rem.y   <- length( perc.m ) - last.y
	
	len = ( first.y - 1 ) + ( rem.y ) + left.border + right.border
	
	dy <- ( perc.m[ first.y ] - perc.m[ last.y ] ) / ( len + 1 )
	
	y <- perc.m[ last.y ] + dy * c( 1 : len )
	# lb lb lb NA NA 
	perc.m <- c( rep( NA, left.border ), perc.m, rep( NA, right.border ) )
#hier weiter!
      	
	# 1 2 3 4 5
	# length( perc.m )              
	p1 <- c( ) 
	if ( right.border + rem.y < length( y ) ) p1 <- y[ ( right.border + rem.y + 1 ) : length( y ) ]
	p2 <- c( )
	if( 0 < right.border + rem.y ) p2 <- y[ 1 : ( right.border + rem.y ) ]
	length( p1 )
	length( perc.i[ first.y : last.y ] )
	length( p2 )
	perc.i <- c( p1, perc.i[ first.y : last.y ], p2 )
	
	common <- perc.m
	common[ is.na( common ) ] <- perc.i[ is.na( common ) ]
	
	perc.r <- common[ 1 + ( ( 0 : 767 ) / 767 * ( length( common ) - 1 ) ) ]
	
	list( p = perc.m, i = perc.i, r = perc.r, a = 1 : 768 )
}

xtrct.percentiles <- function( rgb.grad, pdf.type.oc.plot ) {

	#delete!
	# rgb.grad = graph.rgb.grad
	# pdf.type.oc.plot = plot.type
	# rgb.grad = od.graph.rgb.grad
	# pdf.type.oc.plot = pdf.types$rnflt.1000$PLOTS$RNFLT.OD
	
	# 
	# dims.pdf <- c( 
	# 	pdf.type.oc.plot$GRAPH.RECT.PDF$X.MAX - pdf.type.oc.plot$GRAPH.RECT.PDF$X.MIN + 1,
	# 	pdf.type.oc.plot$GRAPH.RECT.PDF$Y.MIN - pdf.type.oc.plot$GRAPH.RECT.PDF$Y.MAX + 1,
	# 	3 
	# )
	# 
	# rgb.frame <- array( rep( NA, dims.pdf[ 1 ] * dims.pdf[ 2 ] * dims.pdf[ 3 ] ), dims.pdf )
	# rgb.col[ , ] <- c( NA, NA, NA )
	# rgb.row <- rgb.grad[ , 1, ]
	# rgb.row[ , ] <- c( NA, NA, NA )
	# 
	# r<-cbind( rgb.grad, array( )	c( 1 : 2469 ) )
	
	rect.pdf  <- pdf.type.oc.plot$GRAPH.RECT.PDF
	rect.scan <- pdf.type.oc.plot$GRAPH.RECT.SCAN
	rect.real <- pdf.type.oc.plot$GRAPH.RECT.REAL
	
	p95 <- xtrct.perc( rgb.grad, cond95 )
	p05 <- xtrct.perc( rgb.grad, cond05 )
	p01 <- xtrct.perc( rgb.grad, cond01 )
	
	m95 <- find.missings( p95 )
	m05 <- find.missings( p05 )
	m01 <- find.missings( p01 )
	
	i95 <- interpolate.missings( p95, m95 )
	i05 <- interpolate.missings( p05, m05 )
	i01 <- interpolate.missings( p01, m01 )
	
	dy <- ( rect.real$Y.MAX - rect.real$Y.MIN ) / ( rect.pdf$Y.MAX - rect.pdf$Y.MIN - 1 )
	dx <- ( rect.real$X.MAX - rect.real$X.MIN ) / ( rect.pdf$X.MAX - rect.pdf$X.MIN - 1 )
	
	left.border  <- rect.scan$X.MIN - rect.pdf$X.MIN
	right.border <- rect.pdf$X.MAX - rect.scan$X.MAX

	l <- interpolate.x.borders( p95, i95, left.border, right.border )
	p95 <- l$p
	i95 <- l$i
	r95 <- l$r
	l <- interpolate.x.borders( p05, i05, left.border, right.border )
	p05 <- l$p
	i05 <- l$i
	r05 <- l$r
	l <- interpolate.x.borders( p01, i01, left.border, right.border )
	p01 <- l$p
	i01 <- l$i
	r01 <- l$r
	# first.y <- min( which( ! is.na( p01 ) ) )
	# last.y  <- max( which( ! is.na( p01 ) ) )
	# rem.y   <- length( p01 ) - last.y
	# 
	# len   = ( first.y - 1 ) + ( rem.y ) + left.border + right.border
	# 
	# dy <- ( p01[ first.y ] - p01[ last.y ] ) / ( len + 1 )
	# 
	# y.01 <- p01[ last.y ] + dy * c( 1 : len )
	# 
	# p01 <- c( rep( NA, first.y - 1 ), p01, rep( NA, rem.y ) )
	# i01 <- c( y.01[ ( right.border + rem.y + 1 ) : length( y.01 ) ], i01[ first.y : last.y ], y.01[ 1 : ( left.border + first.y - 1 ) ] )
	# 
	
	list(
		ALL = data.frame(
			angle = rect.real$X.MIN + dx * ( 1 : length( p95 ) - 1 ),
			#angle = rect.real$X.MIN + dx * ( 1 : ( 1 + rect.pdf$X.MAX - rect.pdf$X.MIN ) ),
			
			p95.meas = rect.real$Y.MIN - dy * ( p95 - rect.scan$Y.MIN + rect.pdf$Y.MIN ),
			p05.meas = rect.real$Y.MIN - dy * ( p05 - rect.scan$Y.MIN + rect.pdf$Y.MIN ),
			p01.meas = rect.real$Y.MIN - dy * ( p01 - rect.scan$Y.MIN + rect.pdf$Y.MIN ),
			
			p95.imp  = rect.real$Y.MIN - dy * ( i95 - rect.scan$Y.MIN + rect.pdf$Y.MIN ),
			p05.imp  = rect.real$Y.MIN - dy * ( i05 - rect.scan$Y.MIN + rect.pdf$Y.MIN ),
			p01.imp  = rect.real$Y.MIN - dy * ( i01 - rect.scan$Y.MIN + rect.pdf$Y.MIN )
		),
		X768 = data.frame(
			angle    = ( 1 : 768 ) / 768 * 360,
			r95.meas = rect.real$Y.MIN - dy * ( r95 - rect.scan$Y.MIN + rect.pdf$Y.MIN ),
			r05.meas = rect.real$Y.MIN - dy * ( r05 - rect.scan$Y.MIN + rect.pdf$Y.MIN ),
			r01.meas = rect.real$Y.MIN - dy * ( r01 - rect.scan$Y.MIN + rect.pdf$Y.MIN )
		)
	)
}

analyse.all.pdfs <- function( pdfs, dpi = 1000 ) {
	
	apply(
		pdfs,#[ 1 : 2, ],
		1,
		function( patient ) {
			
			print( patient )
			
			pdf.type <- pdf.types[[ as.character( patient[[ "header" ]] ) ]]
			
			if( pdf.type$DPI != dpi ) {
				
				pdf.type <- scale.dpi( dpi.dest = dpi, pdf.type = pdf.type )
			}
			
			patient.fn <- paste0( patient[[ "path" ]], patient[[ "fname" ]] )
			
			patient.dat <- get.measurement.info.from.pdf( pdf_text( patient.fn ), patient[[ "path" ]], patient[[ "fname" ]] )
			
			patient.pdf.gfx <- render.pdf( pdf.filename = patient.fn, dpi = dpi )
			
			l <- lapply(
				pdf.type[[ "PLOTS" ]],
				function( plot.type ) {
					
					graph.rgb <- xtrct.graph( patient.pdf.gfx, plot.type[[ "GRAPH.RECT.SCAN" ]] )
					
					graph.rgb.grad <- compute.gradient( graph.rgb, improve = T )
					
					graph.rgb.grad <- remove.horizontal.lines( graph.rgb.grad )
					
					xtrct.percentiles( 
						graph.rgb.grad, 
						plot.type
					)
				}
			)
			list(
				DATA        = l,
				PATIENT.DAT = patient.dat
			)
		}
	)
}
