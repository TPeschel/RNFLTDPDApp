rm( list = ls( ) )

library( "lubridate" )

library( "plotly" )

source( "pdf.R" )

( path.pdfs <- "visitor/allpdfs/elena/" )

( filenames.pdfs <- get.names.of.all.pdfs.in.directory( path.pdfs ) )

( headers.pdfs <- extract.header( filenames.pdfs, path.pdfs ) )

( headers.pdfs <- headers.pdfs[ ! is.na( headers.pdfs$header ), ] )

( different.pdf.types <- as.vector( unique( headers.pdfs$header ) ) )

( useful.pdfs <-
	headers.pdfs[ headers.pdfs[[ "header" ]] %in% c(
		"GlaucomaOverview", #configured
		"MinimumRimWidth&RNFLAnalysisSingleExamReport",
		"RNFL&AsymmetryAnalysisSingleExamReport", #configured
		"RNFLSingleExamReportOU", #congigured
		"RNFLChangeReport,AllFollow-Ups",
		"RNFLChangeReport,RecentFollow-Ups" 
	), ] )[ , 2 ]

( content.pdfs <- Reduce( 
	rbind.data.frame,
	apply( 
		useful.pdfs,
		1,
		function( pdf. ) {
			get.measurement.info.from.pdf( 
				pdf_text( 
					paste0( pdf.[[ "path" ]], pdf.[[ "fname" ]] ) ), 
				pdf.[[ "path" ]], 
				pdf.[[ "fname" ]]
			)
		}
	)
) )

#######################################################################################################################################
#
# Elena mit plotly als Beispiel
#
#######################################################################################################################################


( patient <- useful.pdfs[ grep( "RNFLT_Elena_1980.pdf", useful.pdfs$fname ), ] )
( patient.fn <- paste0( patient$path, patient$fname ) )
( patient.dat <- get.measurement.info.from.pdf( pdf_text( patient.fn ), patient$path, patient$fname ) )
( patient.pdf.gfx <- render.pdf( pdf.filename = patient.fn, dpi = 1000 ) )

os.graph.rgb <- xtrct.graph( patient.pdf.gfx, pdf.types$RNFLSingleExamReportOU$PLOTS$RNFLT.OS$GRAPH.RECT.SCAN )
od.graph.rgb <- xtrct.graph( patient.pdf.gfx, pdf.types$RNFLSingleExamReportOU$PLOTS$RNFLT.OD$GRAPH.RECT.SCAN )

par( mfrow = c( 2, 1 ) )
plot( as.raster( os.graph.rgb[ dim( os.graph.rgb )[ 1 ] : 1, , ], max = 255 ) )
plot( as.raster( od.graph.rgb[ dim( od.graph.rgb )[ 1 ] : 1, , ], max = 255 ) )
par( mfrow = c( 1, 1 ) )

od.graph.rgb.grad <- compute.gradient( od.graph.rgb, improve = T )
os.graph.rgb.grad <- compute.gradient( os.graph.rgb, improve = T )

par( mfrow = c( 2, 2 ) )
plot( as.raster( od.graph.rgb.grad ) )
plot( as.raster( os.graph.rgb.grad ) )

os.graph.rgb.grad <- remove.horizontal.lines( os.graph.rgb.grad )
od.graph.rgb.grad <- remove.horizontal.lines( od.graph.rgb.grad )

plot( as.raster( od.graph.rgb.grad ) )
plot( as.raster( os.graph.rgb.grad ) )
par( mfrow = c( 1, 1 ) )

( od.df <- xtrct.percentiles(
	od.graph.rgb.grad,
	pdf.types$RNFLSingleExamReportOU$PLOTS$RNFLT.OD
) )

( os.df <- xtrct.percentiles(
	os.graph.rgb.grad,
	pdf.types$RNFLSingleExamReportOU$PLOTS$RNFLT.OS
) )

d.od <- patient.dat[ patient.dat$OCULUS == "OD", ]
d.os <- patient.dat[ patient.dat$OCULUS == "OS", ]

p.od <- plot_ly( data = od.df$ALL, x = ~ angle )

p.od <- add_lines( p.od, y = ~ p95.meas, fill = "tozeroy", fillcolor = "rgba( 200, 255, 150, .25 )", line = list( color = "#C8FF96" ), name = "OD 95% meas" )
p.od <- add_lines( p.od, y = ~ p05.meas, fill = "tozeroy", fillcolor = "rgba( 255, 255, 150, .25 )", line = list( color = "#FFFF96" ), name = "OD 05% meas" )
p.od <- add_lines( p.od, y = ~ p01.meas, fill = "tozeroy", fillcolor = "rgba( 255, 150, 140, .25 )", line = list( color = "#FF968C" ), name = "OD 01% meas" )

p.od <- add_lines( p.od, y = ~ p95.imp, line = list( color = "#FF0000" ), name = "OD 95% imp" )
p.od <- add_lines( p.od, y = ~ p05.imp, line = list( color = "#0000FF" ), name = "OD 55% imp" )
p.od <- add_lines( p.od, y = ~ p01.imp, line = list( color = "#00FFFF" ), name = "OD 01% imp" )
p.od <- layout( p.od, xaxis = list( dtick = 45 ), yaxis = list( dtick = 60 ), title = paste0( d.od$MEASUREMENT.TYPE, "  OD - OS  AGE: ", d.od$AGE, " SEX: ", d.od$SEX ) )

p.os <- plot_ly( data = os.df$ALL, x = ~ angle )

p.os <- add_lines( p.os, y = ~ p95.meas, fill = "tozeroy", fillcolor = "rgba( 200, 255, 150, .25 )", line = list( color = "#C8FF96" ), name = "OS 95% meas" )
p.os <- add_lines( p.os, y = ~ p05.meas, fill = "tozeroy", fillcolor = "rgba( 255, 255, 150, .25 )", line = list( color = "#FFFF96" ), name = "OS 05% meas" )
p.os <- add_lines( p.os, y = ~ p01.meas, fill = "tozeroy", fillcolor = "rgba( 255, 150, 140, .25 )", line = list( color = "#FF968C" ), name = "OS 01% meas" )

p.os <- add_lines( p.os, y = ~ p95.imp, line = list( color = "#FF0000" ), name = "OS 95% imp" )
p.os <- add_lines( p.os, y = ~ p05.imp, line = list( color = "#0000FF" ), name = "OS 05% imp" )
p.os <- add_lines( p.os, y = ~ p01.imp, line = list( color = "#00FFFF" ), name = "OS 01% imp" )
p.os <- layout( p.os, xaxis = list( dtick = 45 ), yaxis = list( dtick = 60 ) )

subplot( p.od, p.os, nrows = 2 )

diff.df <- data.frame(

	angle  = os.df$X768$angle,
	diff95 = od.df$X768$r95.meas - os.df$X768$r95.meas,
	diff05 = od.df$X768$r05.meas - os.df$X768$r05.meas,
	diff01 = od.df$X768$r01.meas - os.df$X768$r01.meas
)

p.diff <- plot_ly( data = diff.df, x = ~ angle )

p.diff <- add_lines( p.diff, y = ~ diff95, line = list( color = "#C8FF96" ), name = "OD - OS 95% meas" )
p.diff <- add_lines( p.diff, y = ~ diff05, line = list( color = "#FFFF96" ), name = "OD - OS 05% meas" )
p.diff <- add_lines( p.diff, y = ~ diff01, line = list( color = "#FF968C" ), name = "OD - OS 01% meas" )

#p.oc <- layout( p.oc, xaxis = list( dtick = 45 ), yaxis = list( dtick = 60 ) )

p.diff


p.oc <- plot_ly( data = od.df$ALL, x = ~ angle )

p.oc <- add_lines( p.oc, y = ~ p95.meas, line = list( color = "#C8FF96" ), name = "OD 95% meas" )
p.oc <- add_lines( p.oc, y = ~ p05.meas, line = list( color = "#FFFF96" ), name = "OD 05% meas" )
p.oc <- add_lines( p.oc, y = ~ p01.meas, line = list( color = "#FF968C" ), name = "OD 01% meas" )

p.oc <- add_lines( p.oc, y = ~ p95.imp, line = list( color = "#FF0000" ), name = "OD 95% imp" )
p.oc <- add_lines( p.oc, y = ~ p05.imp, line = list( color = "#0000FF" ), name = "OD 55% imp" )
p.oc <- add_lines( p.oc, y = ~ p01.imp, line = list( color = "#00FFFF" ), name = "OD 01% imp" )
p.oc <- layout( p.oc, xaxis = list( dtick = 45 ), yaxis = list( dtick = 60 ) )


p.oc <- add_lines( p.oc, data = os.df$ALL, x = ~ angle, y = ~ p95.meas, line = list( color = "#C8FF96" ), name = "OS 95% meas" )
p.oc <- add_lines( p.oc, data = os.df$ALL, x = ~ angle, y = ~ p05.meas, line = list( color = "#FFFF96" ), name = "OS 05% meas" )
p.oc <- add_lines( p.oc, data = os.df$ALL, x = ~ angle, y = ~ p01.meas, line = list( color = "#FF968C" ), name = "OS 01% meas" )

p.oc <- add_lines( p.oc, data = os.df$ALL, x = ~ angle, y = ~ p95.imp, line = list( color = "#FF0000" ), name = "OS 95% imp" )
p.oc <- add_lines( p.oc, data = os.df$ALL, x = ~ angle, y = ~ p05.imp, line = list( color = "#0000FF" ), name = "OS 05% imp" )
p.oc <- add_lines( p.oc, data = os.df$ALL, x = ~ angle, y = ~ p01.imp, line = list( color = "#00FFFF" ), name = "OS 01% imp" )
p.oc <- layout( p.oc, xaxis = list( dtick = 45 ), yaxis = list( dtick = 60 ) )

p.oc


#######################################################################################################################################

#######################################################################################################################################

#######################################################################################################################################
#
# analyse all elenas
#
#######################################################################################################################################

d <- analyse.all.pdfs( useful.pdfs )

lapply( d, function( e ) t( e$PATIENT.DAT ) )

par( mfrow = c( 1, 2 ) )

# hier pdf fuer roten plot auswaehlen d$'1' - d$'5' 
e1 <- d$`2`

# zeige patientendaten an
e1$PATIENT.DAT

# plot des pdfs und Roh (ALL) oder skalierte (X768) Daten auswaehlen
o1 <- e1$DATA$RNFLT.OD$X768
plot(  o1$angle, o1$r95.meas, type = "l", col = "red", ylim = c( 0, 300 ) )
lines( o1$angle, o1$r05.meas, col = "red" )
lines( o1$angle, o1$r01.meas, col = "red" )

# das gleiche wie oben fuer den blauen Plot
e2 <- d$`2`
e2$PATIENT.DAT
o2 <- e2$DATA$RNFLT.OS$X768

lines( o2$angle, o2$r95.meas, col = "blue" )
lines( o2$angle, o2$r05.meas, col = "blue" )
lines( o2$angle, o2$r01.meas, col = "blue" )
title( paste( "red: ", e1$PATIENT.DAT$OCULUS[ 1 ], e1$PATIENT.DAT$AGE[ 1 ], "y  blue:", e2$PATIENT.DAT$OCULUS[ 2 ], e2$PATIENT.DAT$AGE[ 2 ], "y" ), outer = F )

d.diff <- o2$r95.meas - o1$r95.meas
plot( o1$angle, d.diff, type = "l", col = "red" )
d.diff <- o2$r05.meas - o1$r05.meas
lines( o1$angle, d.diff, type = "l", col = "green" )
d.diff <- o2$r01.meas - o1$r01.meas
lines( o1$angle, d.diff, type = "l", col = "blue" )

par( mfrow = c( 1, 1 ) )
