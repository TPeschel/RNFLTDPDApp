# extracts RNFLT curves from Spectralis printouts

require(pdftools)
require(png)

spectralis.pdf.to.thicknesses <- function(filename)
# reads a pdf file of the standard Spectralis circle scan printout
# and extracts thicknesses of OS and OD from the thickness curves
#
# filename: file name of the pdf file
{
	pdf2mat <- function(filename, dpi=312)
	# opens a pdf file and renders it with dpi dots per inch into a matrix of color channels
	{
		# maybe the following works easier somehow?
		readPNG(writePNG(pdf_render_page(filename, dpi=dpi)))
	}
	
	mat = pdf2mat(filename)

	get.thickness.plot <- function(xleft, ybottom=2265, height=257, m=mat)
	# xleft is 280 for OD and 1530 for OS
	{
		m[(ybottom-height):ybottom, xleft:(xleft+768), ]
	}

	matrix2thickness <- function(mat)
	# takes the thickness plot as 4-D matrix and extracts 
	# the median of the black line, column by column;
	# mat needs to contain the precise image patch with the plot,
	# and the y-coordinates need to be (0, 300)
	{
		# black pixels: all 3 color channels are 0:
		m = mat[,,1] == 0 & mat[,,2] == 0 & mat[,,3]==0
		nr = nrow(m)
		300 * (nr -  apply(m, 2, function(v) median(which(v))))/nr
	}
	
	extract.thickness <- function(xleft)
		matrix2thickness(get.thickness.plot(xleft))
	thick.os = extract.thickness(280)
	thick.od = extract.thickness(1530)
	# the OD thickness plot is one pixel "shorter":
	thick.od[1] <- thick.od[768]
	angle = seq(0, 360, length=length(thick.os))
	cbind(angle, thick.os, thick.od)
}

spectralis.pdf.to.thicknesses( "visitor/LI01274671_Beispiel.pdf" )

#### this was used to determine left eye location:
if( ! exists( "aa" ) ) {
	aa = pdf2mat( "visitor/LI01274671_Beispiel.pdf" )
}
?pdf2mat
plot(0:1, 0:1, type='n')
rasterImage(aa[1900:2200, 201:1100, ], 0, 0, 1, 1)
rasterImage(aa, 0, 0, 1, 1)
~ l = locator(1)
~ print(l)

~ xnew = round(dim(aa)[2] * l$x)
~ ynew = round(dim(aa)[1] * (1-l$y))

~ plt <- function(xnew, ynew, height=300)
~ 	rasterImage(aa[(ynew-height):ynew, xnew:(xnew+768), ], 0, 0, 1, 1)

~ plt(xnew, ynew)

results: lower left pixel of left eye: 280,2265; width was fixed to 768 by resolution; height: 257
