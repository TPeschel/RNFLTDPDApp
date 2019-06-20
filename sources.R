##### source code accompanying the article "Inter-ocular Circumpapillary Retinal Nerve Fiber Layer Thickness Asymmetry and its Relationship to Age and Scanning Radius"
#
# (C) 2019 Tobias Elze
#
#~     This program is free software: you can redistribute it and/or modify
#~     it under the terms of the GNU General Public License as published by
#~     the Free Software Foundation, either version 3 of the License, or
#~     (at your option) any later version.

#~     This program is distributed in the hope that it will be useful,
#~     but WITHOUT ANY WARRANTY; without even the implied warranty of
#~     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#~     GNU General Public License for more details.

#~     You should have received a copy of the GNU General Public License
#~     along with this program.  If not, see <https://www.gnu.org/licenses/>.

# Note: function plot.rnflt.diff.sectors.with.norms needs libraries plotrix and gamlss.dist

check.install <- function(p)
	if(!require(p, character.only=TRUE))
	{
		cat("\nR package", p,  "is needed in function plot.rnflt.diff.sectors.with.norms. Press ENTER to install it. This needs to be done only once.")
		invisible(readline())
		install.packages(p, repos="https://cran.wu.ac.at/")
		require(p)
		cat("\n\nPackage", p, "successfully installed.\n\n")
	}
	
check.install( "plotrix" )
check.install( "gamlss.dist" )

### make sure to place the csv files into the current working directory, or modify this code:
# if( ! exists("rnfltdiffnorms" ) )
# 	rnfltdiffnorms <- read.csv( "data/rnflt_difference_norms.csv" )
# 
# if( ! exists( "sector.abs.rnfltdiffnorms" ) )
# 	sector.abs.rnfltdiffnorms <- read.csv( "data/sector_abs_rnflt_difference_norms.csv", row.names = 1 )
# 
# if( ! exists( "visitor" ) )
# 	visitor <- read.csv( "data/rnfltdiff_example.csv" )
# 
# save( list = c( "rnfltdiffnorms", "sector.abs.rnfltdiffnorms", "visitor" ), file = "data/all.RData" )

load( "data/all.RData" )

sector.indices <- function(sector.label, n=768)
# return TRUE for all measurement locations that belong to the sector
# given by sector.label
#
# sector.label: one of "rnfltMeanT", "rnfltMeanTS", "rnfltMeanTI", 
#   "rnfltMeanN", "rnfltMeanNS", "rnfltMeanNI", "rnfltMeanG"
# n: number of A-scans, i.e. measurement locations on the circle
{
	len = round(n/8)
	sec = 1:len
	inds.true <- switch(sector.label,
		rnfltMeanT = c(sec, (n+1) - sec),
		rnfltMeanTS = sec + len,
		rnfltMeanNS = sec + 2*len,
		rnfltMeanN = (1:(2*len)) + 3*len,
		rnfltMeanNI = sec + 5*len,
		rnfltMeanTI = sec + 6*len,
		1:n)
	(1:n) %in% inds.true
}

calculate.normative.distribution <- function(age, radiusdiff=0, angles=seq(0, 360, length=768), smoothing=smooth.spline, norms = rnfltdiffnorms )
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

calculate.normative.distribution.sectors.absdiff <- function(
	age, 
	radiusdiff=0, 
	sectors=c("rnfltMeanG", "rnfltMeanT", "rnfltMeanTS", "rnfltMeanTI", "rnfltMeanN", "rnfltMeanNS", "rnfltMeanNI"), 
	norms = sector.abs.rnfltdiffnorms)
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
{
	mus = norms$mu.intercept + norms$mu.age*age + norms$mu.age2*age^2 + norms$mu.age3*age^3 + norms$mu.rad*radiusdiff + norms$mu.rad2*radiusdiff^2 + norms$mu.rad3*radiusdiff^3
	sigmas = exp(norms$sigma.intercept + norms$sigma.age*age + norms$sigma.age2*age^2 + norms$sigma.age3*age^3 + norms$sigma.rad*radiusdiff + norms$sigma.rad2*radiusdiff^2 + norms$sigma.rad3*radiusdiff^3)
	nus = norms$nu.intercept
	taus = norms$tau.intercept
	
	selected = row.names(norms) %in% sectors
	data.frame(mu=mus[selected], sigma=sigmas[selected], nu=nus[selected], tau=taus[selected], row.names=sectors)
}

plot.rnflt.diff.with.norms <- function(rnfltdiff, age, radiusdiff=0, cents = c( 0.01, 0.05, 0.5, 0.95, 0.99 ), smoothing=smooth.spline, ...)
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
{
	norms = calculate.normative.distribution( age = age, radiusdiff = radiusdiff, smoothing = smooth.spline )
#	qn = sapply(c(0.01, 0.05, 0.5, 0.95, 0.99), function(x) qnorm(x, norms$mu, norms$sigma))
	qn = sapply(cents, function(x) qnorm(x, norms$mu, norms$sigma))
	###
	# why r = 1.1*range(qn)
	###
	r = 1.1 * range(qn)
	angles=seq(0, 360, length=768)
	plot(
		angles,
		angles,
		type="n",
		xlab="Angle (degree)", 
		ylab=expression(paste("RNFLT OS-OD (", mu, "m)")), 
		ylim=r,
		xaxs="i", yaxs="i",
		...
	)
	rect(0, r[1], 360, r[2], col="palegreen", border=NA)
	xpoly = c(angles[1], angles, angles[768])
	polygon(xpoly, c(r[1], qn[,2], r[1]), col="khaki1", border=NA)
	polygon(xpoly, c(r[2], qn[,4], r[2]), col="khaki1", border=NA)
	polygon(xpoly, c(r[1], qn[,1], r[1]), col="indianred1", border=NA)
	polygon(xpoly, c(r[2], qn[,5], r[2]), col="indianred1", border=NA)
	
	abline(v=seq(0, 270, by=90)+45, lty="dotted", col="gray20")
	abline(v=c(90, 270), lty="dotted", col="gray20")
	plot.label <- function(xpos, l)
		text(xpos, r[1], l, pos=3)
	mapply(plot.label, c(22.5, 67.5, 112.5, 180, 360-112.5, 360-67.5, 360-22.5), c("T", "TS", "NS", "N", "NI", "TI", "T"))
	abline(h=0, lty="dotted", col="gray20")
	
	lines(angles, qn[,3], col="limegreen", lwd=3)
	xx = seq(0, 360, length=length(rnfltdiff))
	lines(xx, rnfltdiff, lwd=3)
	legend(
		"top",
		horiz=F,
		col=c("palegreen", "khaki1", "indianred1"),
		legend=c("5%-95%", "1%-5% / 95%-99%", "<1% / >99%"),
		pch=15,
		bg="white")
}

plot.rnflt.diff.sectors.with.norms <- function(rnfltdiff, age, radiusdiff=0)
# analogous to plot.rnflt.diff.with.norms, but for the sectors (and global mean)
# and with absolute RNFLT differences
#
# abs.rnfltdiff.sectors: list with absolute RNFLT difference for each sector
#   list elements: "rnfltMeanG", "rnfltMeanT", "rnfltMeanTS", "rnfltMeanTI", 
#   "rnfltMeanN", "rnfltMeanNS", "rnfltMeanNI"
{
	sector.labels = c("rnfltMeanG", "rnfltMeanT", "rnfltMeanTS", "rnfltMeanTI", "rnfltMeanN", "rnfltMeanNS", "rnfltMeanNI")
	calculate.median.abs.rnflt.diff <- function(sector.label)
		mean(as.numeric(abs(rnfltdiff[sector.indices(sector.label)])), na.rm=T)
	abs.mean.diffs = sapply(sector.labels, calculate.median.abs.rnflt.diff)
	norms = calculate.normative.distribution.sectors.absdiff(age, radiusdiff=radiusdiff)
	qn = sapply(c(0.5, 0.95, 0.99), function(x) qBCT(x, norms$mu, norms$sigma, norms$nu, norms$tau))
	m = as.data.frame(cbind(as.numeric(abs.mean.diffs), qn))
	colnames(m) <- c("m", "median", "q95", "q99")
	rownames(m) <- sector.labels
	cols <- ifelse(m$m>=m$q99, "indianred1", ifelse(m$m>=m$q95, "khaki1", "palegreen"))
	names(cols) <- sector.labels
	
	bw = 0.05	# border width
	op = par(mar=rep(0.1,4))
	plot(1, 1, xlim=c(-3,3), ylim=c(-5,4), type="n", bty="n", xaxt="n", yaxt="n", asp=1, xlab="", ylab="")
	
	draw.circle(0, 0, 1-bw, col=cols["rnfltMeanG"], border = NA)
	draw.sec <- function(angle1, angle2, ...)
		drawSectorAnnulus((angle1-2)*pi/180, (angle2+2)*pi/180, radius1=1+bw, radius2=3, ...)
	draw.sec(180+45, 180-45, col=cols["rnfltMeanT"])
	draw.sec(180-45, 90, col=cols["rnfltMeanTS"])
	draw.sec(90, 45, col=cols["rnfltMeanNS"])
	draw.sec(45, -45, col=cols["rnfltMeanN"])
	draw.sec(-45, -90, col=cols["rnfltMeanNI"])
	draw.sec(-90, -135, col=cols["rnfltMeanTI"])
	draw.circle(0, 0, 3)
	add.label <- function(sec="rnfltMeanG")
	{
		label = sub("rnfltMean(.+)$", "\\1", sec)
		r=ifelse(label=="G", 0, 2)
		phi = switch(label,
			T = 180,
			TS = 112.5,
			NS = 67.5,
			N = 0,
			NI = -67.5,
			TI = -112.5,
			0)
		x = r*cos(phi*pi/180)
		y = r*sin(phi*pi/180)
		text(x, y, paste(label, round(abs.mean.diffs[sec]), "", sep="\n"))
		text(x, y, sprintf("\n\n(%.0f)", m[sec, "median"]), col="limegreen")
	}
	lapply(sector.labels, add.label)
	text(0, 4, "Mean absolute RNFLT\ndifference |OS-OD|", font=2, cex=1.2)
	y = textbox(c(0,3), -4, "<95%", justify="c", margin=0.1, fill="palegreen")
	y = textbox(c(0,3), y[2], "95% - 99%", justify="c", margin=0.1, fill="khaki1")
	y = textbox(c(0,3), y[2], ">99%", justify="c", margin=0.1, fill="indianred1")
	par(op)
}

difference.colorplot <- function(rnfltdiff, age, radiusdiff=0, smoothing=smooth.spline, ...)
	# combines plot.rnflt.diff.sectors.with.norms and plot.rnflt.diff.with.norms
	# in single plot
{
	layout(matrix(1:2, nrow=1), widths=c(0.75, 0.25))
	plot.rnflt.diff.with.norms(rnfltdiff=rnfltdiff, age=age, radiusdiff=radiusdiff, smoothing=smooth.spline, ...)
	plot.rnflt.diff.sectors.with.norms(rnfltdiff=rnfltdiff, age=age, radiusdiff=radiusdiff)
}

difference.colorplot.wrapper <-
	function( visitor, smoothing=smooth.spline, ... ) {
	
	difference.colorplot( rnfltdiff = visitor[ -c( 1, 2 ) ], age = visitor [[ "age" ]], radiusdiff = visitor [[ "radiusdiff" ]], smoothing = smoothing, ... )
}

