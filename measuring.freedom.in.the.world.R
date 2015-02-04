# This script covers two posts:
# 1. "No, Democracy Has Not Been Discarded" (January 29, 2015)
# https://dartthrowingchimp.wordpress.com/2015/01/29/no-democracy-has-not-been-discarded/
# 2. "A Postscript on Measuring Change Over Time in Freedom in the World" (January 30, 2015)
# https://dartthrowingchimp.wordpress.com/2015/01/30/a-postscript-on-measuring-change-over-time-in-freedom-in-the-world/

# Load required packages
library(animation)
library(DataCombine)
library(plyr)
library(WDI)
library(scales)

# Get the Freedom House data, which I've posted in .csv form here:
# https://drive.google.com/file/d/0B5wyt4eDq98GZVhUamRKVXJpLVU/view?usp=sharing
FH <- read.csv("FH.19722014.csv", stringsAsFactors=FALSE)

# Make sure the vars are read as ordered factors so all empty cells are included and reverse
# order so the most liberal appear in the upper right.
FH$pr.f <- ordered(8 - FH$pr, levels = c(1:7), labels = as.character(seq(7,1,-1)))
FH$cl.f <- ordered(8 - FH$cl, levels = c(1:7), labels = as.character(seq(7,1,-1)))

## HEAT MAPS ##

# Set a color palette in greyscale for the maps
greyscale <- rev(rep(paste("grey", seq(0,100,2), sep="")))

heatmappct <- function(yr) {
  z <- subset(FH, year == yr)
  t <- table(z$cl.f, z$pr.f)
  tpct <- t/sum(t)
  png(file=paste("fhm", as.character(yr), "png", sep="."),
    width=6, height=6, units='in', bg='white', res=300)
  heatmap(tpct, Rowv=NA, Colv=NA, col = greyscale,
    xlab = "political rights", ylab = "civil liberties",
    main = paste(as.character(yr)),
    scale = "none",  # Makes sure empty cells get same color as low vals
    margins = c(4,4))  # Moves axis labels a little closer
  dev.off()
}

# Run the function for a selected range of years
for( i in seq(1972,1980,1) ) { heatmappct(i) }
for( i in seq(1982,2014,1) ) { heatmappct(i) }  # Skips missing year of 1981

# Make a .gif if you like.
# NOTE: This won't work unless you have ImageMagick in an identically named directory on your PC. To get it to work, 
# install ImageMagick locally and then use the appropriate directory from your system in line 48.
ani.options( convert = shQuote("c:/program files/ImageMagick-6.9.0-Q8/convert.exe") )
ani.options( outdir = getwd() )
ani.options( nmax = 60)
ani.options( interval = 1 )
ani.options( ani.type = "png", ani.dev = "png")
im.convert("fhm.*.png", cmd.fun = system, output = "freedomhouse.heatmap.2015.gif")

## CHANGE ANALYSIS ##

# Use slide from DataCombine to create one-year lags for cl and pr
FH <- FH[order(FH$country, FH$year),]
FH <- slide(FH, Var = "pr", GroupVar = "country", NewVar = "pr.lag1", slideBy = -1)
FH <- slide(FH, Var = "cl", GroupVar = "country", NewVar = "cl.lag1", slideBy = -1)

# Take diffs
FH$pr.chg1 <- FH$pr - FH$pr.lag1
FH$cl.chg1 <- FH$cl - FH$cl.lag1

# Inspect
with(FH, hist(pr.chg1, breaks=seq(-7,7,1)))
with(FH, hist(cl.chg1, breaks=seq(-7,7,1)))

# Make markers for gainers and losers
FH$gainer <- ifelse(FH$pr.chg1 < 0 | FH$cl.chg1 < 0, 1, 0)
FH$loser <- ifelse(FH$pr.chg1 > 0 | FH$cl.chg1 > 0, 1, 0)

# Get sums by year
FHchg <- ddply(FH, .(year), summarise,
  gainers = sum(gainer, na.rm=TRUE),
  losers = sum(loser, na.rm=TRUE),
  net = sum(pr.chg1 + cl.chg1, na.rm=TRUE))

# Replace wonky values with NAs
FHchg$net <- replace(FHchg$net, list=c(FHchg$year==1972 | FHchg$year==1981 | FHchg$year==1982), NA)
FHchg$gainers <- replace(FHchg$gainers, which(FHchg$year==1972 | FHchg$year==1981 | FHchg$year==1982), NA)
FHchg$losers <- replace(FHchg$losers, which(FHchg$year==1972 | FHchg$year==1981 | FHchg$year==1982), NA)

# Make the first of the two plots from the postscript post
png("c:/users/jay/documents/blog posts/fh maps/fh.annualnetchange.png",
  width=10, height=9/16 * 10, unit="cm", bg="white", res=300)
par(cex.axis=0.5, cex.lab=0.5, mar=c(2,1,1,1))
with(subset(FHchg, year > 1982),
  plot(-1 * net, type="l", lwd=2, col="blue", axes=FALSE,
  xlab="", ylab="", ylim=c(-60,60)))
axis(2, at=seq(-60,60,20), tick=FALSE, las=2, pos=1)
axis(1, at=seq(3,28,5), labels=seq(1985,2010,5), tick=FALSE, las=2, pos=-60)
abline(h = 0, lwd = 0.5, col = "gray25")
segments(x0=24, y0=-30, x1=32, y1=-30, lwd=1, col="gray25")
segments(x0=24, y0=-30, x1=24, y1=-25, lwd=1, col="gray25")
segments(x0=32, y0=-30, x1=32, y1=-25, lwd=1, col="gray25")
text(x=28, y=-30, "nine-year recession?", cex=0.4, pos=1, offset=0.2)
dev.off()

## POP-WEIGHTED, SYSTEM-LEVEL FREEDOM SCORE ##

# Create "freedom" score for each country that combines pr & cl, has intuitive
# direction, and is scaled 0-10
FH$freedom <- with(FH, 10 * ((14 - pr - cl)/12) )

# The next step requires my script to make PITF country codes; you can get it here:
# https://github.com/ulfelder/earlywarningproject-statrisk-replication/blob/master/R/f.pitfcodeit.R
# Save it to the working directory, then run this:
source("f.pitfcodeit.r") 

# Get population size from WDI
library(WDI)
Pop <- WDI(country="all", indicator = "SP.POP.TOTL", extra = FALSE, start = 1972, end = 2014)
Pop <- pitfcodeit(Pop, "country")
Pop <- subset(Pop, is.na(sftgcode)==FALSE, select = c("sftgcode", "year", "SP.POP.TOTL"))
names(Pop) <- c(names(Pop)[1:2], "popsize")
Pop <- Pop[order(Pop$sftgcode, Pop$year),]
row.names(Pop) <- NULL

# Fill in 2014 with 2013 values
Pop$popsize[Pop$year==2014] <- Pop$popsize[Pop$year==2013]

# Merge that with FH, only keeping cases with FH data
FH <- merge(FH, Pop, all.x=TRUE)

# Get annual global total pop for cases with no missing FH data
GloPop <- ddply(FH, .(year), summarise, glopop = sum(popsize, na.rm=TRUE))

# Merge that back into FH data frame
FH <- merge(FH, GloPop, all.x=TRUE)
FH <- FH[order(FH$country, FH$year),]

# Create population weight
FH$popweight <- with(FH, popsize/glopop)

# Now generate data frame with annual sums of pop-weighted scores
SysFreedom <- ddply(FH, .(year), summarise, sysfreedom = sum(freedom * popweight, na.rm=TRUE))
SysFreedom$sysfreedom[SysFreedom$year==1981] <- NA

# Now make the second plot from the postcript post
library(scales)
png("fh.systemfreedom.png", width=10, height=9/16 * 10, unit="cm", bg="white", res=300)
par(cex.axis=0.5, cex.lab=0.5, mar=c(2,2,1,1))
with(SysFreedom,
  plot(sysfreedom, type="l", lwd=2, col="black", axes=FALSE,
  xlab="", ylab="", ylim=c(3.75,5.5)))
axis(2, at=seq(3.75,5.5,0.25), tick=FALSE, las=2, pos=1)
axis(1, at=seq(4,40,5), labels=seq(1975,2010,5), tick=FALSE, las=2, pos=3.85)
abline(h = c(seq(3.75,5.5,0.25)), lwd = 0.5, col = alpha("black", 0.2))
dev.off()
