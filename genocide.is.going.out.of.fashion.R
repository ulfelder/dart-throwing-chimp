# This is the script I used to produce the second figure in the piece Foreign Policy ran on May 14, 2015, and
# entitled "Genocide Is Going Out of Fashion", here:
#
# http://foreignpolicy.com/2015/05/14/genocide-is-going-out-of-fashion/

library(plyr)
library(WDI)
library(scales)

# Ingest data on one-sided violence from UCDP's web site.
PRIO <- read.csv("http://www.pcr.uu.se/digitalAssets/124/124932_1ucdp_one-sidedviolencedataset1.4-2014.csv",
     stringsAsFactors = FALSE)

# Use ddply function from 'plyr' to get annual, global sums of deaths for low, best, and high estimates.
PRIO.yr <- ddply(PRIO, .(Year), summarise,
     low = sum(LowFatalityEstimate),
     best = sum(BestFatalityEstimate),
     high = sum(HighFatalityEstimate))
names(PRIO.yr) <- tolower(names(PRIO.yr))

# Use 'WDI' package to get annual, global population estimates for the time period covered by
# the UCDP data set, then cut unneeded columns and fix names for merging with PRIO.yr. 
Pop <- WDI(country="1W", indicator="SP.POP.TOTL", extra=FALSE,
     start=min(PRIO.yr$year), end=max(PRIO.yr$year))
Pop$iso2c <- Pop$country <- NULL
names(Pop) <- c("population", "year")

# Change variable names to lowercase to prepare for merge to come.
PRIO.yr <- merge(PRIO.yr, Pop)

# Following Pinker, use UCDP's high estimates to generate rates per 100,000.
PRIO.yr$rate <- with(PRIO.yr, (high/(population/100000)))

# Plot the rate with the y-axis scaled to match Pinker's figure (0-400).
png("prio.onesided.death.rate.by.year.for.fp.png", width=6, height=4, unit="in", bg="white", res=150)
par(cex.axis=0.75, mai=c(1, 0.75, 0.1, 0.1))
plot(x=c(1,length(PRIO.yr$rate)), y=c(0,400), type="n", xlab="", ylab="", axes=FALSE)
axis(2, at=seq(0,400,50), pos=1, las=2)
mtext("Deaths per 100,000 people per year", side=2, line=2, cex=0.75)
axis(1, at=seq(2,22,5), labels=seq(1990,2010,5), pos=15, tick=FALSE, las=2)
segments(x0=1, x1=length(PRIO.yr$rate), y0=0, y1=0, lwd=1, col="black")
with(PRIO.yr, lines(rate, col="darkred", lwd=2))
mtext("Annual rates of deaths from one-sided violence worldwide, 1989-2013", side=1, line=2.75, font=2, cex=0.8)
mtext("Data sources: UCDP, World Bank", side=1, line=3.5, cex=0.6)
dev.off()
