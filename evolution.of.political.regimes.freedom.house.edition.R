# Data in .csv format at this link. Load as 'fh', then skip to line 70 to make plots.
https://drive.google.com/file/d/0B5wyt4eDq98GZ21LZXY1VVJNUTg/edit?usp=sharing

# FREEDOM HOUSE HEAT MAPS
# Jay Ulfelder
# 2014-02-13

# Clear the workspace
rm(list=ls(all=TRUE))

########################################
# Data Compilation
########################################

# Load some packages we need
require(xlsx)
require(reshape)

# Get the 1972-2012 data from the web
fiw <- read.xlsx2("c:/users/jay/documents/chenoweth project/indata/Country Status and Ratings, 1973-2013 (FINAL)_0.xls",
  sheetIndex = 1, startRow = 7, endRow = 212)
fhyrs <- c(1972:1980,1982:2012)
var_years <- expand.grid( x=c('PR', 'CL', 'Status'), y = fhyrs)
names(fiw) <- c('country', paste(var_years$x, var_years$y, sep = "_"))
fiw_m <- melt(fiw, id = 'country')
fiw_m <- cbind(fiw_m, colsplit(fiw_m$variable, "_", names = c('indicator', 'year')))
fiw_m$variable <- NULL
fiw.pr <- subset(fiw_m, indicator=="PR")
fiw.pr$indicator <- NULL
names(fiw.pr) <- c("country", "fiw.pr", "year")
fiw.cl <- subset(fiw_m, indicator=="CL")
fiw.cl$indicator <- NULL
names(fiw.cl) <- c("country", "fiw.cl", "year")
fiw.status <- subset(fiw_m, indicator=="Status")
fiw.status$indicator <- NULL
names(fiw.status) <- c("country", "fiw.status", "year")
fiw <- merge(fiw.pr, fiw.cl)
fiw <- merge(fiw, fiw.status)
fiw$country <- as.character(fiw$country)
fiw$fiw.pr <- as.numeric(as.character(fiw$fiw.pr))
fiw$fiw.cl <- as.numeric(as.character(fiw$fiw.cl))
fiw$fiw.status <- as.character(fiw$fiw.status)
fiw$fiw.status[fiw$fiw.status==".."] <- fiw$fiw.status[fiw$fiw.status=="F (NF)"] <- NA
data <- fiw
data <- rename(data, c(country="name"))
source("c:/users/jay/documents/chenoweth project/r/pitf_code_maker.r") # Local script to make country codes
data <- rename(data, c(code="sftgcode"))
fiw <- subset(data, is.na(sftgcode)==FALSE)
fiw$name <- NULL
fiw <- na.omit(fiw)
fiw <- subset(fiw, select = c(sftgcode, fiw.pr, fiw.cl, year))

# Get the fat-fingered 2013 data
fiw2013 <- read.csv("c:/users/jay/documents/couprisk2014/indata/fiw.2013.csv")
fiw2013$year <- NULL
names(fiw2013) <- c("sftgcode", "fiw.pr", "fiw.cl")
fiw2013$sftgcode <- as.character(fiw2013$sftgcode)
fiw2013$year <- 2013

# Put the two together
fh <- data.frame(rbind(fiw, fiw2013), stringsAsFactors = FALSE)
fh <- fh[order(fh$sftgcode, fh$year),]
names(fh) <- c("code", "pr", "cl", "year")

# Make sure the vars are read as ordered factors so all empty cells are included and reverse
# order so the most liberal appear in the upper right.
fh$pr.f <- ordered(8 - fh$pr, levels = c(1:7), labels = as.character(seq(7,1,-1)))
fh$cl.f <- ordered(8 - fh$cl, levels = c(1:7), labels = as.character(seq(7,1,-1)))

##########################################
# Plotting
##########################################

# Set a color palette in greyscale for the maps
greyscale <- rev(rep(paste("grey", seq(0,100,2), sep="")))

heatmappct <- function(yr) {
  z <- subset(fh, year == yr)
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
for( i in seq(1982,2013,1) ) { heatmappct(i) }  # Skips missing year of 1981

require(animation)
ani.options( convert = shQuote("C:/Program Files (x86)/ImageMagick-6.7.6-q16/convert.exe") )
ani.options( outdir = getwd() )
ani.options( nmax = 50)
ani.options( interval = 1 )
ani.options( ani.type = "jpeg", ani.dev = "jpeg")
im.convert("fhm.*.png", output = "freedomhouse.heatmap.20140213.gif")

