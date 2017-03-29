# This script produces the animated series of heat maps in this post:
# http://dartthrowingchimp.wordpress.com/2012/10/01/211-years-of-political-evolution-in-60-seconds-new-and-improved/

# Retreive the Polity data circa 2010 in .csv format from my Google Drive
https://docs.google.com/file/d/0B5wyt4eDq98GVVFnN0JjZElCV3c/edit?usp=sharing

# Load the data
poldat <- read.csv("p4v2010.csv", header = TRUE)

# Create a smaller file with just the years & the two vars of interest
tabdat <- subset(poldat, select=c('year', 'exrec', 'polcomp'))

# Replace the Polity "special" codes with NAs
tabdat$exrec[tabdat$exrec < 1] <- NA
tabdat$polcomp[tabdat$polcomp < 1] <- NA

# Make sure the vars are read as ordered factors so all empty cells are included
tabdat$exrec <- as.ordered(tabdat$exrec)
tabdat$polcomp <- as.ordered(tabdat$polcomp)

# Set a color palette in greyscale for the maps
greyscale <- rev(rep(paste("grey", seq(0,100,2), sep="")))

heatmappct <- function(x) {
                           z <- subset(tabdat, year == x)
                           t <- table(z$polcomp, z$exrec)
                           tpct <- t/sum(t)
                           jpeg(file=paste("pm", as.character(x), "pct", ".jpg", sep=""),
                                width=6, height=6, units='in', bg='white', res=150, quality=100)
                           heatmap(tpct, Rowv=NA, Colv=NA, col = greyscale,
                                   xlab = "executive recruitment", ylab = "political competition",
                                   main = paste(as.character(x)),
                                   scale = "none" )  # Makes sure empty cells get same color as low vals.
                           dev.off()
                           }

# Run the function for a selected range of years
for( i in seq(1800,2010,1) ) { heatmappct(i) }

# Now make an animation using 'animation'
library(animation)
ani.options( convert = shQuote("C:/Program Files (x86)/ImageMagick-6.7.6-q16/convert.exe"),
             outdir = getwd(),     # Send finished animation to working drive.
             interval = 60/211,    # Shorten interval between slides to make whole thing run in 1 min.
             nmax = 211,           # Set maximum number of slides to 211.
             autoplay = F )        # Turn off autoplay.
ani.options( ani.type = "jpeg", ani.dev = "jpeg")             
im.convert("pm*pct.jpg", output = "politymoviepcts.gif")
