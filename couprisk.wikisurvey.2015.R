# This script assumes that the working directory contains the file called in line 13. It can be
# downloaded from https://drive.google.com/file/d/0B5wyt4eDq98GcGVpeS0zR1lmTmc/view?usp=sharing

# Housekeeping
rm(list=ls(all=TRUE))

# Load required packages
library(rworldmap)
library(Hmisc)
library(gplots)

# Load the data
wiki2015 <- read.csv("wikisurvey_5678_ideas_2015-01-14T15-58-25Z.csv", stringsAsFactors = FALSE)

# Join the data to a map
wikimap <- joinCountryData2Map(wiki2015, nameJoinColumn = "Idea.Text", joinCode = "NAME", verbose = TRUE)

# Correct mismatches
wiki2015$Idea.Text[wiki2015$Idea.Text=="Timor Leste"] <- "Timor-Leste"

# Remove "None of them" row and user-submitted ideas
wiki2015 <- wiki2015[wiki2015$Idea.Text != "None of them",]
wiki2015 <- wiki2015[wiki2015$User.Submitted != TRUE,]

# Redo with mismatches corrected
wikimap <- joinCountryData2Map(wiki2015, nameJoinColumn = "Idea.Text", joinCode = "NAME", verbose = TRUE)

# Turn off mapping of Antarctica and Greenland
tmp <- wikimap
tmp <- tmp[ tmp$ADMIN != "Antarctica" & tmp$ADMIN != "Greenland",]
wikimap <- tmp

# Create broad grayscale palette
grayscale <- c(paste0(rep("gray", times = max(round(wiki2015$Score, 0)) - min(round(wiki2015$Score, 0))),
    as.character(seq(max(round(wiki2015$Score, 0)),min(round(wiki2015$Score, 0)),-1))))

# Generate the world map
jpeg("wikisurvey.couprisk.2015.map.jpg", width=800, height=450, bg="white", quality = 90)
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
map.Score <- mapCountryData(wikimap,
  nameColumnToPlot = "Score",
  addLegend = FALSE,
  numCats = max(round(wiki2015$Score, 0)) - min(round(wiki2015$Score, 0)),
  catMethod = "fixedWidth", colourPalette = grayscale, borderCol = "white",
  mapTitle = "Relative Risk of Any Coup Attempts in 2015")
do.call(addMapLegend, c(map.Score, legendWidth = 0.5, legendMar = 2))
mtext("map made by Jay Ulfelder using rworldmap             ", # Spaces needed for alignment
  line = -4, side = 1, adj = 1, cex = 0.8)
dev.off()

# Dot plot of scores with rankings shown
wiki2015 <- wiki2015[order(-wiki2015$Score),]
row.names(wiki2015) <- NULL  # Reset to current order for rankings
jpeg(file = "wikisurvey.couprisk.2015.dotplot.jpg",
  width=500, height=2500, bg="white", quality=90 )
dotchart2(wiki2015$Score,
  labels=paste(wiki2015$Idea.Text, row.names(wiki2015), sep="  "),  # Show ranking next to country name
  lines=TRUE, lwd=0.05, lty=3, sort=FALSE, dotsize=1, col="cornflowerblue", pch=20, cex.labels=0.8 )
title(main=list("Relative Risk of Any Coup Attempts in 2015", cex=0.9))
dev.off()
