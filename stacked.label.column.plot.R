# Associated with the post "The Stacked-Label Column Plot"
# http://wp.me/p1domH-1Gs

# Load required packages and functions
library(DataCombine)

# JOINING GATT/WTO

# Load data downloaded from here: 
# https://drive.google.com/file/d/0B5wyt4eDq98GUGhOQU4wUzZ6Y0E/view?usp=sharing
ios <- read.csv("ios.csv", stringsAsFactors = FALSE)

# Merge with generic TSCS rack covering same period to get yrborn variable needed for next step, in case any
# countries joined in the year they were "born", when the lagged indicator will be missing. To get that rack,
# source the two functions called in line 20. They can be downloaded from the following two links:
# https://drive.google.com/file/d/0B5wyt4eDq98GblZpSEQwbVI1ZFk/view?usp=sharing
# https://drive.google.com/file/d/0B5wyt4eDq98GWlY0T1NqdXNrRTg/view?usp=sharing
source("f.countryyearrackit.r")
source("f.pitfcodeit.r")
rack <- pitfcodeit(countryyearrackit (1955,2014), "country") 
ios <- merge(rack, ios)

# Create indicator for joining from data on belonging
ios <- ios[order(ios$country, ios$year),]
ios <- slide(ios, Var = "ios.gattwto", NewVar = "ios.gattwto.1", GroupVar = "country", slideBy = -1)
ios$gattwto.join <- ifelse((ios$ios.gattwto == 1 & ios$ios.gattwto.1 == 0) |
     (ios$ios.gattwto == 1 & ios$year == ios$yrborn), 1, 0)

# Create event file for joining and add y values for plotting based on alphabetical order within years
Years <- subset(ios, gattwto.join == 1, select = c(sftgcode, year))
Years <- Years[order(Years$year, Years$sftgcode),]
for (i in 1:dim(Years)[1]) Years$y[i] <- match(Years$sftgcode[i], Years$sftgcode[Years$year == Years$year[i]])

# Make the chart
png("stack.wto.png", width = 8, height = 2, units = "in", res = 150, bg = "white")
par(mai=c(0.25, 0.05, 0.1, 0.05), cex.axis = 0.5)
with(Years, plot(x = year, y = y, type = "n", axes = FALSE, xlim = c(1960,2014), xlab = "", ylab = ""))
with(Years, text(x = year, y = y, labels = sftgcode, cex = 0.4, family = "mono", pos = 3 ))
axis(1, at = (min(Years$year)-1):max(Years$year), las = 2, pos = 2.25, tick = FALSE)
segments(1959.5, 1.25, 2014.5, 1.25, lwd = 1, col = "red")
dev.off()

# COUP ATTEMPTS

# Ingest data available here:
# https://drive.google.com/file/d/0B5wyt4eDq98GaUdVNlA1STdPUmc/view?usp=sharing
cpt <- read.csv("cpt.csv", stringsAsFactors = FALSE)

# Create event file for joining and add y values for plotting based on alphabetical order within years
cpt$coup <- ifelse(cpt$cpt.succ > 0, 2, ifelse(cpt$cpt.fail > 0, 1, 0))
Coups <- subset(cpt, coup > 0, select = c(sftgcode, year, coup))
Coups <- Coups[order(Coups$year, Coups$sftgcode),]
for (i in 1:dim(Coups)[1]) Coups$y[i] <- match(Coups$sftgcode[i], Coups$sftgcode[Coups$year == Coups$year[i]])

# Make the chart for 1960 on
Coups <- subset(Coups, year >= 1960)
condcol <- ifelse(Coups$coup == 2, "red", "#636363")
png("c:/users/jay/documents/blog posts/stack.coups.png", width = 8, height = 2, units = "in", res = 150, bg = "white")
par(mai=c(0.25, 0.05, 0.1, 0.05), cex.axis = 0.5)
with(Coups, plot(x = year, y = y, type = "n", axes = FALSE, xlab = "", ylab = ""))
with(Coups, text(x = year, y = y, labels = sftgcode, cex = 0.4, family = "mono", col = condcol, pos = 3 ))
axis(1, at = c(1960:2014), las = 2, pos = 2.25, tick = FALSE)
segments(1959.5, 1.25, 2014.5, 1.25, lwd = 1.25, col = "gray40")
dev.off()
