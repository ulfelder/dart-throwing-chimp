# Associated with the post "The Stacked Label Plot"
# http://wp.me/p1domH-1Gs

# Load required packages and functions
library(DataCombine)

# Load updated data with memberships in various IGOs and treaty regimes
# Note: this code assumes the data set is in working directory; you can download it in .csv format from this URL:
# 
rack <- read.csv("ios.csv", stringsAsFactors = FALSE)

# Create lagged version of WTO membership indicator using DataCombine's slide function
rack <- rack[order(rack$country, rack$year),]
rack <- slide(rack, Var = "ios.gattwto", NewVar = "ios.gattwto.1", GroupVar = "country", slideBy = -1)

# Compare the current to the lagged indicator to create a marker for joining
rack$gattwto.join <- ifelse((rack$ios.gattwto == 1 & rack$ios.gattwto.1 == 0) |
     (rack$ios.gattwto == 1 & rack$year == rack$yrborn), 1, 0)

# Create a y value based on the case's position in an alphabetically ordered vector of all cases for that year
Years <- subset(rack, gattwto.join == 1, select = c(sftgcode, year))
Years <- Years[order(Years$year, Years$sftgcode),]
for (i in 1:dim(Years)[1]) Years$y[i] <- match(Years$sftgcode[i], Years$sftgcode[Years$year == Years$year[i]])

# Now plot it
png("stack.wto.png", width = 8, height = 2, units = "in", res = 150, bg = "white")
par(mai=c(0.25, 0.05, 0.1, 0.05), cex.axis = 0.5)
with(Years, plot(x = year, y = y, type = "n", axes = FALSE, xlim = c(1960,2014), xlab = "", ylab = ""))
with(Years, text(x = year, y = y, labels = sftgcode, cex = 0.4, family = "mono", pos = 3 ))
axis(1, at = (min(Years$year)-1):max(Years$year), las = 2, pos = 2.25, tick = FALSE)
segments(1959.5, 1.25, 2014.5, 1.25, lwd = 1, col = "red")
dev.off()
