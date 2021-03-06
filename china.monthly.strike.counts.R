# This script uses data downloaded from the China Labour Bulletin's strike map (http://maps.clb.org.hk/strikes/en)
# on May 28, 2015. The date range was set to January 2011-May 2015; all other fields were set to "Any".

# It will need to be edited if the number of months covered changes. The summarizing parts will work fine;
# it's just that some aspects of the plotting code (e.g., position of axes) are based on time series with 53 observations.

# You can download the data file I used here from my Google Drive, here:
# https://drive.google.com/file/d/0B5wyt4eDq98GUWNwTUg0ZGpWVHM/view?usp=sharing

library(XLConnect)
library(plyr)
library(scales)

CLB <- readWorksheetFromFile("china labor bulletin incident data.xlsx", sheet=1)
CLB$year <- as.numeric(substr(as.character(CLB$Date), 1, 4))
CLB$month <- as.numeric(substr(as.character(CLB$Date), 6, 7))
CLB$event <- 1

# PLOTTING BY PROVINCE

CLB.mo.pro <- ddply(CLB, .(year, month, Location), summarise, total = sum(event))

# The resulting df doesn't have rows for cases with no events, but we want to include those
# and put zeroes there instead. So we make a grid with all province-month combinations, merge
# the ddply results with it, and replace the NAs with 0s.
CLB.mo.pro.grid <- expand.grid(Location = unique(CLB$Location),
     year = seq(min(CLB$year), max(CLB$year)),
     month = seq(1, 12))
# Cut rows for months that haven't happened and unnamed locations
CLB.mo.pro.grid <- subset(CLB.mo.pro.grid, (year < 2015 | (year==2015 & month <= 5)) & is.na(Location)==FALSE)
# Merge
CLB.mo.pro.2 <- merge(CLB.mo.pro.grid, CLB.mo.pro, all.x=TRUE)
# Replace NAs with 0s
CLB.mo.pro.2[is.na(CLB.mo.pro.2)] <- 0

# Function to plot by province
plotit <- function(name) {
     z <- subset(CLB.mo.pro.2, Location == name)
     with(z, plot(total, type = "n", xlab = "", ylab = "", ylim=c(0,40), axes=FALSE))
     mtext(name, side=2, line=1, las=2, cex=0.8)
     abline(h=c(0,20,40), col=alpha("gray50", 0.5), lwd=0.5)
     with(z, lines(total, col="gray25", lwd = 2))
     axis(4, at=c(0,20,40), tick=FALSE, pos=50, las=2)
}

png("sparklines.province.png", width=6, height=10, unit="in", bg="white", res=300)
par(mai=c(0.1, 1.25, 0.1, 0.2), cex.axis=0.5, mfrow=c(ceiling(length(unique(CLB.mo.ind.2$Location))/2),2))
for (i in 1:length(unique(CLB.mo.pro.2$Location))) plotit(as.character(unique(CLB.mo.pro.2$Location)[i]))
dev.off()

# BY INDUSTRY

CLB.mo.ind <- ddply(CLB, .(year, month, Industry), summarise, total = sum(event))

# The resulting df doesn't have rows for cases with no events, but we want to include those
# and put zeroes there instead. So we make a grid with all province-month combinations, merge
# the ddply results with it, and replace the NAs with 0s.
CLB.mo.ind.grid <- expand.grid(Industry = unique(CLB$Industry),
     year = seq(min(CLB$year), max(CLB$year)),
     month = seq(1, 12))
# Cut rows for months that haven't happened and unnamed industries
CLB.mo.ind.grid <- subset(CLB.mo.ind.grid, (year < 2015 | (year==2015 & month <= 5)) & is.na(Industry)==FALSE)
# Merge
CLB.mo.ind.2 <- merge(CLB.mo.ind.grid, CLB.mo.ind, all.x=TRUE)
# Replace NAs with 0s
CLB.mo.ind.2[is.na(CLB.mo.ind.2)] <- 0

# Function to plot by industry. The y-axis range is set to accommodate the maximum
# value for any one industry and is standardized to facilitate comparison across industries.
plotit <- function(name) {
     z <- subset(CLB.mo.ind.2, Industry == name)
     with(z, plot(total, type = "n", xlab = "", ylab = "", ylim=round(range(CLB.mo.ind.2$total), -1), axes=FALSE))
     mtext(name, side=2, line=1, las=2, cex=0.8)
     abline(h=c(0,60,120), col=alpha("gray50", 0.5), lwd=0.5)
     with(z, lines(total, col="gray25", lwd = 2))
     axis(4, at=c(0,60,120), tick=FALSE, pos=50, las=2)
}

# I'm setting the height of the .png to be proportionate to the one for provinces so the sparklines
# are the same size across the two figures.
png("sparklines.industry.png", width=6, height=5/16 * 10, unit="in", bg="white", res=300)
par(mai=c(0.1, 1.25, 0.1, 0.2), cex.axis=0.5, mfrow=c(ceiling(length(unique(CLB.mo.ind.2$Industry))/2),2))
for (i in 1:length(unique(CLB.mo.ind.2$Industry))) plotit(as.character(unique(CLB.mo.ind.2$Industry)[i]))
dev.off()

# BY DEMAND

# Run through the demand field iteratively, looking for specific strings. This starts with strsplit to break
# the Demands field into strings separated by commas. Next, it uses grep to look within the vectors that creates
# for each row for the specified string (e.g., "wage arrear"). isTRUE converts the result to a logical that ifelse
# can understand. If the specified word or phrase is there, it gets a 1; otherwise 0. I thought about doing this in
# a single claim.type variable, but that gets screwy with cases that fit multiple categories. Then this would keep
# replacing the value sequentially, ultimately only showing the last one checked for in this sequence.

# Start by making a lowercase version of the Demands var to avoid errors of omission from capitalization.
CLB$demands <- tolower(CLB$Demands)
# Get list with vectors of comma-separated demand strings for each record
x <- strsplit(CLB$demands, ",")
for (i in 1:dim(CLB)[1]) CLB$wage.arrears[i] <- ifelse(isTRUE(grep("wage arrear", x[[i]]) > 0), 1, 0)
for (i in 1:dim(CLB)[1]) CLB$social.security[i] <- ifelse(isTRUE(grep("social security", x[[i]]) > 0), 1, 0)
for (i in 1:dim(CLB)[1]) CLB$work.conditions[i] <- ifelse(isTRUE(grep("work conditions", x[[i]]) > 0), 1, 0)
for (i in 1:dim(CLB)[1]) CLB$pay[i] <- ifelse(isTRUE(grep("pay", x[[i]]) > 0), 1, 0)
for (i in 1:dim(CLB)[1]) CLB$violence[i] <- ifelse(isTRUE(grep("violence", x[[i]]) > 0) |
     isTRUE(grep("attack", x[[i]]) > 0) | isTRUE(grep("thug", x[[i]]) > 0) |
     isTRUE(grep("beat", x[[i]]) > 0) | isTRUE(grep("kill", x[[i]]) > 0), 1, 0)
for (i in 1:dim(CLB)[1]) CLB$compensation[i] <- ifelse(isTRUE(grep("compensation", x[[i]]) > 0), 1, 0)
for (i in 1:dim(CLB)[1]) CLB$pension[i] <- ifelse(isTRUE(grep("pension", x[[i]]) > 0), 1, 0)
for (i in 1:dim(CLB)[1]) CLB$taxi[i] <- ifelse(isTRUE(grep("taxi", x[[i]]) > 0) |
     isTRUE(grep("cabs", x[[i]]) > 0) | isTRUE(grep("uber", x[[i]]) > 0) |
     isTRUE(grep("car", x[[i]]) > 0) | isTRUE(grep("rickshaw", x[[i]]) > 0), 1, 0)
for (i in 1:dim(CLB)[1]) CLB$relocation[i] <- ifelse(isTRUE(grep("relocation", x[[i]]) > 0), 1, 0)
for (i in 1:dim(CLB)[1]) CLB$corruption[i] <- ifelse(isTRUE(grep("corruption", x[[i]]) > 0), 1, 0)
for (i in 1:dim(CLB)[1]) CLB$prices[i] <- ifelse(isTRUE(grep("prices", x[[i]]) > 0), 1, 0)
for (i in 1:dim(CLB)[1]) CLB$overtime[i] <- ifelse(isTRUE(grep("ot", x[[i]]) > 0), 1, 0)
for (i in 1:dim(CLB)[1]) CLB$layoffs[i] <- ifelse(isTRUE(grep("layoff", x[[i]]) > 0), 1, 0)
for (i in 1:dim(CLB)[1]) CLB$bonus[i] <- ifelse(isTRUE(grep("bonus", x[[i]]) > 0), 1, 0)
for (i in 1:dim(CLB)[1]) CLB$merger[i] <- ifelse(isTRUE(grep("merger", x[[i]]) > 0), 1, 0)
for (i in 1:dim(CLB)[1]) CLB$housing[i] <- ifelse(isTRUE(grep("housing", x[[i]]) > 0), 1, 0)
for (i in 1:dim(CLB)[1]) CLB$regulation[i] <- ifelse(isTRUE(grep("regulation", x[[i]]) > 0), 1, 0)
for (i in 1:dim(CLB)[1]) CLB$leave[i] <- ifelse(isTRUE(grep("leave", x[[i]]) > 0), 1, 0)
for (i in 1:dim(CLB)[1]) CLB$contract[i] <- ifelse(isTRUE(grep("contract", x[[i]]) > 0), 1, 0)
for (i in 1:dim(CLB)[1]) CLB$housing[i] <- ifelse(isTRUE(grep("housing", x[[i]]) > 0), 1, 0)
for (i in 1:dim(CLB)[1]) CLB$management[i] <- ifelse(isTRUE(grep("management", x[[i]]) > 0), 1, 0)

# Then use ddply to get monthly sums by various aggregations of these dummies...
CLB.monthly <- ddply(CLB, .(year, month), summarise,
     total = sum(event),
     pay = sum(wage.arrears, pay, compensation, bonus, overtime),
     conditions = sum(work.conditions, housing, leave), 
     layoffs = sum(layoffs),
     taxi = sum(taxi), 
     welfare = sum(social.security, pension))

# Then plot results

png("china.monthly.strike.count.png",
     width = 6, height = 2.5, unit = "in", bg = "white", res = 300)
par(cex.axis = 0.7, mai=c(0.4, 0.5, 0.1, 0.1))
with(CLB.monthly, plot(total, type = "n", xlab = "", ylab = "", axes = FALSE))
axis(2, tick = FALSE, las = 2)
axis(1, at=seq(1, dim(CLB.monthly)[1], 12), labels=seq(2011,2015), tick=FALSE, pos=15)
abline(h = seq(0, 300, 50), lwd = 0.5, col = alpha("gray50", alpha = 1/2))
with(CLB.monthly, lines(total, col = "gray25", lwd = 2))
dev.off()

# Did this one manually to customize labels.
png("sparklines.demands.png", width=6, height=2/16 * 10, unit="in", bg="white", res=300)
par(mai=c(0.1, 1.25, 0.1, 0.2), cex.axis=0.5, mfrow=c(2,2))
with(CLB.monthly, plot(pay, type = "n", xlab = "", ylab = "", ylim=c(0,260), axes=FALSE))
mtext("Pay", side=2, line=1, las=2, cex=0.8)
abline(h=seq(0,200,100), col=alpha("gray50", 0.5), lwd=0.5)
with(CLB.monthly, lines(pay, col="gray25", lwd = 2))
axis(4, at=seq(0,200,100), tick=FALSE, pos=50, las=2)
with(CLB.monthly, plot(welfare, type = "n", xlab = "", ylab = "", ylim=c(0,260), axes=FALSE))
mtext("Social security", side=2, line=1, las=2, cex=0.8)
abline(h=seq(0,200,100), col=alpha("gray50", 0.5), lwd=0.5)
with(CLB.monthly, lines(welfare, col="gray25", lwd = 2))
axis(4, at=seq(0,200,100), tick=FALSE, pos=50, las=2)
with(CLB.monthly, plot(conditions, type = "n", xlab = "", ylab = "", ylim=c(0,260), axes=FALSE))
mtext("Work conditions", side=2, line=1, las=2, cex=0.8)
abline(h=seq(0,200,100), col=alpha("gray50", 0.5), lwd=0.5)
with(CLB.monthly, lines(conditions, col="gray25", lwd = 2))
axis(4, at=seq(0,200,100), tick=FALSE, pos=50, las=2)
with(CLB.monthly, plot(layoffs, type = "n", xlab = "", ylab = "", ylim=c(0,260), axes=FALSE))
mtext("Layoffs", side=2, line=1, las=2, cex=0.8)
abline(h=seq(0,200,100), col=alpha("gray50", 0.5), lwd=0.5)
with(CLB.monthly, lines(layoffs, col="gray25", lwd = 2))
axis(4, at=seq(0,200,100), tick=FALSE, pos=50, las=2)
dev.off()
