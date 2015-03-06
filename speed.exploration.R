# Housekeeping
rm(list=ls(all=TRUE))

# Load required packages
library(plyr)
library(RColorBrewer)
library(scales)

# Load the data directly from the web
SPEED <- read.csv("http://www.clinecenter.illinois.edu/research/documents/ssp_public.csv", stringsAsFactors = FALSE)

# Get annual counts of events by type
SPEED$record <- 1  # Create unit to count
polatk.yr <- ddply(SPEED, .(year, EV_TYPE), summarise,
                   count = sum(record, na.rm = TRUE))

# Plot those counts
pal <- brewer.pal(4, "Paired") # Palette for color-blind qualitative comparisons
png("speed.type.counts.by.year.png", width = 6, height = 9/16 * 6, unit = "in", bg = "white", res = 300)
par(cex.axis = 0.75, mai = c(0.25, 0.25, 0.1, 0.1))
with(subset(polatk.yr, EV_TYPE == 1), plot(count, type = "l",
     lwd = 2, col = pal[1],
     axes = FALSE, ylim = c(0, round(max(polatk.yr$count), -2)), xlab = "", ylab = ""))
axis(1, at = seq(5,60,5), labels = seq(1950,2005,5), tick = FALSE, pos = 50)
axis(2, las = 2, tick = FALSE, pos = 1)
abline(h = seq(0, round(max(polatk.yr$count), -2), 200), lwd = 1, col = alpha("gray75", 0.5))
with(subset(polatk.yr, EV_TYPE == 2), lines(count, lwd = 2, col = pal[2]))
with(subset(polatk.yr, EV_TYPE == 4), lines(count, lwd = 2, col = pal[3]))
with(subset(polatk.yr, EV_TYPE == 5), lines(count, lwd = 2, col = pal[4]))
legend(x = 1, y = round(max(polatk.yr$count), -2) + 10,
       c("Political expression", "Political attacks", "Disruptive state acts", "Political reconfigurations"),
       cex = 0.75, lty = c(1,1), lwd = c(2,1), col = pal, bty = "n")
dev.off()

# Now for deaths
deaths.yr <- ddply(SPEED, .(year), summarise,
                   total = sum(N_KILLED_A, na.rm = TRUE))
png("speed.deaths.by.year.png", width = 6, height = 9/16 * 6, unit = "in", bg = "white", res = 300)
par(cex.axis = 0.75, mai = c(0.5, 0.5, 0.1, 0.1))
with(deaths.yr, plot(total, type = "l",
     lwd = 2, col = "red",
     axes = FALSE, ylim = c(0, 6000000), xlab = "", ylab = ""))
axis(1, at = seq(5,60,5), labels = seq(1950,2005,5), tick = FALSE, pos = 50)
axis(2, las = 2, at = seq(0,6000000,1000000), labels = c("0", "1m", "2m", "3m", "4m", "5m", "6m"), tick = FALSE, pos = 1)
abline(h = seq(0,6000000,1000000), lwd = 1, col = alpha("gray75", 0.5))
dev.off()

# And then for index of violence intensity
scale.yr <- ddply(SPEED, .(year), summarise,
                   total = sum(POL_VIOL, na.rm = TRUE))
png("speed.violence.intensity.by.year.png", width = 6, height = 9/16 * 6, unit = "in", bg = "white", res = 300)
par(cex.axis = 0.75, mai = c(0.5, 0.5, 0.1, 0.1))
with(scale.yr, plot(total, type = "l",
     lwd = 2, col = "darkred",
     axes = FALSE, ylim = c(0, 2200), xlab = "", ylab = ""))
axis(1, at = seq(5,60,5), labels = seq(1950,2005,5), tick = FALSE, pos = 50)
axis(2, las = 2, at = seq(0,2000,500), tick = FALSE, pos = 1)
abline(h = seq(0,2000,500), lwd = 1, col = alpha("gray75", 0.5))
dev.off()

# And then for mass demonstrations and strikes, a subset of expressive events
exp.yr <- ddply(SPEED, .(year, EXP_TYPE), summarise,
                   count = sum(record, na.rm = TRUE))
png("speed.protest.counts.by.year.png", width = 6, height = 9/16 * 6, unit = "in", bg = "white", res = 300)
par(cex.axis = 0.75, mai = c(0.25, 0.25, 0.1, 0.1))
with(subset(exp.yr, EXP_TYPE == 4), plot(count, type = "l",
     lwd = 2, col = "forestgreen",
     axes = FALSE, ylim = c(0, 250), xlab = "", ylab = ""))
axis(1, at = seq(5,60,5), labels = seq(1950,2005,5), tick = FALSE, pos = 10)
axis(2, las = 2, tick = FALSE, pos = 1)
abline(h = seq(0, 250, 50), lwd = 1, col = alpha("gray75", 0.5))
dev.off()

# Coup activity
coup.yr <- ddply(SPEED, .(year), summarise,
                   coup.s = sum(coup, na.rm = TRUE),
                   coup.f = sum(coup_failed, na.rm = TRUE) )
# X-axis scaling is funky in barplot, so the next two lines get me the positions I want to
# use to build an x-axis with the features I want after plotting. Sources:
# http://stackoverflow.com/questions/8285759/r-barplot-axis-scaling
# http://stackoverflow.com/questions/5237557/extracting-every-nth-element-of-a-vector
# Also, barplot requires a vector or matrix, not a data frame, and it wants observations in columns and categories
# in rows. So, to get stacked bars from a typical country-year TSCS data set, you need to transpose a matrix representing
# only the columns of the original data frame you want (here, the counts without the year column.
b <- barplot(t(as.matrix(coup.yr[,2:3])), plot = FALSE) # Gets vector of bar positions
a <- b[seq(5, length(b), 5)] # Gets vector of every 5th bar pos, starting w/fifth (1950)
# Now make the barplot I want
png("speed.coup.counts.by.year.png", width = 6, height = 9/16 * 6, unit = "in", bg = "white", res = 300)
par(cex.axis = 0.66, mai = c(0.25, 0.25, 0.25, 0.1))
barplot(t(as.matrix(coup.yr[,2:3])), border = "white", space = 0.2,
     axes = FALSE, ylim = c(0,25),
     col = c("darkolivegreen", "darkolivegreen2"),
     legend = c("successful", "failed"),
     args.legend = list(x = max(a), y = 25, bty = "n", border = "white", cex = 0.75))
axis(1, at = a, labels = seq(1950,2005,5), tick = FALSE, pos = 1.5)
axis(2, las = 2, tick = FALSE, pos = 1)
dev.off()
