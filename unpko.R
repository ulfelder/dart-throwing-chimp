library(plyr)
library(scales)
library(RColorBrewer)

PKO <- read.csv("http://www.providingforpeacekeeping.org/documents/Data.TCC.csv", stringsAsFactors = FALSE)

PKO$year <- as.numeric(substr(PKO$date, 1, 4))
PKO$month <- as.numeric(substr(PKO$date, 6, 7))

PKO.eu <- ddply(PKO, .(year, month, tcc.eu), summarise,
     tot = sum(total.sum, na.rm = TRUE))

pal <- brewer.pal(3, "Dark2")

png("unpko.contribution.comparison.png", width = 6.5, height = 9/16 * 6.5, unit = "in", bg = "white", res = 300)
par(cex.axis = 0.7, mai=c(0.25, 0.5, 0.1, 0.1))
plot(PKO.eu$tot[PKO.eu$tcc.eu == 1], type = "n",
     ylim = c(0,100000), xlab = "", ylab = "", axes = FALSE)
axis(2, at = seq(0,100000,25000), tick = FALSE, las = 2, line = -1/2)
axis(1, at = seq(2,278,12), labels = seq(1990, 2013, 1), tick = FALSE, pos = 5000)
abline(h = seq(0,100000,25000), lwd = 0.5, col = alpha("gray50", alpha = 1/2))
lines(PKO.eu$tot[PKO.eu$tcc.eu == 1], col = pal[3], lwd = 2)
lines(PKO.eu$tot[PKO.eu$tcc.eu == 0], col = pal[2], lwd = 2)
legend(x = 0, y = 95000, legend = c("Other countries", "EU countries"), cex = 0.75,
     bty = "n", fill = pal[2:3], border = "white")
dev.off()

PKO.us <- ddply(subset(PKO, tcc == "United States of America"), .(year, month), summarise,
     tot = sum(total.sum, na.rm = TRUE))

png("unpko.contribution.comparison.eu.us.png",
     width = 6.5, height = 9/16 * 6.5, unit = "in", bg = "white", res = 300)
par(cex.axis = 0.7, mai=c(0.25, 0.5, 0.1, 0.1))
plot(PKO.eu$tot[PKO.eu$tcc.eu == 1], type = "n",
     ylim = c(0,100000), xlab = "", ylab = "", axes = FALSE)
axis(2, at = seq(0,100000,25000), tick = FALSE, las = 2, line = -1/2)
axis(1, at = seq(2,278,12), labels = seq(1990, 2013, 1), tick = FALSE, pos = 5000)
abline(h = seq(0,100000,25000), lwd = 0.5, col = alpha("gray50", alpha = 1/2))
lines(PKO.eu$tot[PKO.eu$tcc.eu == 1], col = pal[3], lwd = 2)
lines(PKO.us$tot, col = pal[1], lwd = 2)
legend(x = 0, y = 95000, legend = c("USA", "EU countries"), cex = 0.75,
     bty = "n", fill = c(pal[1], pal[3]), border = "white")
dev.off()
