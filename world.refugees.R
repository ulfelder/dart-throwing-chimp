# The first step assumes you have downloaded the Excel series for the Forcibly Displaced Populations data set from
# the Center for Systemic Peace's INSCR Data Page and saved it to the working directory. The link address for that
# data set is:
#
# http://www.systemicpeace.org/inscr/FDP2008a.xls

require(XLConnect)
fdp <- readWorksheetFromFile("fdp2008a.xls", sheet=1)

# This step uses ddply to sum internally displaced persons and hosted refugees for the world each year. The years
# with no observations get zeroes, so we manually replace those with NAs.

require(plyr)
refugees <- ddply(fdp, .(year), summarise,
  idps = sum(idp, na.rm = TRUE),
  hosted = sum(host, na.rm = TRUE),
  total = idps + hosted)
# Replace false 0s with NAs
refugees$idps <- ifelse(refugees$idps == 0, NA, refugees$idps)
refugees$hosted <- ifelse(refugees$hosted == 0, NA, refugees$hosted)
refugees$total <- ifelse(refugees$total == 0, NA, refugees$total)

# This step uses the WDI package to ingest data directly from the World Bank's World Development Indicators, so
# no downloading required:

require(WDI)
pop <- WDI(country="1W", indicator = "SP.POP.TOTL", extra = FALSE, start = 1964, end = 2008)
pop$pop <- pop$SP.POP.TOTL/1000 # Rescale to 1,000s to match refugee data
refugees <- merge(refugees, subset(pop, select = c(year, pop)), all.x = TRUE)

# Then we generate the statistics on refugess as a share of the global population

refugees$total.pct <- 100 * (refugees$total / refugees$pop)

# Then we plot the data.

setwd("c:/users/jay/documents/blog posts/")
png(file = "global.refugees.png", width=12, height=6, units='cm', bg='white', res = 150)
par(mar=c(2,2,3,1))
par(cex.axis = 0.5)
par(cex.main = 0.75)
plot(refugees$total, type = "n", axes = FALSE, ylim = c(0,60000), ylab = "", xlab = "")
axis(1, at = seq(2,42,5), labels = seq(from = 1965, to = 2005, by = 5), tick = FALSE, las = 2, line = -1)
axis(2, at = seq(0, 60000, by = 10000), labels = seq(0, 60, 10), tick = FALSE, las = 2, line = -1)
abline(h = 0, lwd = 0.5, col = "gray")
abline(h = 10000, lwd = 0.5, col = "gray")
abline(h = 20000, lwd = 0.5, col = "gray")
abline(h = 30000, lwd = 0.5, col = "gray")
abline(h = 40000, lwd = 0.5, col = "gray")
abline(h = 50000, lwd = 0.5, col = "gray")
abline(h = 60000, lwd = 0.5, col = "gray")
abline(h = 51200, lwd = 0.5, col = "darkorange1")
text(x = 0, y = 53000, pos = 4, "2013 figure (51.2 million)", col = "darkorange1", cex = 0.5)
lines(refugees$total, lwd = 2, col = "cornflowerblue")
title(main = "Forcibly Displaced Persons Worldwide in Millions, 1964-2008")
dev.off()

png(file = "global.refugees.percent.png", width=12, height=6, units='cm', bg='white', res = 150)
par(mar=c(1.5,2,3,1))
par(cex.axis = 0.5)
par(cex.main = 0.75)
plot(refugees$total.pct, type = "n", axes = FALSE, ylim = c(0,1), ylab = "", xlab = "")
axis(1, at = seq(2,42,5), labels = seq(from = 1965, to = 2005, by = 5), tick = FALSE, las = 2, line = -1)
axis(2, at = seq(0, 1, by = 0.25), labels = c("0", "0.25%", "0.50%", "0.75%", "1.00%"), tick = FALSE, las = 2, line = -1)
abline(h = 0, lwd = 0.5, col = "gray")
abline(h = 0.25, lwd = 0.5, col = "gray")
abline(h = 0.5, lwd = 0.5, col = "gray")
abline(h = 0.75, lwd = 0.5, col = "gray")
abline(h = 1, lwd = 0.5, col = "gray")
abline(h = 0.72, lwd = 0.5, col = "darkorange1")
text(x = 0, y = 0.67, pos = 4, "2013 figure (~0.72%)", col = "darkorange1", cex = 0.5)
lines(refugees$total.pct, lwd = 2, col = "cornflowerblue")
title(main = "Forcibly Displaced Persons Worldwide \nas a Share of Global Population, 1964-2008")
dev.off()
