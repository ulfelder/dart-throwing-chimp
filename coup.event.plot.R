library(dplyr)
library(countrycode)

# Data ingestion and transformation
Coups <- read.delim("http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt", stringsAsFactors=FALSE) %>% # fetch coup event list
  mutate(iso3c = countrycode(country, "country.name", "iso3c")) %>% # add iso3c country codes to use as labels in plot
  arrange(year, -coup, iso3c) %>% # arrange events by year, outcome, and country for upcoming plotting
  group_by(year) %>% # group by year...
  mutate(y = seq_along(coup)) # ...and then create cumulative count of events within each year to use as y coordinates in plot

# Hard-code some country codes that 'countrycode" can't get from names given
Coups$iso3c[Coups$country=="Yemen Arab Republic; N. Yemen"] <- "YAR"
Coups$iso3c[Coups$country=="Republic of Vietnam"] <- "RVN"

# Make the chart with production date embedded in filename
png(sprintf("stack.coups.%s.png", paste0(unlist(strsplit(as.character(Sys.Date()), "-")), collapse="")),
  width = 8, height = 2, units = "in", res = 300, bg = "white")
par(mai=c(0.25, 0.01, 0.1, 0.01), cex.axis = 0.4)
with(Coups, plot(x=year, y=y, type="n", axes=FALSE, xlab="", ylab=""))
with(Coups, text(x=year, y=y, labels=iso3c, cex=0.33, family="mono", col=ifelse(coup==2, "red", "black"), pos=3))
axis(1, at=seq(min(Coups$year), max(Coups$year)), las=2, pos=2.25, tick=FALSE)
segments(min(Coups$year) - 0.5, 1, max(Coups$year) + 0.5, 1, lwd=1.25, col="gray30")
legend(x=min(Coups$year), y=max(Coups$y), legend=c("successful", "failed"), fill=c("red", "gray40"),
  border=c("red", "gray40"), bty="n", cex=0.4, xjust=0, yjust=1)
dev.off()
