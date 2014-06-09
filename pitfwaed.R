# PITF WORLDWIDE ATROCITIES EVENT DATASET INGESTION
# 2014-06-05
# Jay Ulfelder

rm(list=ls(all=TRUE))

# NOTE: This script assumes:
#   a) That you have already extracted the spreadsheets from the downloaded .zip files; and
#   b) That you have set the working directory to the folder where those extracted files sit.
# Source files available at: http://eventdata.parusanalytics.com/data.dir/atrocities.html
# This script worked for files posted on 2014-06-05.

# These will need to be updated or expanded as future updates are posted.
old <- "pitf.world.19950101-20121231.xls"
recent <- "pitf.world.20130101-20140228.xls"

### IMPORT ###
require(XLConnect)
waed.hist <- readWorksheetFromFile(old, sheet = 1,
  startRow = 3, startCol = 1, endCol = 73) # Skip two meta-header rows, drop empty last column
waed.new <- readWorksheetFromFile(recent, sheet = 1,
  startRow = 3, startCol = 1, endCol = 73)
waed <- data.frame(rbind(waed.hist, waed.new))

### SCRUB ###

# Fix some country labels based on eyeballing in step 1
unique(waed$Country)
waed$Country[waed$Country=="\nIRQ" | waed$Country=="IRQ "] <- "IRQ"
waed$Country[waed$Country=="SYR "] <- "SYR"
waed$Country[waed$Country=="Somalia" | waed$Country=="SOM "] <- "SOM"
waed$Country[waed$Country=="Nigeria"] <- "NGA"
waed$Country[waed$Country=="South Sudan" | waed$Country=="South Sudan "] <- "SSD"

# Create a yearmon variable based on start time for crosstabbing. The convoluted code for Start.Month
# is required to deal with the fact that some months in the 1-9 range are given as "01" and others
# as "1".
waed$yearmon <- paste(waed$Start.Year,
  ifelse(as.numeric(waed$Start.Month) < 10 & substr(waed$Start.Month, 1, 1)!="0",
    paste0("0", waed$Start.Month), waed$Start.Month),
  sep = '-')

# Create a numeric version of death counts with NAs for ambiguous ones
waed$deathnum <- as.numeric(waed$Deaths.Number)

# Assign value of 1 to each type of event for summing, accounting for slight variations in labels seen in step 1
unique(waed$Event.Type)
waed$incident <- ifelse(waed$Event.Type=="incident" | waed$Event.Type=="Incident" | waed$Event.Type=="Incident ", 1, 0)
waed$campaign <- ifelse(waed$Event.Type=="Campaign" | waed$Event.Type=="Campaign ", 1, 0)

### SUMMARIZE ###

require(plyr)

# Country-month event and death counts by event type
waed.mo <- ddply(waed, .(Country, yearmon), summarise,
waed.mo <- ddply(waed, .(Country, yearmon), summarise,
  incidents.count = sum(incident),
  campaigns.count = sum(campaign),
  incident.deaths = sum(deathnum[incident==1], na.rm = TRUE),
  campaign.deaths = sum(deathnum[campaign==1], na.rm = TRUE),
  total.count = sum(incident) + sum(campaign),
  total.deaths = sum(deathnum, na.rm = TRUE))

# Create numeric versions of year & month for easier sorting & subsetting
waed.mo$year <- substr(waed.mo$yearmon, 1, 4)
waed.mo$month <- as.numeric(substr(waed.mo$yearmon, 6, 7))
waed.mo <- waed.mo[order(waed.mo$Country, waed.mo$year, waed.mo$month),]

# Merge into full country-month rectangle and fill in NAs with 0s
year <- rep(seq(1995, 2014), each = 12)
month <- rep(c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), times = length(year))
yearmonth <- paste(year, month, sep = '-')
countries <- unique(waed$Country)
countries <- countries[is.na(countries)==FALSE] # Get rid of NA, which trips up next step if it's in
rack <- expand.grid(countries, yearmonth)
names(rack) <- c("Country", "yearmonth")
rack$year <- substr(rack$yearmonth, 1, 4)
rack$month <- as.numeric(substr(rack$yearmonth, 6, 7))
rack$Country <- as.character(rack$Country) # Change type for merging
rack <- rack[order(rack$Country, rack$year, rack$month),]
rack <- subset(rack, year < 2014 | month <= 2, select = c(Country, year, month)) # Truncate to last available month
waed.mo.x <- merge(rack, waed.mo, all.x = TRUE)
waed.mo.x$yearmon <- paste(waed.mo.x$year,  # Fix the yearmon variable
  ifelse(as.numeric(waed.mo.x$month) < 10, paste0("0", waed.mo.x$month), waed.mo.x$month),
  sep = '-')
waed.mo.x[is.na(waed.mo.x)] <- 0  # Replace NAs from grid expansion with 0s


### PLOT ###
png(file = "deaths.monthly.somalia.png", width=12, height=6, units='cm', bg='white', res = 150)
par(mar=c(2,2,3,1))
par(cex.axis = 0.5)
par(cex.main = 0.75)
plot(waed.mo.x$incident.deaths[waed.mo.x$Country=="SOM"], type = "n", axes = FALSE, ylab = "", xlab = "")
axis(1, at = seq(1, 240, by = 12), labels = yr, tick = FALSE, las = 2, line = -1)
axis(2, at = seq(0, 250, by = 50), tick = FALSE, las = 2, line = -1)
abline(h = 0, lwd = 0.5, col = "gray")
abline(h = 50, lwd = 0.5, col = "gray")
abline(h = 100, lwd = 0.5, col = "gray")
abline(h = 150, lwd = 0.5, col = "gray")
abline(h = 200, lwd = 0.5, col = "gray")
abline(h = 250, lwd = 0.5, col = "gray")
lines(waed.mo.x$incident.deaths[waed.mo.x$Country=="SOM"], lwd = 2, col = "red")
title(main = "Monthly Sums of Deaths in Somalia \nfrom PITF Atrocities Event Dataset 'Incidents'")
dev.off()

png(file = "deaths.monthly.nigeria.png", width=12, height=6, units='cm', bg='white', res = 150)
par(mar=c(2,2,3,1))
par(cex.axis = 0.5)
par(cex.main = 0.75)
plot(waed.mo.x$incident.deaths[waed.mo.x$Country=="NGA"], type = "n", axes = FALSE, ylab = "", xlab = "")
axis(1, at = seq(1, 240, by = 12), labels = yr, tick = FALSE, las = 2, line = -1)
axis(2, at = seq(0, 600, by = 100), tick = FALSE, las = 2, line = -1)
abline(h = 0, lwd = 0.5, col = "gray")
abline(h = 100, lwd = 0.5, col = "gray")
abline(h = 200, lwd = 0.5, col = "gray")
abline(h = 300, lwd = 0.5, col = "gray")
abline(h = 400, lwd = 0.5, col = "gray")
abline(h = 500, lwd = 0.5, col = "gray")
abline(h = 600, lwd = 0.5, col = "gray")
lines(waed.mo.x$incident.deaths[waed.mo.x$Country=="NGA"], lwd = 2, col = "forestgreen")
title(main = "Monthly Sums of Deaths in Nigeria \nfrom PITF Atrocities Event Dataset 'Incidents'")
dev.off()
