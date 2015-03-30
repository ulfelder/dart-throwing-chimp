# This script takes compressed event data files downloaded from the Integrated Crisis Early Warning System (ICEWS) archive
# on Dataverse and creates counts of events by type at the country-month level. It presumes that all of the compressed
# files are in the working directory, and that no other files with the relevant years in their names are in that directory.
# The archive is here:
#
# http://thedata.harvard.edu/dvn/dv/icews
#
# This script works on the files downloaded from the 'ICEWS Coded Event Data' part.

# Load required packages
library(plyr)
library(tidyr)

# Function to ingest data for a single year regardless of time stamp in name; add yr, mo, day vars; and
# use that to create a table with monthly counts by event type.
read.icews <- function(year) {

     # File names are time stamped, so we need to find a way to ingest them based on the constant
     # bit. The first line uses the year to pull the full name of the .zip archive from a list
     # of files in the working directory. The second line gets the name of the file inside that
     # archive. The last finally extracts and loads that file.
     zname <- list.files()[grep(as.character(year), list.files())]
     tname <- substr(zname, 1, 30)
     file <- read.delim(unz(zname, tname), stringsAsFactors = FALSE)

     # Now we'll add some variables to make later manipulation easier.
     file$year <- as.integer(substr(file$Event.Date, 1, 4))
     file$month <- as.integer(substr(file$Event.Date, 6, 7))
     file$day <- as.integer(substr(file$Event.Date, 9, 10))
     file$event <- 1  # Makes next step easier by giving simple thing to count

     # Now we'll use plyr to generate counts of each CAMEO event type by country-month. We'll
     # start in long format because that makes it easier to generate all the desired counts in
     # one go.
     require(plyr)
     counts.long <- ddply(file, .(Source.Country, year, month, CAMEO.Code), summarise,
          count = sum(event))
     names(counts.long) <- c("country", "year", "month", "code", "count")
     counts.long$country[counts.long$country == ""] <- "none assigned"

     # Now we'll use tidyr to convert that long file to wide format with useful names.
     require(tidyr)
     counts.wide <- spread(counts.long, code, count)
     names(counts.wide) <- c(names(counts.wide)[1:3],
          paste("cameo", names(counts.wide)[4:length(names(counts.wide))], sep = "."))

     return(counts.wide)
}

# I'm sure there's a more efficient way to do the rest of this, but I couldn't quickly see my way to it, so: iterate
# that function over the available complete years...
I.95 <- read.icews(1995)
I.96 <- read.icews(1996)
I.97 <- read.icews(1997)
I.98 <- read.icews(1998)
I.99 <- read.icews(1999)
I.00 <- read.icews(2000)
I.01 <- read.icews(2001)
I.02 <- read.icews(2002)
I.03 <- read.icews(2003)
I.04 <- read.icews(2004)
I.05 <- read.icews(2005)
I.06 <- read.icews(2006)
I.07 <- read.icews(2007)
I.08 <- read.icews(2008)
I.09 <- read.icews(2009)
I.10 <- read.icews(2010)
I.11 <- read.icews(2011)
I.12 <- read.icews(2012)
I.13 <- read.icews(2013)

# ...and then merge all of those sequentially.
rollup <- merge(I.95, I.96, all = TRUE)
rollup <- merge(rollup, I.97, all = TRUE)
rollup <- merge(rollup, I.98, all = TRUE)
rollup <- merge(rollup, I.99, all = TRUE)
rollup <- merge(rollup, I.00, all = TRUE)
rollup <- merge(rollup, I.01, all = TRUE)
rollup <- merge(rollup, I.02, all = TRUE)
rollup <- merge(rollup, I.03, all = TRUE)
rollup <- merge(rollup, I.04, all = TRUE)
rollup <- merge(rollup, I.05, all = TRUE)
rollup <- merge(rollup, I.06, all = TRUE)
rollup <- merge(rollup, I.07, all = TRUE)
rollup <- merge(rollup, I.08, all = TRUE)
rollup <- merge(rollup, I.09, all = TRUE)
rollup <- merge(rollup, I.10, all = TRUE)
rollup <- merge(rollup, I.11, all = TRUE)
rollup <- merge(rollup, I.12, all = TRUE)
rollup <- merge(rollup, I.13, all = TRUE)

# The data set we have now created does not include rows for countries that have no
# events. To make sure we have observations for all country-months, we need to create
# a dummy grid with rows for all of them, then merge our observed counts with it. At
# the end, we replace all the NAs in that grid with zeros, and voila.
countries <- unique(rollup$country)
years <- c(min(rollup$year):max(rollup$year))
months <- c(1:12)
dummy <- data.frame(rep(countries, each = 12, times = length(years)),
     rep(years, each = 12 * length(countries)),
     rep(months, times = length(countries) * length(years)),
     stringsAsFactors = FALSE )
names(dummy) <- c("country", "year", "month")
whole <- merge(dummy, rollup, all.x = TRUE)
whole[is.na(whole)] <- 0

# Write that out
write.csv(whole, "icews.country.month.counts.csv", row.names = FALSE)
