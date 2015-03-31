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

# Apply that function to desired range of years and merge the results; thanks to Gonzalo Rivero for showing me this 
# efficient approach.
yrdata <- llply(1995:2013, read.icews)
rollup <- Reduce(function(...) merge(..., all=TRUE), yrdata)

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

# Add column with count of all events per country-month for possible use in normalization
whole$total <- rowSums(whole[,4:dim(whole)[2]])

# Write that out
write.csv(whole, "icews.country.month.counts.csv", row.names = FALSE)

# If you want to merge the ICEWS Events of Interest Ground Truth Data with that table, you can do this. NOTE: The first bit,
# which ingests the ground truth file, will need a proper file path. That file should NOT be stored in the same directory
# as the event data files, or else those years in the ground-truth file name will screw up the ingestion of the event
# data.
GT <- read.csv("gtds_2001.to.feb.2014.csv", stringsAsFactors = FALSE) 
GT$country <- tolower(GT$country)
whole$country <- tolower(whole$country)
mismatches <- unique(GT$country)[which(!unique(GT$country) %in% unique(whole$country))] # Short lists mismatches
unique(whole$country) # Used to find the required replacements by eye
GT$country[GT$country == mismatches[1]] <- "cote d'ivoire"
GT$country[GT$country == mismatches[2]] <- "libya"
GT$country[GT$country == mismatches[3]] <- "syria"
GT <- subset(GT, year < 2014, select = c(country, year, month, ins, reb, dpc, erv, ic))
whole <- merge(whole, GT, all.x = TRUE)
