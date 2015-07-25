# This script can be used to automatically ingest and summarize PITF WorldWide Atrocities Event Data. It is designed to detect
# the relevant urls from the PITF WAED website, parse the relevant file names from those urls, download the zip files to
# temporary files, extract the desired Excel files from those zip archives into other temporary files, read those Excel files
# into R, merge them, clean up some data glitches, and summarize the data into some common cross-tabulations. If it works
# properly---and so far it does---the script should continue working even as the PITF WAED files are updated and their names
# change. If the structure of the website changes, however, the script will fail, because it will be looking for the urls in
# the wrong places.

# The script creates two data frames:
#    1. WAED: an event file containing all WAED events from 1 January 1995 through the latest update (usually 2-3 months ago)
#    2. WAED.cm: a country-month file with counts of incidents, people killed, and people injured in them 

# Load required packages

library(XML)
library(XLConnect)
library(dplyr)
library(tidyr)
library(stringr)
library(countrycode)

# SCRAPE

# Get urls and file names target files from PITF WAED website. This is clunkier and more fragile than the approach I used in
# the ACLED ingester, but that code didn't work on this website (Error 406), and this is what I got to work instead.

waed.page <- "http://eventdata.parusanalytics.com/data.dir/atrocities.html"

waed.child <- waed.page %>%
  htmlTreeParse(.) %>% # Parse the target page html into a tree structure
  xmlRoot(.) %>% # Get a list of the roots
  xmlChildren(.) # Get a list of the children of those roots

# Pull the desired text from the right node in the right child --- found by eyeballing waed.child[[2]] --- and paste it
# into a string with the rest of the url
waed.new.link <- paste0("http://eventdata.parusanalytics.com/data.dir/", unlist(getNodeSet(waed.child[[2]], "/body/div/div/div/p[7]/a[1]"))["attributes.href"])
waed.new.file <- sub(".zip", "", unlist(getNodeSet(waed.child[[2]], "/body/div/div/div/p[7]/a[1]"))["attributes.href"])

waed.old.link <- paste0("http://eventdata.parusanalytics.com/data.dir/", unlist(getNodeSet(waed.child[[2]], "/body/div/div/div/p[6]/a[1]"))["attributes.href"])
waed.old.file <- sub(".zip", "", unlist(getNodeSet(waed.child[[2]], "/body/div/div/div/p[6]/a[1]"))["attributes.href"])

# Now download those target archives, extract desired Excel files, and import results
# See: http://stackoverflow.com/questions/31589170/download-unzip-and-load-excel-file-in-r-using-tempfiles-only

# First, the historical data
tmp <- tempfile() # Create tempfile connection
download.file(waed.old.link, tmp) # Download the .zip file to that connection
tmp2 <- tempfile() # Create another connection
tmp2 <- unzip(zipfile=tmp, files = waed.old.file, exdir=tempdir()) # Unzip the specified file from the download to that slot
WAED.old <- readWorksheetFromFile(tmp2, sheet = 1, startRow = 3, startCol = 1, endCol = 73) # Deals w/odd formatting in Excel

# Then repeat with the latest version of the newer stuff
tmp3 <- tempfile()
download.file(waed.new.link, tmp3)
tmp4 <- tempfile()
tmp4 <- unzip(zipfile=tmp3, files = waed.new.file, exdir=tempdir())
WAED.new <- readWorksheetFromFile(tmp4, sheet = 1, startRow = 3, startCol = 1, endCol = 73)

# Merge the two files, keeping all rows from both
WAED <- merge(WAED.old, WAED.new, all = TRUE)

# CLEANING

# I don't like capitalized variable names
names(WAED) <- tolower(names(WAED))

# fix some errant country names spotted with unique(WAED$country)
WAED$country[WAED$country=="IRQ "] <- "IRQ"
WAED$country[WAED$country=="SYR "] <- "SYR"
WAED$country[WAED$country=="THL"] <- "THA" 
WAED$country[WAED$country=="SUD"] <- "SDN"
WAED$country[WAED$country=="TMP"] <- "TLS"
WAED$country[WAED$country=="Somalia" | WAED$country=="SOM "] <- "SOM"
WAED$country[WAED$country=="Nigeria" | WAED$country=="NGR"] <- "NGA"
WAED$country[WAED$country=="South Sudan" | WAED$country=="South Sudan "] <- "SSD"
# NOTE: SCG (Serbia and Montenegro before 2006) and GZS (Gaza Strip) are valid codes that 'countrycode' does not recognize

# Change type of selected variables to numeric
WAED <- WAED %>%
  mutate_each(funs(as.numeric), contains("day")) %>%
  mutate_each(funs(as.numeric), contains("month")) %>%
  mutate_each(funs(as.numeric), contains("year")) %>%
  mutate_each(funs(as.numeric), contains("degrees")) %>%
  mutate_each(funs(as.numeric), contains("minutes")) %>%
  mutate_each(funs(as.numeric), contains("seconds")) %>%
  mutate_each(funs(as.numeric), contains("distance")) %>%
  mutate_each(funs(as.numeric), contains("number"))

# Country-month summation on incidents only

enddate <- str_extract_all(waed.new.file, "[:digit:]+", simplify=TRUE)[,2]  # End date of last update; needed for filtering to come

WAED.cm <- WAED %>%
  filter(.,str_detect(tolower(event.type), "incident")) %>%  # filter down to incidents only, accounting for variation in labels; loses some info, but enhances comparability
  rename(., iso3c = country, year = start.year, month = start.month) %>%
  group_by(., iso3c, year, month) %>%  # group and order by country, year, and month when event started
  summarise(., incidents = n(), dead = sum(deaths.number, na.rm=TRUE), injured = sum(injured.number, na.rm=TRUE)) %>%  # create new df of event and casualty counts
  left_join(expand(., iso3c, year, month), .) %>% # Expand data frame to cover all country-months by left-joining tallies to complete series created with expand() from tidyr
  replace(is.na(.), 0) %>%  # Replace NAs created in last step with 0s
  filter(., year < as.numeric(substr(enddate, 1, 4)) | (year == as.numeric(substr(enddate, 1, 4)) & month <= as.numeric(substr(enddate, 5, 6)))) %>% # Drop rows for months that haven't happened yet
  mutate(., yearmo = as.Date(paste(year, ifelse(month < 10, paste0("0", month), month), "01", sep="-"))) %>%
  filter(., is.na(yearmo)==FALSE) %>% # drop rows with missing yearmo because of missing start month in WAED
  mutate(., country = countrycode(iso3c, "iso3c", "country.name", warn = FALSE)) # Use 'countrycode' to add country names based on COW numeric codes 
  
# Clean up the workspace
unlink(tmp)
unlink(tmp2)
unlink(tmp3)
unlink(tmp4)
