# This script will download, unzip, prep, and merge ACLED's Version 5 historical data (1997-2014) and its realtime
# data. The downside is that it will need to be modified as the realtime data are updated, or if the url or filename for
# the historical data changes. The upside is that those changes only need to be made in the "File info" section that
# starts on line 21. Unless the basic structure of the files or their variable names change, the rest should keep working.

# Load required packages, which are only used in country-month aggregation stage
library(dplyr)
library(tidyr)
library(countrycode)
library(ggplot2)

# Function to get zip file and extract csv using vector of two string objects and returning data frame
getfile <- function(vector) {
  temp <- tempfile()
  download.file(vector[1], temp)
  df <- read.csv(unz(temp, vector[2]), stringsAsFactors=FALSE)
  unlink(temp)
  return(df)
}

# Info on files to be ingested. To get this, I did the following on a Windows-driven PC:
# 1. Pointed my browser to http://www.acleddata.com/data/
# 2. Clicked on http://www.acleddata.com/data/version-5-data-1997-2014/ to get info on historical data
# 3. Right-clicked on the (csv) hyperlink for 'ACLED Version 5 (1997 – 2014) standard file' and selected 'Copy link address'
# 4. Used Ctrl-V to paste that in between quotation marks in the past.url slot below.
# 5. Left-clicked on that same link to download the .zip file
# 6. Double-clicked on the downloaded .zip file to inspect the contents
# 7. Right-clicked on the .csv in the resulting window, selected 'Properties', and used Ctrl-C to copy the csv file's name
# 8. Used Ctrl-V to paste that file name in between quotation marks in the past.file slot below
# 9. Back on the ACLED site, clicked on 'Realtime Data (2015)'
# 10. Repeated steps 3 through 8 for 'Realtime 2015 All Africa File (updated 11th July 2015)(csv)' and the realtime.* slots below

past.url <- "http://www.acleddata.com/wp-content/uploads/2015/06/ACLED-Version-5-All-Africa-1997-2014_dyadic_Updated_csv-no-notes.zip"
past.file <- "ACLED-Version-5-All-Africa-1997-2014_dyadic_Updated_no_notes.csv"

realtime.url <- "http://www.acleddata.com/wp-content/uploads/2015/07/ACLED-All-Africa-File_20150101-to-20150711_csv.zip"
realtime.file <- "ACLED All Africa File_20150101 to 20150711_csv.csv"

# Data fetching
ACLED.targets <- list(c(past.url, past.file), c(realtime.url, realtime.file)) # Make list of target dataset info
ACLED.list <- lapply(ACLED.targets, getfile) # Use function created above to ingest files into list form
names(ACLED.list[[1]]) <- sub("GEO_PRECIS", "GEO_PRECISION", names(ACLED.list[[1]])) # Fix name of var in Version 5 to match realtime
names(ACLED.list[[2]]) <- gsub("ADM_LEVEL_", "ADMIN", names(ACLED.list[[2]])) # Fix names of location vars to match Version 5
ACLED <- Reduce(function(...) merge(..., all=TRUE), ACLED.list) # Merge all files in the list, keeping all non-duplicate rows
names(ACLED) <- tolower(names(ACLED)) # Convert var names in merged file to lower case

# Inspect the result to make sure it's worked as expected
str(ACLED)

# Get country-month counts of each event type and add column counting all battles of any type
ACLED.cm <- ACLED %>%
  mutate(event_type = make.names(tolower(event_type))) %>% # Change event type labels for use as proper var names, and to deal with "Remote Violence", "Remote violence"
  mutate(month = as.numeric(substr(event_date, 4, 5))) %>%  # Create month var to use in grouping
  group_by(gwno, year, month, event_type) %>%  # Define groupings from highest to lowest level; data are automatically ordered accordingly
  tally(.) %>%  # Get counts of records in each group (i.e., each country/year/month/type subset)
  spread(., key = event_type, value = n, fill = 0) %>% # Make data wide by spreading event types into columns
  left_join(expand(., gwno, year, month), .) %>% # Expand data frame to cover all possible country-months by left-joining tallies to complete series created with expand() from tidyr
  replace(is.na(.), 0) %>%  # Replace all NAs created by that last step with 0s
  mutate(., battles = rowSums(select(., contains("battle")))) %>% # Create var summing counts of all battle types
  filter(., year < as.numeric(substr(Sys.Date(), 1, 4)) | (year == as.numeric(substr(Sys.Date(), 1, 4)) & month < as.numeric(substr(Sys.Date(), 6, 7)))) %>% # Drop rows for months that haven't happened yet
  mutate(., country = countrycode(gwno, "cown", "country.name", warn = FALSE)) # Use 'countrycode' to add country names based on COW numeric codes
  
# Some examples of time-series plots

# Montly counts of events involving violence against civilians in Burundi
ACLED.cm %>%
  filter(., country == "Burundi") %>%
  mutate(., yearmo = as.Date(paste(year, ifelse(month < 10, paste0("0", month), month), "01", sep="-"))) %>%
  qplot(data = ., x = yearmo, y = violence.against.civilians, geom = "line", xlab="Month", ylab="Event count") + ggtitle("Violence against civilians in Burundi") %>%
  print

# Monthly counts of battles by country (small multiples)
ACLED.cm %>%
  mutate(., yearmo = as.Date(paste(year, ifelse(month < 10, paste0("0", month), month), "01", sep="-"))) %>%
  qplot(data = ., x = yearmo, y = battles, facets = ~country, geom = "line", xlab="Month", ylab="Event count: battles")
  print
# If you'd rather save that plot to your hard drive, replace the 'print' line with one like this:
# ggsave("ACLED.battles.ts.png", path = "[path to directory in which to save it]", width = 6, height = 8, units="in")
