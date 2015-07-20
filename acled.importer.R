# See https://dartthrowingchimp.wordpress.com/2015/07/19/acled-in-r/ for a blog post discussing this script.

# This script will download, unzip, prep, and merge ACLED's Version 5 historical data (1997-2014) and its realtime
# data. It is designed to keep working as ACLED posts weekly updates; instead of calling fixed addresses and file names,
# it scrapes the relevant link addresses from ACLED's site and parses the names of those .zip files to guess at the correct
# name of the .csv files they contain. As such, it should keep working throughout 2015, as long as ACLED does not change
# the layout or structure of their web site or the conventions by which they name those files. It will almost certainly need
# to be updated in early 2016 when the URL for ACLED's Realtime page changes and then again when the next annual update of
# the historical data is completed and posted.

# If any of those things do change, the script will fail at the "Data fetching" step, if not sooner. If that happens, you
# should be able to work around the problem by hard-coding the past.url, past.file, realtime.url, and realtime.file objects
# according to the steps given in the comments at the bottom of the script and then re-running the script starting at
# the "Data fetching" step.

# Also note that the csv of the historical data does not include the Notes field because special characters in those fields
# make it difficult to read cleanly. If you want to see the notes, you need to download the .xslx version, which includes
# that column. Once downloaded, you can read that spreadsheet into R with these lines:
# library(readxl)
# ACLED.v5.xlsx <- read_excel("ACLED-Version-5-All-Africa-1997-2014_dyadic_Update.xlsx", 1,
#  col_types = c("numeric", "text", "numeric", "date", rep("numeric",2), rep("text",3), "numeric", rep("text",2),
#  rep("numeric",2), rep("text",5), rep("numeric",3), rep("text",2), "numeric")) 

# Load required packages
library(lubridate)
library(rvest)
library(dplyr)
library(tidyr)
library(countrycode)
library(ggplot2)

# This block of code pulls the link address for the historical data from the ACLED web site and directs the download there.
# Unfortunately, the name of the .csv file in that zip archive is not a direct derivation of the link address, so I am leaving
# that part hard-coded for now. That means it should work for the rest of 2015, as long as ACLED doesn't rearrange or rename
# the page, but the script will need to be updated in 2016. This block and the one that follow depend on 'rvest'.
past.page <- "http://www.acleddata.com/data/version-5-data-1997-2014/"
past.html <- html(past.page)
past.link <- html_node(past.html, xpath = "/html/body/div/div/div/div/div/article/div/p[4]/a[2]")
past.url <- html_attr(past.link, "href")
past.file <- "ACLED-Version-5-All-Africa-1997-2014_dyadic_Updated_no_notes.csv"

# This gets link address for latest zipped realtime csv from the ACLED website and parses it to guess at the name of the
# .csv file inside.
realtime.page <- "http://www.acleddata.com/data/realtime-data-2015/"
realtime.html <- html(realtime.page)
realtime.link <- html_node(realtime.html, xpath = "/html/body/div/div/div/div[1]/div/article/div/ul[1]/li[2]/a")
realtime.url <- html_attr(realtime.link, "href")
realtime.file <- gsub("-", " ", sub("zip", "csv", substr(realtime.url, 53, nchar(realtime.url))))

# Function to get zip file and extract csv using vector of two string objects and returning data frame
getfile <- function(vector) {
  temp <- tempfile()
  download.file(vector[1], temp)
  df <- read.csv(unz(temp, vector[2]), stringsAsFactors=FALSE)
  unlink(temp)
  return(df)
}

# Data fetching
ACLED.targets <- list(c(past.url, past.file), c(realtime.url, realtime.file)) # Make list of target dataset info
ACLED.list <- lapply(ACLED.targets, getfile) # Use function created above to ingest files into list form
names(ACLED.list[[1]]) <- sub("GEO_PRECIS", "GEO_PRECISION", names(ACLED.list[[1]])) # Change name of var in Version 5 to match realtime
names(ACLED.list[[2]]) <- gsub("ADM_LEVEL_", "ADMIN", names(ACLED.list[[2]])) # Change names of location vars to match Version 5
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
  
# NOT RUN: Some examples of time-series plots

# Montly counts of events involving violence against civilians in Burundi
# ACLED.cm %>%
#  filter(., country == "Burundi") %>%
#  mutate(., yearmo = as.Date(paste(year, ifelse(month < 10, paste0("0", month), month), "01", sep="-"))) %>%
#  qplot(data = ., x = yearmo, y = violence.against.civilians, geom = "line", xlab="Month", ylab="Event count") + ggtitle("Violence against civilians in Burundi") %>%
#  print

# Monthly counts of battles by country (small multiples)
# ACLED.cm %>%
#  mutate(., yearmo = as.Date(paste(year, ifelse(month < 10, paste0("0", month), month), "01", sep="-"))) %>%
#  qplot(data = ., x = yearmo, y = battles, facets = ~country, geom = "line", xlab="Month", ylab="Event count: battles") %>%
#  print
# If you'd rather save that plot to your hard drive, replace the 'print %>%' line with one like this:
# ggsave("ACLED.battles.ts.png", path = "[path to directory in which to save it]", width = 6, height = 8, units="in")

# To hard-code the link addresses and file names used in the "Data fetching" step, I did the following on a Windows PC:
# 1. Pointed my browser to the ACLED home page: http://www.acleddata.com/
# 2. Clicked on the Data tab, which took me to: http://www.acleddata.com/data/
# 3. Clicked on "ACLED Version 5 (1997-2014)" under Africa Data, which took me to: http://www.acleddata.com/data/version-5-data-1997-2014/
# 4. Right-clicked on the (csv) option for 'ACLED Version 5 (1997 – 2014) standard file' and selected 'Copy link address'
# 5. Used Ctrl-V to paste that in the past.url slot, .e.g, past.url <- "[paste here]"
# 6. Left-clicked on that same link to download the .zip file
# 7. Double-clicked on the downloaded .zip file to inspect the contents
# 8. Right-clicked on the .csv in the resulting window, selected 'Properties', and used Ctrl-C to copy the csv file's name
# 9. Used Ctrl-V to paste that file name in the past.file slot, e.g., past.file <- "[paste here]"
# 10. Back on the ACLED site, clicked on 'Realtime Data (2015)'
# 11. Repeated steps 4-9 for 'Realtime 2015 All Africa File (updated 11th July 2015)(csv)' and pasted the results in
#     the realtime.url and realtime.file slots
