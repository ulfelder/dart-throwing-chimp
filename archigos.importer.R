# INGEST ARCHIGOS

# Load required packages
library(rvest)
library(stringr)
library(dplyr)
library(tidyr)

# Function to source scripts from GitHub
source_github <- function(u) {
  require(RCurl)
  script <- getURL(u, ssl.verifypeer=FALSE)
  eval(parse(text = script), envir=.GlobalEnv)
}

# Use source_github() to get f.countryyears(), a function to make a table of country-years
source_github("https://raw.githubusercontent.com/ulfelder/dart-throwing-chimp/master/f.countryyears.R")

# Get the Archigos data set, which is a spell file of leaders' tenures
Archigos <- "http://privatewww.essex.ac.uk/~ksg/archigos.html" %>%        # Provide url of page that hosts data set
  read_html(.) %>%                                                        # Parse the html for the relevant page
  html_nodes("a") %>%                                                     # Identify all the hyperlinks in the results
  html_attr("href") %>%                                                   # Extract the urls for those hyperlinks as a vector of strings
  str_subset("\\.txt") %>%                                                # Trim down to the one we want, which incls ".txt"
  paste0("http://privatewww.essex.ac.uk/~ksg/", .) %>%                    # Add the leading portion of full url
  read.delim(., stringsAsFactors=FALSE)                                   # Ingest tab-delimited txt file from that link

# Compute tenure times in days, weeks, and years
Archigos$start <- as.POSIXct(Archigos$startdate)
Archigos$end <- as.POSIXct(Archigos$enddate)
Archigos$duration.days = with(Archigos, as.numeric(difftime(end, start, unit = "days")))
Archigos$duration.weeks = with(Archigos, as.numeric(difftime(end, start, unit = "weeks")))
Archigos$duration.years = Archigos$duration.days/365

# Entry and exit modes as factors
Archigos$entry = as.factor(Archigos$entry)
Archigos$exit = as.factor(Archigos$exit)

# Get counts of exit types by country-year
Archigos.annual.exits <- Archigos %>%
  mutate(year = as.numeric(substr(as.character(end), 1, 4))) %>%
  group_by(ccode, year, exit) %>%
  tally() %>%
  spread(., key = exit, value = n, fill = NA)
# Make better variable names
names(Archigos.annual.exits)[3:length(names(Archigos.annual.exits))] <- paste("exit",
  make.names(tolower(names(Archigos.annual.exits)[3:length(names(Archigos.annual.exits))])), sep = ".")

# Get counts of entry types by country-year
Archigos.annual.entries <- Archigos %>%
  mutate(year = as.numeric(substr(as.character(start), 1, 4))) %>%
  group_by(ccode, year, entry) %>%
  tally() %>%
  spread(., key = entry, value = n, fill = NA)
# Make better variable names
names(Archigos.annual.entries)[3:length(names(Archigos.annual.entries))] <- paste("entry",
  make.names(tolower(names(Archigos.annual.entries)[3:length(names(Archigos.annual.entries))])), sep = ".")
  
# Merge those counts with a table of country-years derived from Polity using f.countryyears()
Archigos.annual <- f.countryyears(1875, max(Archigos.annual.exits$year)) %>%
  left_join(., Archigos.annual.entries) %>%
  left_join(., Archigos.annual.exits)

# Replace the NAs with 0s
Archigos.annual[is.na(Archigos.annual)] <- 0

# Get rid of the interim summary tables
rm(Archigos.annual.entries, Archigos.annual.exits)
