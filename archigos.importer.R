# This script ingests the Archigos data set, on the tenures of national political leaders, from the web and creates
# two data frames in the current environment:
#
# 1. Archigos: A data frame in the original spell file format, i.e., one row for each leader episode with start and
# end dates for their time in office, plus measures of episode duration in days, weeks, and years.
#
# 2. Archigos.annual: A country-year summary version that contains counts of leader entries and exits by mode
# (e.g., regular vs. irregular entry) for each country for every year (1875-2014 at the moment).
#
# Note that the construction of Archigos.annual depends on a custom function that is pulled from GitHub
# (f.countryyears) and another (source_github) that is created at the start of the script. The former works
# right now on Windows and Mac, but you never know when these things will break. So, caveat emptor.
#
# For more on Archigos, see http://privatewww.essex.ac.uk/~ksg/archigos.html

# Load required packages
library(RCurl)
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

# Use source_github() to get f.countryyears(), a function to make a table of country-years from Polity IV
source_github("https://raw.githubusercontent.com/ulfelder/dart-throwing-chimp/master/f.countryyears.R")

# Get the Archigos data set, which is a spell file of leaders' tenures
Archigos <- "http://privatewww.essex.ac.uk/~ksg/archigos.html" %>%        # Provide url of page that hosts data set
  read_html(.) %>%                                                        # Parse the html for the relevant page
  html_nodes("a") %>%                                                     # Identify all the hyperlinks in the results
  html_attr("href") %>%                                                   # Extract the urls for those hyperlinks as a vector of strings
  str_subset("\\.txt") %>%                                                # Trim down to the one we want, which incls ".txt"...
  str_subset("Archigos") %>%                                              # ...and "Archigos"
  paste0("http://privatewww.essex.ac.uk/~ksg/", .) %>%                    # Add the leading portion of full url
  read.delim(., stringsAsFactors=FALSE)                                   # Ingest tab-delimited txt file from that link

# Compute tenure times in days, weeks, and years
Archigos$start <- as.POSIXct(Archigos$startdate)
Archigos$end <- as.POSIXct(Archigos$enddate)
Archigos$duration.days = with(Archigos, as.numeric(difftime(end, start, unit = "days")))
Archigos$duration.weeks = with(Archigos, as.numeric(difftime(end, start, unit = "weeks")))
Archigos$duration.years = Archigos$duration.days/365

# Set variables for entry and exit modes to factor type
Archigos$entry = as.factor(Archigos$entry)
Archigos$exit = as.factor(Archigos$exit)

# Get counts of exit events by country-year
Archigos.annual.exits <- Archigos %>%
  mutate(year = year(end)) %>%
  group_by(ccode, year, exit) %>%
  tally() %>%
  spread(., key = exit, value = n, fill = NA)
# Make better variable names
names(Archigos.annual.exits)[3:length(names(Archigos.annual.exits))] <- paste("exit",
  make.names(tolower(names(Archigos.annual.exits)[3:length(names(Archigos.annual.exits))])), sep = ".")

# Get counts of entry events by country-year
Archigos.annual.entries <- Archigos %>%
  mutate(year = year(start)) %>%
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

### MONTHLY

# Get counts of exit events by country-month
Archigos.monthly.exits <- Archigos %>%
  mutate(yrmo = as.yearmon(end)) %>%
  group_by(ccode, yrmo, exit) %>%
  tally() %>%
  spread(., key = exit, value = n, fill = NA)
# Make better variable names
names(Archigos.monthly.exits)[3:length(names(Archigos.monthly.exits))] <- paste("exit",
  make.names(tolower(names(Archigos.monthly.exits)[3:length(names(Archigos.monthly.exits))])), sep = ".")
Archigos.monthly.exits$iso3c <- countrycode(Archigos.monthly.exits$ccode, "cown", "iso3c")

# Get counts of entry events by country-year
Archigos.monthly.entries <- Archigos %>%
  mutate(yrmo = as.yearmon(start)) %>%
  group_by(ccode, yrmo, entry) %>%
  tally() %>%
  spread(., key = entry, value = n, fill = NA)
# Make better variable names
names(Archigos.monthly.entries)[3:length(names(Archigos.monthly.entries))] <- paste("entry",
  make.names(tolower(names(Archigos.monthly.entries)[3:length(names(Archigos.monthly.entries))])), sep = ".")
Archigos.monthly.entries$iso3c <- countrycode(Archigos.monthly.entries$ccode, "cown", "iso3c")
  
# Merge those counts with a table of country-years derived from Polity using f.countryyears()
Archigos.monthly <- f.yrmorack("1875-01-01",
                               sprintf("%s-12-31", max(year(Archigos$end)))) %>%
  mutate(yrmo = as.yearmon(paste(year, month, sep = "-"))) %>%
  select(-year, -month) %>%
  left_join(., Archigos.monthly.entries) %>%
  left_join(., Archigos.monthly.exits) %>%
  select(-ccode)

# Replace the NAs with 0s
Archigos.monthly[is.na(Archigos.monthly)] <- 0

# Get rid of the interim summary tables
rm(Archigos.monthly.entries, Archigos.monthly.exits)
