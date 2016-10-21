# a script to scrape the country-year version of Polity from the CSP web site

library(dplyr)
library(rvest)
library(stringr)
library(readxl)

# Get url for latest version of Polity in .xls format by parsing the CSP data page

 # Provide url of page that hosts data set
Polity.url <- "http://www.systemicpeace.org/inscrdata.html" %>% 

  # Parse the html for the relevant page
  read_html(.) %>%
  
  # Identify all the hyperlinks in the results
  html_nodes("a") %>%               
  
  # Extract the urls for those hyperlinks as a vector of strings
  html_attr("href") %>%
  
  # Get url for annual file, hopefully robust to coming change to Version 5
  str_subset("p[0-9]{1}v[0-9]{4}\\.xls")

# Download the .xls file from that url to a temp path and ingest it from there
temp <- paste0(tempfile(), ".xls")
download.file(Polity.url, destfile = temp, mode = "wb")
Polity <- read_excel(path = temp)

# Correct several scodes to match PITF standard
Polity$scode[Polity$scode=="SER"] <- "SRB"
Polity$scode[Polity$scode=="MNT"] <- "MNE"
Polity$scode[Polity$scode=="GMY"] <- "GER"
Polity$scode[Polity$scode=="SSU"] <- "SSD"
Polity$scode[Polity$scode=="SDN"] <- "SUD"
Polity$scode[Polity$scode=="USR"] <- "USS"
