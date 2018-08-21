# This script gets the Fund for Peace's Fragile States Index data files from the web and makes
# a country-year table from them

library(tidyverse)
library(rvest)
library(readxl)
library(lubridate)

options(stringsAsFactors = FALSE)

# scrape the urls for the excel files from the FFP download page
fsi_urls <- read_html("http://fundforpeace.org/fsi/excel/") %>%
    # found this by inspecting the html in Chrome
    html_nodes("td") %>%
    # grab the urls from those nodes, which are basically long strings; I tried http_attr("html")
    # and a couple of variations, but it didn't work
    lapply(., function(x) str_extract(as.character(x), "http://fundforpeace.org/.{1,}.xlsx")) %>%
    # each url is repeated, so get rid of the duplicates
    unique(.) %>%
    # get rid of the remaining NA
    .[!is.na(.)] %>%
    # one of the duplicates was a jackelope for some reason, so we need this to get rid of it
    .[!grepl("a href", .)]

# download the excel files from those urls to tempfiles and read them into R as data tables
fsi_list <- lapply(fsi_urls, function(x) {
	
	tmp <- tempfile()
	
	download.file(x, tmp)
	
	df <- read_excel(tmp)
	
})

# collapse that list of annual data tables into one big data frame
fsi_db <- bind_rows(fsi_list)

# clean up the column names
names(fsi_db) <- names(fsi_db) %>%
    gsub("[A-Z]{1}[0-9]{1}: ", "", .) %>%
    tolower(.) %>%
    make.names(.)

# convert the date field to a simple year and drop the messy rank column
fsi_db <- fsi_db %>%
    mutate(year = lubridate::year(year)) %>%
    select(-rank)
