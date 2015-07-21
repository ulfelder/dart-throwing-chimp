# This script can be used to automatically ingest and summarize PITF WorldWide Atrocities Event Data. It is designed to detect the relevant
# urls from the PITF WAED website, parse the relevant file names from those urls, download the zip files to the working directory, extract the
# desired Excel files from those zip archives to the working directory, read those Excel files into R, merge them, clean up some country
# names, and summarize the data into some common cross-tabulations. If it works properly---and so far it does---the script should continue
# working even as the PITF WAED files are updated and their names change. If the structure of the website changes, however, the script will
# fail, because it will be looking for the urls in the wrong places.

# NOTE: This script will write the .zip archives and the files extracted from them (~20MB total) to your working directory. So far, I haven't
# been able to figure out how to code a version that doesn't.

# Load required packages

library(XML)
library(XLConnect)
library(downloader)
library(dplyr)
library(tidyr)
library(stringr)
library(countrycode)

# Get urls and file names target files from PITF WAED website
# This helped: http://gastonsanchez.com/work/webdata/getting_web_data_r4_parsing_xml_html.pdf

waed.page <- "http://eventdata.parusanalytics.com/data.dir/atrocities.html"

waed.child <- waed.page %>%
  htmlTreeParse(.) %>%
  xmlRoot(.) %>%
  xmlChildren(.)

waed.new.link <- paste0("http://eventdata.parusanalytics.com/data.dir/", unlist(getNodeSet(waed.child[[2]], "/body/div/div/div/p[7]/a[1]"))["attributes.href"])
waed.new.file <- sub(".zip", "", unlist(getNodeSet(waed.child[[2]], "/body/div/div/div/p[7]/a[1]"))["attributes.href"])

waed.old.link <- paste0("http://eventdata.parusanalytics.com/data.dir/", unlist(getNodeSet(waed.child[[2]], "/body/div/div/div/p[6]/a[1]"))["attributes.href"])
waed.old.file <- sub(".zip", "", unlist(getNodeSet(waed.child[[2]], "/body/div/div/div/p[6]/a[1]"))["attributes.href"])

# Now download those target archives, extract desired Excel files, and import results
# See: http://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data

download(waed.old.link, dest="dataset.zip", mode="wb")
unzip("dataset.zip", files = waed.old.file)

download(waed.new.link, dest="dataset.2.zip", mode="wb")
unzip("dataset.2.zip", files = waed.new.file)

# Now ingest, merge, and clean the data tables

# Import, accounting for idiosynracies of these sheets, and merge in one go
WAED <- merge(readWorksheetFromFile(waed.old.file, sheet = 1, startRow = 3, startCol = 1, endCol = 73),
  readWorksheetFromFile(waed.new.file, sheet = 1, startRow = 3, startCol = 1, endCol = 73),
  all=TRUE)

# I don't like capitalized variable names
names(WAED) <- tolower(names(WAED))

# fix some errant country names spotted with unique(WAED$country)
WAED$country[WAED$country=="IRQ "] <- "IRQ"
WAED$country[WAED$country=="SYR "] <- "SYR"
WAED$country[WAED$country=="Somalia" | WAED$country=="SOM "] <- "SOM"
WAED$country[WAED$country=="Nigeria"] <- "NGA"
WAED$country[WAED$country=="South Sudan" | WAED$country=="South Sudan "] <- "SSD"

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
  mutate(., country = countrycode(iso3c, "iso3c", "country.name", warn = FALSE)) # Use 'countrycode' to add country names based on COW numeric codes 
  
# Clean up the workspace
rm(enddate, waed.child, waed.new.file, waed.new.link, waed.old.file, waed.old.link, waed.page)
