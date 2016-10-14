# This script gets the Fund for Peace's Failed States Index data files from the web and makes a country-year
# table from them

library(dplyr)
library(stringr)
library(readxl)
library(countrycode)

url.archive <- "http://fsi.fundforpeace.org/library/fragilestatesindex-2006to2014.xlsx"
url.2015 <- "http://fsi.fundforpeace.org/library/fragilestatesindex-2015.xlsx"
url.2016 <- "http://fsi.fundforpeace.org/library/fragilestatesindex-2016.xlsx"

# get vector of years covered by archive sheet from file name to use for ingesting it
archive.years <- as.numeric(str_extract_all(url.archive, "\\(?[0-9]+\\)?")[[1]])
archive.years <- seq(archive.years[1], archive.years[2])

# vector of cleaner names to give to columns as ingested
fsivars <- c("rank", "country", "fsi.total", "fsi.demographic.pressures", "fsi.refugees",
             "fsi.group.grievance", "fsi.human.flight", "fsi.uneven.development", 
             "fsi.poverty", "fsi.legitimacy", "fsi.services", "fsi.human.rights",
             "fsi.security", "fsi.faction", "fsi.intervention")

# archived years
temp <- paste0(tempfile(), ".xlsx")

download.file(url.archive, destfile = temp, mode = "wb")

FSI.archive <- lapply(archive.years, function(i) {

  DF <- read_excel(path = temp, sheet = match(i, archive.years))

  names(DF) <- fsivars

  DF$year <- i

  # for some reason this was causing trouble, and it's easy to recreate, so...buh-bye
  DF$rank <- NULL

  return(DF)

})

FSI.archive <- bind_rows(FSI.archive)

# standalone years

fetchFSI <- function(year) {

  url <- sprintf("http://fsi.fundforpeace.org/library/fragilestatesindex-%s.xlsx", year)

  temp <- paste0(tempfile(), ".xlsx")
  
  download.file(url, destfile = temp, mode = "wb")

  FSI <- read_excel(path = temp)

  unlink(temp)

  names(FSI) <- fsivars

  FSI$year <- year

  FSI$rank <- NULL

  return(FSI)

}

FSI.2015 <- fetchFSI(2015)
FSI.2016 <- fetchFSI(2016)

# put it all together
FSI <- bind_rows(list(FSI.archive, FSI.2015, FSI.2016)) %>%
  arrange(country, year) %>%
  filter(country != "AVERAGE" & !is.na(country))

# fix some country name typos before adding codes; got this with manual check
# for rows where is.na(code) after running countrycode
FSI$country[FSI$country == "Dijbouti"] <- "Djibouti"
FSI$country[FSI$country == "Serbia and Montenegro"] <- "Serbia"
FSI$country[FSI$country == "Trinadad"] <- "Trinidad and Tobago"

# add country code, reorder columns, drop country name to avoid merging hassles, 
# and recreate ranking variable
FSI <- FSI %>%
  mutate(code = countrycode::countrycode(country, "country.name", "iso3c")) %>%
  select(-country) %>%
  group_by(year) %>%
  arrange(-fsi.total) %>%
  mutate(rank = seq_along(fsi.total)) %>%
  select(code, year, rank, everything()) %>%
  ungroup() %>%
  arrange(code, year)
