# This script gets the Fund for Peace's Failed States Index data files from the web and makes a country-year
# table from them

library(dplyr)
library(readxl)
library(countrycode)

url.archive <- "http://fsi.fundforpeace.org/library/fragilestatesindex-2006to2014.xlsx"
url.2015 <- "http://fsi.fundforpeace.org/library/fragilestatesindex-2015.xlsx"
url.2016 <- "http://fsi.fundforpeace.org/library/fragilestatesindex-2016.xlsx"

archive.years <- seq(2014, 2006)

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
temp <- paste0(tempfile(), ".xlsx")
download.file(url.2015, destfile = temp, mode = "wb")
FSI.2015 <- read_excel(path = temp)
unlink(temp)
names(FSI.2015) <- fsivars
FSI.2015$year <- 2015
FSI.2015$rank <- NULL

temp <- paste0(tempfile(), ".xlsx")
download.file(url.2016, destfile = temp, mode = "wb")
FSI.2016 <- read_excel(path = temp)
unlink(temp)
names(FSI.2016) <- fsivars
FSI.2016$year <- 2015
FSI.2016$rank <- NULL

# put it all together
FSI <- bind_rows(list(FSI.archive, FSI.2015, FSI.2016)) %>%
  arrange(country, year) %>%
  filter(country != "AVERAGE" & !is.na(country))

# fix some country names before adding codes; got this with manual check
FSI$country[FSI$country == "Dijbouti"] <- "Djibouti"
FSI$country[FSI$country == "Serbia and Montenegro"] <- "Serbia"
FSI$country[FSI$country == "Trinadad"] <- "Trinidad and Tobago"

# add country code
FSI$code <- countrycode::countrycode(FSI$country, "country.name", "iso3c")

# reorder columns and drop country name to avoid merging hassles
FSI <- FSI %>%
  select(code, year, everything()) %>%
  select(-country)
