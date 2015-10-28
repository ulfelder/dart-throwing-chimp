# This script creates a function, f.countryyears(), that produces a table of country-years with a number of id codes
# from the selected start and end points. It uses the Polity IV data set as its foundation, so it can't go back past
# 1800 or forward past the last Polity update, usually the end of the previous calendar year. The function depends on
# several of Hadley Wickham's packages, so be sure to install those before running. To add other country codes such as
# ISO3c, use the 'countrycode' package.

f.countryyears <- function(start = 1800, end = as.numeric(substr(Sys.Date(), 1, 4)) - 1) {

  # Package dependencies

  require(dplyr)
  require(rvest)
  require(stringr)
  require(readxl)
  require(countrycode)

  # Get url for latest version of Polity in .xls format by parsing the CSP data page

  Polity.url <- "http://www.systemicpeace.org/inscrdata.html" %>%    # Provide url of page that hosts data set
    read_html(.) %>%                                                 # Parse the html for the relevant page
    html_nodes("a") %>%                                              # Identify all the hyperlinks in the results
    html_attr("href") %>%                                            # Extract the urls for those hyperlinks as a vector of strings
    str_subset("p4v[0-9]{4}\\.xls")                                  # Get the Excel version without the "d" before the extension

  # Download the .xls file from that url to a temp path and use 'readxl' to ingest it from there
  
  temp <- paste0(tempfile(), ".xls")
  download.file(Polity.url, destfile = temp, mode = "wb")
  Polity <- read_excel(path = temp)

  # Reduce the resulting data frame to id variables and selected years

  CY <- Polity %>%
    select(country, ccode, pitfcode = scode, year) %>%  # Change name of scode to pitfcode so it's clearer what it is
    filter(year >= start & year <= end)

  # Correct several pitfcodes to match PITF standard

  CY$pitfcode[CY$pitfcode=="SER"] <- "SRB"
  CY$pitfcode[CY$pitfcode=="MNT"] <- "MNE"
  CY$pitfcode[CY$pitfcode=="GMY"] <- "GER"
  CY$pitfcode[CY$pitfcode=="SSU"] <- "SSD"
  CY$pitfcode[CY$pitfcode=="SDN"] <- "SUD"
  CY$pitfcode[CY$pitfcode=="USR"] <- "USS"

  # Return the results

  CY <- as.data.frame(CY)
  return(CY)

}
