require(dplyr)
require(tidyr)
require(stringr)
require(readxl)
require(lubridate)
require(countrycode)

# load all acled events based on range of years; defaults to whole data set
acledr.years <- function(start = 1997, end = as.numeric(substr(Sys.Date(), 1, 4))) {

  require(dplyr)

  years <- seq(start, end)

  List <- lapply(years, function(x) {

    read.csv(sprintf("http://api.acleddata.com/acled/read.csv?limit=0&year=%d", x),
             stringsAsFactors = FALSE)

  })

  DF <- bind_rows(List) %>%
    mutate(event_date = as.Date(event_date, '%d/%m/%Y')) %>%
    arrange(event_date, iso, event_type, actor1, actor2)

}

# load all acled data for a single country; defaults to Nigeria
acledr.country <- function(country = "Nigeria") {

  code <- countrycode(country, "country.name", "iso3n")

  url <- sprintf("http://api.acleddata.com/acled/read.csv?limit=0&iso=%d", code)

  DF <- read.csv(url, stringsAsFactors = FALSE) %>%
    mutate(event_date = as.Date(event_date, '%d/%m/%Y')) %>%
    arrange(event_date, event_type, actor1, actor2)

}

# summarize downloaded ACLED event file in country-month counts
acledr.como <- function(acleddata) {

  require(dplyr)
  require(readxl)
  require(lubridate)
  require(stringr)

  # load and prep standalone table of start and end dates of coverage by country
  tmp <- paste0(tempfile(), ".xlsx")
  download.file("https://www.acleddata.com/download/3769/", tmp)
  M <- read_excel(tmp)
  names(M) <- make.names(tolower(names(M)))
  M$iso <- countrycode(M$country, "country.name", "iso3n")

  # make a master table for merging incomplete tables
  ACLED.cm.master <- bind_rows(lapply(unique(M$iso), function(x) {
  	
  	S <- filter(M, iso == x)
  	
  	G <- with(S, expand.grid(iso = iso,
  	                         year = seq(year(start.date), year(Sys.Date())),
  	                         month = seq(12),
  	                         stringsAsFactors = FALSE)) %>%
  	  # lop off excess months in current year w/two-week buffer
      filter(as.Date(paste(year, month, "01", sep="-")) <= Sys.Date() - 14) %>%
      arrange(iso, year, month)
  	
  }))

  # Counts of events by type
  ACLED.cm.types <- acleddata %>%
    # Change event type labels for use as proper var names, and to deal with "Remote Violence", "Remote violence"
    mutate(event_type = make.names(str_trim(tolower(event_type)))) %>%
    # Create month var to use in grouping
    mutate(month = month(event_date)) %>% 
    # Define groupings from highest to lowest level; data are automatically ordered accordingly
    group_by(iso, year, month, event_type) %>% 
    # Get counts of records in each group (i.e., each country/year/month/type subset)
    tally() %>% 
    # Make data wide by spreading event types into columns
    spread(., key = event_type, value = n, fill = 0) %>%
    # merge those summaries with master table
    left_join(ACLED.cm.master, .) %>%
    # Replace all NAs created by that last step with 0s
    replace(is.na(.), 0) %>% 
    # Create vars summing counts of all battle types
    mutate(battles = rowSums(select(., contains("battle")))) %>%
    # Use 'countrycode' to add country names based on COW numeric codes
    mutate(country = countrycode(iso, "iso3n", "country.name", warn = FALSE))

  # Death counts
  ACLED.cm.deaths <- acleddata %>%
    mutate(month = month(event_date)) %>% 
    group_by(iso, year, month) %>%
    summarise(deaths = sum(fatalities, na.rm = TRUE)) %>%
    left_join(ACLED.cm.master, .) %>% 
    replace(is.na(.), 0) %>%
    mutate(country = countrycode(iso, "iso3n", "country.name", warn = FALSE))

  # Civilian death counts
  ACLED.cm.deaths.civilian <- acleddata %>%
    mutate(month = month(event_date)) %>% 
    group_by(iso, year, month) %>%
    # pare down to events w/civilian fatalities
    filter(event_type == "Violence against civilians" | (event_type == "Remote violence" & grepl("Civilians", actor2))) %>%
    summarise(deaths.civilian = sum(fatalities, na.rm = TRUE)) %>%
    left_join(ACLED.cm.master, .) %>% 
    replace(is.na(.), 0) %>% 
    mutate(country = countrycode(iso, "iso3n", "country.name", warn = FALSE))

  # Battle death counts
  ACLED.cm.deaths.battle <- acleddata %>%
    mutate(month = month(event_date)) %>% 
    group_by(iso, year, month) %>%
    # pare down to battle events
    filter(grepl("battle", event_type, ignore.case=TRUE)) %>%
    summarise(deaths.battle = sum(fatalities, na.rm=TRUE)) %>%
    left_join(ACLED.cm.master, .) %>% 
    replace(is.na(.), 0) %>% 
    mutate(country = countrycode(iso, "iso3n", "country.name", warn = FALSE))

  DF <- ACLED.cm.types %>%
    merge(., ACLED.cm.deaths) %>%
    merge(., ACLED.cm.deaths.civilian) %>%
    merge(., ACLED.cm.deaths.battle) %>%
    arrange(country, year, month)
    
  return(DF)

}

# make a time series object with a logged count of a particular country-month series. assumes
# data is in format produced by acledr.como
acledr.tslog <- function(df, var) {

  ts(log1p(df[,var]),
     start = c(first(df[,"year"]), first(df[,"month"])),
     frequency = 12)

}
