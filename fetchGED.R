library(dplyr)

# load the data from the web
url <- "http://ucdp.uu.se/downloads/ged/ged50-csv.zip"
tmp <- paste0(tempfile(), ".zip") # Create tempfile connection
download.file(url, tmp) # Download the .zip file to that connection
tmp2 <- tempfile() # Create another connection
tmp2 <- unzip(zipfile = tmp, files = "ged50.csv", exdir = tempdir()) # Unzip the specified file from the download to that slot
GED.events <- read.csv(tmp2, stringsAsFactors = FALSE)

# get simple country-month summaries
GED.cm <- GED.events %>%
  mutate(year = lubridate::year(as.Date(date_start)),
         month = lubridate::month(as.Date(date_start))) %>%
  group_by(country, year, month) %>%
  summarise(ged.events = n(),
            ged.deaths.best = sum(best_est, na.rm = TRUE),
            ged.deaths.low = sum(low_est, na.rm = TRUE),
            ged.deaths.high = sum(high_est, na.rm = TRUE),
            ged.deaths.civilians = sum(deaths_civilians, na.rm = TRUE)) %>%
  left_join(with(GED.events, expand.grid(country = unique(country), year = unique(year), month = seq(12), stringsAsFactors = FALSE)), .) %>%
  arrange(country, year, month)
GED.cm[is.na(GED.cm)] = 0 # replace NAs with 0s

# add country code. doing this after last step so NAs in iso3c don't get replaced with 0s
GED.cm <- GED.cm %>%
  mutate(iso3c = countrycode::countrycode(country, "country.name", "iso3c")) %>%
  select(country, iso3c, everything())

# function to make line plot of monthly death count for one country
gedplot.deaths <- function(x) {

  require(dplyr)
  require(ggplot2)

  GED.cm %>%
    filter(iso3c == x) %>%
    mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
    ggplot(., aes(x = date, y = ged.deaths.best)) +
      geom_ribbon(aes(ymin = ged.deaths.low, ymax = ged.deaths.high), alpha = 1/5) +
      geom_line() +
      theme_bw() +
      ggtitle(countrycode::countrycode(x, "iso3c", "country.name")) +
      labs(x = "", y = "total deaths")

}

# not run: make a line plot of death counts for Afghanistan
# png("ged.afg.png", width = 5, height = 3, unit = "in", res = 150)
# gedplot.deaths("AFG")
# dev.off()
