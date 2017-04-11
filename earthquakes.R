library(jsonlite)
library(dplyr)
library(lubridate)
library(RColorBrewer)

options(stringsAsFactors = FALSE)

# a function to fetch earthquake event data from the USGS API
quakedata <- function(start = Sys.Date() - 365.25, end = Sys.Date()) {

  # we're going to fetch in monthly batches to reduce the risk of the request timing out, so we'll start
  # by building a data frame whose rows include the start and end times for each monthly batch
  queries <- data.frame(starttime = seq(from = as.Date(start), to = as.Date(end), by = "month"))
  queries$endtime <- lead(queries$starttime) - 1
  queries$endtime[nrow(queries)] <- Sys.Date()
  
  # now we make a vector of API queries using those date pairs
  queries <- unlist(apply(queries, 1, function(x) {

    sprintf("https://earthquake.usgs.gov/fdsnws/event/1/query?format=csv&starttime=%s&endtime=%s", x[1], x[2])

  }))

  # now we iterate the fetching over those queries, returning the results in a list
  quakeslist <- lapply(queries, function(i) {

    X <- read.csv(i, stringsAsFactors = FALSE) %>%
      mutate(date = date(time)) %>%
      select(date, latitude, longitude, place, mag) %>%
      arrange(date)

  })

  # now bind the rows of that list's elements into a single data frame
  quakes <- bind_rows(quakeslist)

  return(quakes)  

}

# run the function, using January 1, 2013 as the start date. it defaults to the current date as the end time.
Q <- quakedata(start = "2013-01-01")

# drop the string column describing the place, which we don't need for mapping and hogs space.
Q$place <- NULL

# create an ordered factor for ranges of magnitude from 0-1 to 7+
Q$magcat <- cut(Q$mag, breaks = c(0:7,10), include.lowest = TRUE, right = FALSE, ordered_result = TRUE)

# write the file to disk to export to Carto DB
write.csv(Q, "quake.data.cartodb.csv", row.names = FALSE)

# get colors to use for orange scale. we'll enter these manually in the Carto DB wizard.
print(colorRampPalette(c("yellow", "red"))(n = 8))
