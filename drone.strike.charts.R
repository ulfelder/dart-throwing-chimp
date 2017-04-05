library(jsonlite)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)

# Pull the data from the API. Thanks to Josh Begley for making this so easy.
drone <- fromJSON("http://api.dronestre.am/data")

# The previous step gives us a list with two items. We want the second, "strike", which
# contains the strike event data.
drone <- drone[["strike"]]

# there's just one strike located in Pakistan-Afghanistan border, so for simplicity's sake, let's dump
# it in with the Pakistan ones
drone$country <- replace(drone$country, which(drone$country == "Pakistan-Afghanistan Border"), "Pakistan")

drone.events <- drone %>%
  mutate(yearmon = as.yearmon(date)) %>%
  group_by(country, yearmon) %>%
  tally()

png("drone.strike.event.counts.png", width = 6, height = 6, unit = "in", res = 300)
ggplot(drone.events, aes(x = yearmon, y = n)) + geom_col() + theme_bw() +
  labs(x = "", y = "total strikes") +
  facet_grid(country ~ .)
dev.off()

drone.deaths <- drone %>%
  mutate(yearmon = as.yearmon(date)) %>%
  group_by(country, yearmon) %>%
  summarise(deaths_min = sum(as.numeric(deaths_min), na.rm = TRUE),
            deaths_max = sum(as.numeric(deaths_max), na.rm = TRUE))

png("drone.strike.death.counts.png", width = 6, height = 6, unit = "in", res = 300)
ggplot(drone.deaths, aes(x = yearmon, y = deaths_max)) + geom_col(fill = "firebrick") + theme_bw() +
  labs(x = "", y = "total deaths (maximum)") +
  facet_grid(country ~ .)
dev.off()

drone.deaths.civilian <- drone %>%
  mutate(yearmon = as.yearmon(date)) %>%
  # The column with civilian deaths includes single numbers, ranges (e.g., "6-8"), and empty slots, and
  # it's string. To convert that to useful numbers, we will use 'separate' from tidyr to split the
  # ranges into two columns, deaths_civilian_min and deaths_civilian_max. Single numbers get NAs in the
  # max column.
  separate(civilians, c("deaths_civilian_min", "deaths_civilian_max"), sep = "-", remove = FALSE) %>%
  # Then we convert those split strings into numbers and add a new column that uses the average of the 
  # min and max if both are present, otherwise just the min/only. This is what we'll plot.
  mutate(deaths_civilian_min = as.numeric(deaths_civilian_min),
         deaths_civilian_max = as.numeric(deaths_civilian_max),
         deaths_civilian_est = ifelse(!is.na(deaths_civilian_max), (deaths_civilian_max + deaths_civilian_min)/2, deaths_civilian_min)) %>%
  group_by(country, yearmon) %>%
  summarise(deaths_civilian_min = sum(deaths_civilian_min, na.rm = TRUE),
            deaths_civilian_max = sum(deaths_civilian_max, na.rm = TRUE),
            deaths_civilian_est = sum(deaths_civilian_est, na.rm = TRUE))

png("drone.strike.civilian.death.counts.png", width = 6, height = 6, unit = "in", res = 300)
ggplot(drone.deaths.civilian, aes(x = yearmon, y = deaths_civilian_est)) + geom_col(fill = "darkorange") + theme_bw() +
  labs(x = "", y = "total civiilan deaths") +
  facet_grid(country ~ .)
dev.off()
