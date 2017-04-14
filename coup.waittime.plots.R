library(dplyr)
library(countrycode)
library(lubridate)
library(ggplot2)

options(stringsAsFactors = FALSE)

# coup data ingestion and transformation
Coups <- read.delim("http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt") %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c"),
         date = date(paste(year, month, day, sep = "-"))) %>%
  arrange(date, iso3c, coup)

# make a table of time gaps between attempts, with an index to use as x-axis in plots
Coup.gaps <- Coups %>%
  transmute(index = seq_along(coup),
            waittime = date - lag(date))
 
png(sprintf("coup.waittime.%s.png", gsub("-", "", Sys.Date())),
    width = 7, height = 5, unit = "in", res = 300)
ggplot(Coup.gaps, aes(x = index, y = waittime)) + geom_line() +
  theme_bw() +
  labs(title = "time in days between coup attempts worldwide, 1950-present",
       subtitle = "data source: Jonathan Powell and Clayton Thyne") + 
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())
dev.off()

# how long right now since last attempt?
Sys.Date() - last(Coups$date)

# do the same thing, but with just successful coup attempts (coup == 2)
Coup.gaps.successes <- Coups %>%
  filter(coup == 2) %>%
  transmute(index = seq_along(coup),
            waittime = date - lag(date))
 
png(sprintf("coup.waittime.successes.%s.png", gsub("-", "", Sys.Date())),
    width = 7, height = 5, unit = "in", res = 300)
ggplot(Coup.gaps.successes, aes(x = index, y = waittime)) + geom_line(color = "darkred") +
  theme_bw() +
  labs(title = "time in days between successful coups worldwide, 1950-present",
       subtitle = "data source: Jonathan Powell and Clayton Thyne") + 
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())
dev.off()

# how long since last successful coup?
Sys.Date() - last(filter(Coups, coup == 2)$date)
