# make plots of state-level data on participation in Women's March 2017 events
# code seeded by https://gist.github.com/benmarwick/a1ac9c7235ebef542824512162ff2f44

data_source <- "https://docs.google.com/spreadsheets/d/1xa0iLqYKz8x9Yc_rfhtmSOJQ2EGgeUVjvV4A8LsIaxY/htmlview?sle=true#gid=0"

library(dplyr)
library(tidyr)
library(stringr)
library(rvest)
library(Hmisc)

# load and clean location-level data on participation
crowds <- 
  read_html(data_source)  %>% 
  html_table(header = FALSE) %>%
  .[[1]] %>%
  slice(9:n()) %>%
  transmute(location = X2,
            est.low = as.numeric(gsub(",", "", X4)),
            est.high = as.numeric(gsub(",", "", X5)),
            source = X8) %>%
  filter(!is.na(location),
         !grepl("Disablity", location))

# manually fix a couple of location labels to facilitate splitting to come
crowds$location[which(crowds$location == "Champaign IL")] <-  "Champaign, IL"
crowds$location[which(crowds$location == "Washington DC")] <-  "Washington, DC"

# get data for US events only by splitting at row where international total is inserted, then split city & state info
crowds_usa <- crowds %>%
  slice(1:(which(grepl("Int'l", crowds$location)) - 1)) %>%
  filter(!is.na(location) & !is.null(location) & location != "") %>%
  separate(location, c("city", "state"), ",") %>%
  mutate(city = str_trim(city),
         state = str_trim(state)) %>%
  mutate(state = ifelse(state %in% state.abb | state == "DC", state, state.abb[match(state, state.name)]))

# summarize by state
state_totals <- crowds_usa %>%
  group_by(state) %>%
  summarise(total.high = sum(est.high, na.rm = T),
            total.low = sum(est.low, na.rm = T)) %>%
  arrange(desc(total.high))

png("womens.marchers.by.state.png", res = 150, width = 5, height = 7, unit = "in")
par(mai=c(1,0.25,0.25,0.25))
dotchart2(state_totals$total.high,
         labels = state_totals$state, cex.labels = 3/4,
         lines = TRUE, lwd = 1/20, lty = 3,
         dotsize = 1, col = "deeppink2", pch = 20,
         xlab = 'total marchers (high est.)')
title(main=list("Women's march participants by U.S. state", cex=0.9))
dev.off()

# load and clean/prep state population data from Census
state_pops <- read.csv("http://www2.census.gov/programs-surveys/popest/datasets/2010-2016/national/totals/nst-est2016-alldata.csv",
                       stringsAsFactors = FALSE) %>%
  filter(STATE > 0) %>%
  transmute(statename = NAME,
            pop = POPESTIMATE2016) %>%
  mutate(state = state.abb[match(statename, state.name)])
state_pops$state[state_pops$statename == "District of Columbia"] <- "DC"

# merge the pop data with the march data and compute rates
state_totals_pop <- merge(state_totals, state_pops) %>%
  filter(!is.na(state), state != "DC") %>%
  mutate(marchers.per.1000.high = (total.high/pop) * 1000,
         marchers.per.1000.low  = (total.low/pop) * 1000) %>%
  arrange(desc(marchers.per.1000.high))

png("womens.marchers.per.1000.png", res = 150, width = 5, height = 7, unit = "in")
par(mai=c(1,0.25,0.25,0.25))
dotchart2(state_totals_pop$marchers.per.1000.high,
         labels = state_totals_pop$statename, cex.labels = 3/4,
         lines = TRUE, lwd = 1/20, lty = 3,
         dotsize = 1, col = "deeppink2", pch = 20,
         xlab = "2017 Women's March participants \nper 1,000 state pop.")
dev.off()

# make separate data frame of international events (for possible future use)
crowds_global <- crowds %>%
  slice((which(grepl("Int'l", crowds$location)) + 1):n()) %>%
  filter(!is.na(location))
