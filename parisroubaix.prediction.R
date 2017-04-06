# script to predict finishing time of men's Paris-Roubaix race

library(dplyr)
library(tidyr)
library(stringr)
library(rvest)
library(gam)

# get historical data, following http://www.r-bloggers.com/using-rvest-to-scrape-an-html-table/
url <- "http://bikeraceinfo.com/classics/paris-roubaix/paris-roubaix-index.html"
PR <- url %>%
  read_html() %>%
  html_nodes(xpath='/html/body/div[1]/div[3]/div[3]/table[2]') %>%
  html_table(header=TRUE) %>%
  .[[1]] # previous line returns df inside a list; this returns just the df

# fix the variable names
names(PR) <- c("year", "first", "second", "third", "distance", "speed")

# convert variable types and calculate finish time from reported data
PR <- PR %>%
  transmute(year = as.numeric(year), 
            wintime = str_extract(first, "[0-9]{1,2}[a-z]{2} [0-9]{1,2}[a-z]{3} [0-9]{1,2}[a-z]{3}"),
            distance = as.numeric(sub(" km", "", distance))) %>%
  separate(wintime, c("hours", "minutes", "seconds"), sep = " ") %>%
  mutate_at(vars(hours, minutes, seconds), funs(as.numeric(gsub("[a-z]", "", .)))) %>%
  # get winner's finish time in minutes
  mutate(time = 1/60 * (hours * 3600 + minutes * 60 + seconds)) %>%
  filter(!is.na(year))

# simple linear model with time as a function of distance and historical year
mod1 <- lm(time ~ distance + year, data = PR, na.action = na.exclude)
mod1.res <- resid(mod1)
plot(PR$time, mod1.res)

# generalized additive model that allows nonlinear time trend
mod2 <- gam(time ~ s(year) + distance, data = PR, na.action = na.exclude)
mod2.res <- resid(mod2)
plot(PR$time, mod2.res)
plot(mod2, se=TRUE, ask=TRUE) # clear nonlinear association with time, so prefer this version

# use preferred model to generate a prediction for current year. this assumes that the
# current year is a row in the original table, with the race distance given.
p <- predict(mod2, newdata=PR[nrow(PR),], sefit=TRUE)
