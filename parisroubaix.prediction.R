# script to predict finishing time of 2016 Paris-Roubaix for this contest:
# http://pages.rapha.cc/spring-monuments-competition/spring-monuments-competition-4-paris-roubaix

library(dplyr)
library(rvest)

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
            distance = as.numeric(sub(" km", "", distance)), # strip unit from recent years that include it before converting
            speed = as.numeric(speed)) %>%
  filter(!is.na(year)) %>%
  mutate(time = distance/speed)

# simple linear model with time as a function of distance and historical year
mod1 <- lm(time ~ distance + year, data = PR, na.action = na.exclude)
mod1.res <- resid(mod1)
plot(PR$time, mod1.res)

# generalized additive model that allows nonlinear time trend
mod2 <- gam(time ~ s(year) + distance, data = PR, na.action = na.exclude)
mod2.res <- resid(mod2)
plot(PR$time, mod2.res)
plot(mod2, se=TRUE, ask=TRUE) # clear nonlinear association with time, so prefer this version

# use preferred model to generate a prediction for 2016
p2016 <- 60 * (predict(mod2, newdata=PR[nrow(PR),], sefit=TRUE))  # 6 hrs 5 min 24 sec
