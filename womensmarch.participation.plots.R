data_source <- "https://docs.google.com/spreadsheets/d/1xa0iLqYKz8x9Yc_rfhtmSOJQ2EGgeUVjvV4A8LsIaxY/htmlview?sle=true#gid=0"

library(dplyr)
library(tidyr)
library(stringr)
library(rvest)
library(Hmisc)
library(WDI)
library(scales)

# load and clean event-level data on women's marches
crowds <- 
  read_html(data_source)  %>% 
  html_table(header = FALSE) %>%
  .[[1]] %>%
  # get rid of junk rows at the top
  slice(12:n()) %>% 
  transmute(location = X2,
            state = X4,
            country = X5,
            est.low = as.numeric(gsub(",", "", X6)),
            est.high = as.numeric(gsub(",", "", X7)),
            source = X10) %>%
  # clear out some remaining rows with no info or aggregates
  filter(!is.na(country),
         !grepl("Disablity", location),
         !grepl("Int'l", location))

### US STATES ###

# state-level march data
state_marchers <- crowds %>%
  filter(country == "US") %>%
  group_by(state) %>%
  summarise(total.high = sum(est.high, na.rm = TRUE),
            total.low = sum(est.low, na.rm = TRUE))

# state-level population data
state_pops <- read.csv("http://www2.census.gov/programs-surveys/popest/datasets/2010-2016/national/totals/nst-est2016-alldata.csv",
                       stringsAsFactors = FALSE) %>%
  filter(STATE > 0) %>%
  transmute(statename = NAME,
            pop = POPESTIMATE2016) %>%
  mutate(state = state.abb[match(statename, state.name)])
state_pops$state[state_pops$statename == "District of Columbia"] <- "DC"

# join those two and compute shares
state <- left_join(state_marchers, state_pops) %>%
  filter(!is.na(pop)) %>%
  mutate(marchers.percent.high = (total.high/pop) * 100,
         marchers.percent.low  = (total.low/pop) * 100) %>%
  arrange(desc(marchers.percent.high))

# create a version that drops DC, which is an outlier in a couple of ways
state_nodc <- filter(state, state != "DC")

png("womens.marchers.state.percent.png",
  res = 150, width = 5, height = 7, unit = "in")
par(mai=c(1,0.25,0.25,0.25))
dotchart2(state_nodc$marchers.percent.high,
         labels = state_nodc$statename, cex.labels = 3/4,
         lines = TRUE, lwd = 1/20, lty = 3,
         dotsize = 1, col = "deeppink2", pch = 20,
         xlab = "2017 Women's March event participants \nas a share of pop. of events' host state")
dev.off()

# get and prep election results data
state_vote <- read.table("https://raw.githubusercontent.com/kshaffer/election2016/master/2016ElectionResultsByState.csv",
                          sep = ",", header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(trumpshare = trumpVotes/totalVotes * 100) %>%
  select(state = postal, trumpshare)

# merge vote data with no-DC version of state data
state_nodc <- left_join(state_nodc, state_vote)

# make scatter plot of participation vs. trump vote share
png("womens.marchers.relative.to.trump.vote.png",
  res = 150, width = 5, height = 5, unit = "in")
par(mai=c(1, 1, 0.1, 0.1))
plot(x = state_nodc$trumpshare, xlim = c(25,75),
     y = state_nodc$marchers.percent.high, ylim = c(0,4),
     type = "n", axes = FALSE, xlab = "Trump's host-state vote share", ylab = "total marchers as % of host-state pop.")
text(x = state_nodc$trumpshare, xlim = c(25,75),
     y = state_nodc$marchers.percent.high,
     labels = state_nodc$state,
     cex = 1, col = scales::alpha("black", 2/3))
axis(1, at = seq(25, 75, 25))
axis(2, at = seq(0,4,1), las = 2)
dev.off()

### GLOBAL ###

# get country-level sums of crowd sizes
crowds_intl <- crowds %>%
  group_by(country) %>%
  summarise(total.low = sum(est.low, na.rm = TRUE),
            total.high = sum(est.high, na.rm = TRUE)) %>%
  arrange(desc(total.high)) %>%
  filter(country != "") %>%
  mutate(iso3c = countrycode::countrycode(country, "country.name", "iso3c"))

crowds_intl$country[crowds_intl$country == "Czech"] <- "Czechia"
crowds_intl$iso3c[crowds_intl$country == "Czechia"] <- "CZE"
crowds_intl$iso3c[crowds_intl$country == "Kosovo"] <- "KOS"

# use WDI package get population data and prep it for merging
pop_intl <- WDI(country="all",
                indicator = "SP.POP.TOTL",
                extra = TRUE,
                start = 2015, end = 2015) %>%
  filter(!is.na(iso3c)) %>%
  select(iso3c, pop = SP.POP.TOTL) %>%
  # add a row for Kosovo, which WB doesn't consider to be a state, I guess
  rbind(data.frame(iso3c = "KOS", pop = 1824000)) %>%
  mutate(iso3c = as.character(iso3c))

# merge and arrange country data 
intl <- left_join(crowds_intl, pop_intl) %>%
  mutate(participation.rate = (total.high/pop) * 100) %>%
  arrange(desc(participation.rate))

# make dot plot of country participation rates
png("womens.march.global.participation.rate.png",
    res = 150, width = 5, height = 7.5, unit = "in")
par(mai=c(1,0.2,0.2,0.2))
dotchart2(intl$participation.rate,
         labels = intl$country, cex.labels = 7/10,
         lines = TRUE, lwd = 1/20, lty = 3,
         dotsize = 1, col = "deeppink2", pch = 20,
         xlab = 'total marchers (high estimates) \nas a percentage of national population')
dev.off()
