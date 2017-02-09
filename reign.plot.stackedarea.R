# Make a stacked area plot of change over time in the prevalence of various political regime types
# using the REIGN data set maintained by One Earth Future Foundation; see
#
#  http://oefresearch.org/datasets/reign
# 
# The script assumes you have downloaded the REIGN data to your working directory. It also assumes
# the version posted in Feb 2017, but that can be fixed with a change to the string in line 17 and
# to the date used for filtering in line 50.

library(dplyr)
library(lubridate)
library(ggplot2)
library(wesanderson)

options(stringsAsFactors = FALSE)

REIGN <- read.csv("reign_2017_02.csv")

# remove superfluous rows, which we'll identify by the absence of a year field
REIGN <- filter(REIGN, !is.na(year))

# vector of labels for higher-level types
types <- c("democracy", "military autocracy", "personalist autocracy", "one-party autocracy", "hybrid autocracy", "other")

# create a factor with original types mapped to those higher-order types
REIGN$macrotype <- NA
REIGN$macrotype[REIGN$regimetype == 8 | REIGN$regimetype == 14] <- 1 # democracy
REIGN$macrotype[REIGN$regimetype >= 2 & REIGN$regimetype <= 4] <- 2 # military autocracy
REIGN$macrotype[REIGN$regimetype == 13] <- 3 # personalist autocracy
REIGN$macrotype[REIGN$regimetype == 7 | REIGN$regimetype == 9] <- 4 # one-party autocracy
REIGN$macrotype[REIGN$regimetype == 5 | (REIGN$regimetype >= 10 & REIGN$regimetype <= 12)] <- 5 #hybrid #autocracy
REIGN$macrotype[REIGN$regimetype == 1 | REIGN$regimetype == 6 | 
                  REIGN$regimetype == 15 | REIGN$regimetype == 16] <- 6 # other
REIGN$macrotype <- factor(REIGN$macrotype, labels = types)

# summarize the data by month, counting each type and then calculating what share that count represents
Regimes <- REIGN %>%
  group_by(year, month) %>%
  count(macrotype) %>%
  mutate(date = as_date(paste(year, month, "01", sep = "-")),
         share = n/sum(n)) %>%
  ungroup() %>%
  select(date, macrotype, n, share) %>%
  # not all types are present in all months, so we need to merge our monthly tallies with a complete
  # table of all possible year-month-type combinations, then set the ones with no values to 0
  left_join(expand.grid(date = unique(.$date), macrotype = types), .) %>%
  arrange(date, macrotype) %>%
  mutate(share = ifelse(is.na(share), 0, share)) %>%
  # REIGN includes some looks ahead, so we need to truncate to the observed data
  filter(macrotype != "NA", date < as.Date("2017-02-01"))

# make a custom palette using the 'wesanderson' package. We need seven values and only get six from the
# "Darjeeling" palette, so we add "gray75" as the color for our "other" type.
mypal <- c(wes_palettes[["Darjeeling"]], "#CCCCCC")

# make the plot and save it to the working directory as a png
png(sprintf("reign.regimetype.%s.png", Sys.Date()),
  width = 7, height = 4, unit = "in", res = 150)
ggplot(Regimes, aes(x = date, y = share, fill = macrotype)) +
  geom_area(colour = NA, alpha = 2/3) +
  scale_fill_manual(values = mypal) +
  labs(fill = "regime type", x = "", y = "share of states worldwide")
dev.off()
