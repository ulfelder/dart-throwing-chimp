library(gsheet)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemese)

ETA <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1hVJsI0BZ0SgFz51vy0oydSppAhtaS45cA88z2N86tFg/edit?hl=en#gid=0") %>%
  transmute(year = as.numeric(Year),
            victims = as.numeric(str_trim(Victims))) %>%
  filter(!is.na(year))

png("eta.deaths.plot.png", unit = "in", width = 6, height = 3, res = 300)
ggplot(ETA, aes(x = year, y = victims)) + geom_col() + theme_tufte() +
  labs(x = "", y = "", title = "Annual counts of people killed in ETA attacks")
dev.off()
