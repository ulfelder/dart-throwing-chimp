library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

# Ingest Lego data
Lego <- read.csv("https://raw.githubusercontent.com/seankross/lego/master/data-tidy/legosets.csv", stringsAsFactors=FALSE)

# Filter to Lego Star Wars kits
Lego.SW <- filter(Lego, Theme=="Star Wars")

# Make list of desired plot objects
lego_plot_list = list()
lego_plot_list[[1]] = ggplot(tally(group_by(Lego.SW, Year)), aes(x=Year, y=n)) + geom_line() + theme_bw() + ylab("Number of sets released")
lego_plot_list[[2]] = ggplot(Lego.SW, aes(Pieces)) + geom_histogram(binwidth=10) + theme_bw() + ylab("Number of sets")
lego_plot_list[[3]] = ggplot(Lego.SW, aes(USD_MSRP)) + geom_histogram(binwidth=10) + theme_bw() + xlab("Price (USD)") + ylab("Number of sets")
lego_plot_list[[4]] = ggplot(Lego.SW, aes(x=Pieces, y=USD_MSRP)) + geom_point() + theme_bw() + xlab("Number of pieces") + ylab("Price")

# Put plot objects in grid
lego.grid = marrangeGrob(lego_plot_list, nrow=2, ncol=2, top="Overview of Lego Star Wars Sets")

# Look at it
lego.grid

# Save it to png
png("legostarwars.png", width=6, height=6, unit="in", res=300)
lego.grid
dev.off()
