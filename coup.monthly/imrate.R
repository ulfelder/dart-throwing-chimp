# CENSUS BUREAU INFANT MORTALITY ESTIMATES IN COUNTRY-MONTH FORMAT
# 2014-06-01

# Source: U.S. Census Bureau International Division, via PITF

rm(list=ls(all=TRUE))

setwd("c:/users/jay/documents/coup.monthly/indata/")

imrates <- read.csv("pitf.csv")
imrates <- imrates[,1:4]
names(imrates) <- c("sftgcode", "year", "cnsimr", "xxxcimr")
imrates$sftgcode <- substr(as.character(imrates$sftgcode),1,3)
imrates$cnsimr <- as.numeric(as.character(imrates$cnsimr))
imrates$xxxcimr <- as.numeric(substr(as.character(imrates$xxxcimr),1,5))

source("c:/users/jay/documents/r scripts/country.year.month.rack.maker.r")
rack <- subset(rack, select=c(country, sftgcode, year, month))
rack <- merge(rack, imrates, all.x = TRUE)
rack <- rack[order(rack$country, rack$year, rack$month),]

# Hard code values for USA, which source doesn't cover, to values for Canada
rack$cnsimr[rack$sftgcode=="USA"] <- rack$cnsimr[rack$sftgcode=="CAN"]
rack$xxxcimr[rack$sftgcode=="USA"] <- rack$xxxcimr[rack$sftgcode=="CAN"]

rack$month <- paste(as.character(rack$year),
  ifelse(rack$month < 10, paste0("0", as.character(rack$month)), as.character(rack$month)), sep = '-')

# Logged version of rate relative to annual global median
rack$xxxcimrln <- log(rack$xxxcimr)

write.csv(rack, "c:/users/jay/documents/coup.monthly/outdata/imrate.csv", row.names = FALSE)
