# POWELL & THYNE COUP DATA TO COUNTRY-MONTH FORMAT
# 2014-06-01

rm(list=ls(all=TRUE))

memory.limit(size = 4000)

pt <- read.delim("http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt")
pt <- subset(pt, select=c("country", "year", "month", "coup"))
data <- pt
require(reshape)
data <- rename(data, c(country="name"))
source("c:/users/jay/documents/ushmm/statrisk2014v2/r/pitf_code_maker.r")
pt <- data
pt$pt.succ <- ifelse(pt$coup==2, 1, 0)
pt$pt.fail <- ifelse(pt$coup==1, 1, 0)
pt <- rename(pt, c(code="sftgcode"))
ptsum.s <- tapply(pt$pt.succ, list(pt$sftgcode, pt$year, pt$month), sum)
ptsum.f <- tapply(pt$pt.fail, list(pt$sftgcode, pt$year, pt$month), sum)
pt.s <- melt(ptsum.s)
pt.f <- melt(ptsum.f)
names(pt.s) <- c("sftgcode", "year", "month", "pt.succ")
names(pt.f) <- c("sftgcode", "year", "month", "pt.fail")
source("c:/users/jay/documents/r scripts/country.year.month.rack.maker.R")
rack <- subset(rack, year >= 1950 & (year < 2013 | month < 6), select = c(country, sftgcode, year, month))
coups <- merge(rack, pt.s, all.x = TRUE)
coups <- merge(coups, pt.f, all.x = TRUE)
coups[is.na(coups)] <- 0
coups$month <- paste(as.character(coups$year),
  ifelse(coups$month < 10, paste0("0", as.character(coups$month)), as.character(coups$month)), sep = '-')

# Binary versions of success, failed, and any
coups$coup.s.d <- ifelse(coups$pt.succ > 1, 1, coups$pt.succ )
coups$coup.f.d <- ifelse(coups$pt.fail > 1, 1, coups$pt.fail )
coups$coup.a.d <- ifelse(coups$coup.f.d==1 | coups$coup.s.d==1, 1, ifelse(is.na(coups$coup.s.d)==TRUE, NA, 0) )
coups <- coups[order(coups$country,coups$year),]

# Prior moving sum and dummy
coups <- ddply(coups, .(sftgcode), transform, coups_24 = as.numeric(filter(coup.a.d, c(0,rep(1,24)), sides=1)))
coups$coups_24d <- ifelse(is.na(coups$coups_24)==TRUE, NA, ifelse(coups$coups_24 > 0, 1, 0))
# NOTE: It would be nice to have wider window, but SSD only has 30 months of data, and 'filter' balks if any
# one of the time series is shorter than the window.

write.csv(coups, "c:/users/jay/documents/coup.monthly/outdata/coups.csv", row.names = FALSE)
