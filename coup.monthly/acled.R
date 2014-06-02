# ACLED EVENT FILES TO COUNTRY-MONTH EVENT AND FATALITIES COUNTS
# 2014-06-01

rm(list=ls(all=TRUE))

setwd("C:/Users/Jay/Documents/coup.monthly/indata/")

# NOTE: I manually converted both Excel files to tab-delimited text using the "Save As"
# option in Excel because XLConnect kept balking at the .xls files. Partly because of that,
# a lot of code here is devoted to getting the variable names and formats to match across
# the two files so they would merge properly.

base <- read.delim("ACLED-All-Africa_19970101-to-2020131231_final.txt")
base$X <- base$EVENT_ID_NO_CNTY <- NULL # Drop vars that aren't in updates
base$EVENT_ID_CNTY <- as.character(base$EVENT_ID_CNTY)
base$EVENT_DATE <- as.character(base$EVENT_DATE)
base$DATE <- as.character(as.Date(as.character(base$EVENT_DATE), format = "%d %B %Y"))
base$EVENT_TYPE <- as.character(base$EVENT_TYPE)
base$ACTOR1 <- as.character(base$ACTOR1)
base$ALLY_ACTOR_1 <- as.character(base$ALLY_ACTOR_1)
base$ACTOR2 <- as.character(base$ACTOR2)
base$ALLY_ACTOR_2 <- as.character(base$ALLY_ACTOR_2)
base$COUNTRY <- as.character(base$COUNTRY)
base$ADM_LEVEL_1 <- as.character(base$ADMIN1)
base$ADM_LEVEL_2 <- as.character(base$ADMIN2)
base$ADM_LEVEL_3 <- as.character(base$ADMIN3)
base$LOCATION <- as.character(base$LOCATION)
base$SOURCE <- as.character(base$SOURCE)
base$NOTES <- as.character(base$NOTES)
base$ADMIN1 <- base$ADMIN2 <- base$ADMIN3 <- NULL
require(reshape)
base <- rename(base, c(GEO_PRECIS = "GEO_PRECISION"))
base <- rename(base, c(FATALITIES = "TOTAL_FATALITIES"))

update <- read.delim("ACLED All Africa File_20140101 to 20140524.txt")
update$EVENT_DATE <- as.character(update$EVENT_DATE)
update$DATE <- as.character(as.Date(update$EVENT_DATE, "%d %B %Y"))
update$EVENT_ID_CNTY <- as.character(update$EVENT_ID_CNTY)
update$EVENT_TYPE <- as.character(update$EVENT_TYPE)
update$ACTOR1 <- as.character(update$ACTOR1)
update$ALLY_ACTOR_1 <- as.character(update$ALLY_ACTOR_1)
update$ACTOR2 <- as.character(update$ACTOR2)
update$ALLY_ACTOR_2 <- as.character(update$ALLY_ACTOR_2)
update$COUNTRY <- as.character(update$COUNTRY)
update$ADM_LEVEL_1 <- as.character(update$ADM_LEVEL_1)
update$ADM_LEVEL_2 <- as.character(update$ADM_LEVEL_2)
update$ADM_LEVEL_3 <- as.character(update$ADM_LEVEL_3)
update$LOCATION <- as.character(update$LOCATION)
update$SOURCE <- as.character(update$SOURCE)
update$NOTES <- as.character(update$NOTES)

all <- as.data.frame(rbind(base, update))
all$MONTH <- substr(as.character(all$DATE), 6, 7)
# Fix some labels so the events will aggregate properly
all$EVENT_TYPE[all$EVENT_TYPE == "Violence against civilians "] <- "Violence against civilians"
all$EVENT_TYPE[all$EVENT_TYPE == "Battle-non-state actor overtakes territory"] <-
  "Battle-Non-state actor overtakes territory"
all$EVENT_TYPE[all$EVENT_TYPE == "Battle-No Change of territory"] <- "Battle-No change of territory"
all$EVENT <- 1 # Give each event a value of 1 for the summing that follows
civviol <- ddply(subset(all, EVENT_TYPE=="Violence against civilians"),
  ~ COUNTRY + YEAR + MONTH, summarise, CIVVIOL = sum(EVENT, na.rm = TRUE))
protest <- ddply(subset(all, EVENT_TYPE=="Riots/Protests"),
  ~ COUNTRY + YEAR + MONTH, summarise, PROTEST = sum(EVENT, na.rm = TRUE))
battle <- ddply(subset(all,
  EVENT_TYPE=="Battle-No change of territory" |
  EVENT_TYPE=="Battle-Government regains territory" |
  EVENT_TYPE=="Battle-Non-state actor overtakes territory" ),
  ~ COUNTRY + YEAR + MONTH, summarise, BATTLE = sum(EVENT, na.rm = TRUE))

acled <- merge(civviol, protest, all = TRUE)
acled <- merge(acled, battle, all = TRUE)
acled <- acled[order(acled$COUNTRY, acled$YEAR, acled$MONTH),]
names(acled) <- c("name", "year", "month", "civviol", "protest", "battle")
data <- acled
source("c:/users/jay/documents/r scripts/pitf_code_maker.R")
acled <- data
acled$name <- NULL
acled <- rename(acled, c(code="sftgcode"))

acled$month <- as.numeric(acled$month)

source("c:/users/jay/documents/r scripts/country.year.month.rack.maker.R")

africa <- subset(rack, dosreg == "Africa" & year >= 1997)
acled <- merge(africa, acled, all.x = TRUE)
acled[is.na(acled)] <- 0

acled$yearmon <- paste(as.character(acled$year),
  ifelse(acled$month < 10, paste0("0", as.character(acled$month)), as.character(acled$month)), sep = '-')

acled <- subset(acled, select = c("sftgcode", "country", "year", "month", "yearmon", "civviol", "protest", "battle"))
rownames(acled) <- NULL

# Prior sliding sums by event type
acled <- acled[order(acled$sftgcode, acled$year, acled$month),]
acled <- ddply(acled, .(sftgcode), transform, civviol_3 = log1p(as.numeric(filter(civviol, c(0,rep(1,3)), sides=1))))
acled <- ddply(acled, .(sftgcode), transform, civviol_6 = log1p(as.numeric(filter(civviol, c(0,rep(1,6)), sides=1))))
acled <- ddply(acled, .(sftgcode), transform, civviol_12 = log1p(as.numeric(filter(civviol, c(0,rep(1,12)), sides=1))))
acled <- ddply(acled, .(sftgcode), transform, protest_3 = log1p(as.numeric(filter(protest, c(0,rep(1,3)), sides=1))))
acled <- ddply(acled, .(sftgcode), transform, protest_6 = log1p(as.numeric(filter(protest, c(0,rep(1,6)), sides=1))))
acled <- ddply(acled, .(sftgcode), transform, protest_12 = log1p(as.numeric(filter(protest, c(0,rep(1,12)), sides=1))))
acled <- ddply(acled, .(sftgcode), transform, battle_3 = log1p(as.numeric(filter(battle, c(0,rep(1,3)), sides=1))))
acled <- ddply(acled, .(sftgcode), transform, battle_6 = log1p(as.numeric(filter(battle, c(0,rep(1,6)), sides=1))))
acled <- ddply(acled, .(sftgcode), transform, battle_12 = log1p(as.numeric(filter(battle, c(0,rep(1,12)), sides=1))))

write.csv(acled, "c:/users/jay/documents/coup.monthly/outdata/acled.csv", row.names = FALSE)
