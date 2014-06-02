# POLITY 3D TO POLITY MONTHLY
# 2014-06-01

rm(list=ls(all=TRUE))

memory.limit(size = 4000)

require(XLConnect)
require(reshape)

filename <- "p4v2013d.xls"
dir <- "c:/users/jay/documents/coup.monthly/indata/"
pol3d <- readWorksheetFromFile(paste0(dir, filename), sheet = 1)
pol3d$start <- as.Date(with(pol3d, paste(byear, bmonth, bday, sep = "-")))
pol3d$end <- as.Date(with(pol3d, paste(eyear, emonth, eday, sep = "-")))
pol3d$end[is.na(pol3d$end)] <- as.Date(paste(substr(filename, 4,7), "12", "31", sep = "-")) # Create end date for right-censored cases
pol3d$id <- c(1:dim(pol3d)[1]) # Create a case id
pol3d$scode[pol3d$scode=="UKG"] <- "UK " # Fix some country codes to match PITF
pol3d$scode[pol3d$scode=="SER"] <- "SRB"
pol3d$scode[pol3d$scode=="MNT"] <- "MNE"
pol3d$scode[pol3d$scode=="GMY"] <- "GER"
pol3d$scode[pol3d$scode=="SSU"] <- "SSD"
pol3d$scode[pol3d$scode=="SDN"] <- "SUD"
pol3d$scode[pol3d$scode=="USR"] <- "USS"
vars <- c("scode", "country", "polity", "democ", "autoc", "xrreg", "xrcomp", "xropen", "xconst",
  "parreg", "parcomp", "exrec", "exconst", "polcomp", "start", "end", "id")
pol3d <- subset(pol3d, select = vars)
date.range <- seq(min(pol3d$start), max(pol3d$end), by="month") # Expand data across time
date.range <- as.Date(paste(format(date.range, '%Y-%m'), '01', sep='-'))
skeleton <- expand.grid(pol3d$id, date.range)
colnames(skeleton) <- c('id', 'date')
pol3d <- join(skeleton, pol3d, by='id', type="left")
rm(skeleton); gc()  # get rid of skeleton and garbage collection
pol3d <- pol3d[(pol3d$date >= pol3d$start & pol3d$date <= pol3d$end), ] # Trim TSCS to correct regimes
pol3d$month <- as.character(format(pol3d$date, '%Y-%m'))
pol3d <- rename(pol3d, c(scode="sftgcode"))
pol3d <- pol3d[order(pol3d$sftgcode, pol3d$date), ]

# TRANSFORMATIONS FOR MODELING

# Regime spell duration
pol3d$duration <- as.numeric(as.Date(pol3d$date, format = "%Y-%m-%d") - as.Date(pol3d$start, format = "%Y-%m-%d"))
pol3d$durln <- log1p(pol3d$duration)

# Fearon & Laitin regime type (autocracy, anocracy, democracy)
pol3d$polcat[pol3d$polity >= -10 & pol3d$polity < -5] <- 1 
pol3d$polcat[pol3d$polity >= -5 & pol3d$polity <= 5] <- 2
pol3d$polcat[pol3d$polity > 5] <- 3
pol3d$polcat[pol3d$polity == -66 | pol3d$polity == -77 | pol3d$polity == -88 ] <- 7

# PITF AJPS regime types
pol3d$pitfcat[pol3d$polity==-66 | pol3d$polity==-77 | pol3d$polity==-88] <- "other"
pol3d$pitfcat[(pol3d$exrec >= 1 & pol3d$exrec <= 6) & (pol3d$parcomp == 1 | pol3d$parcomp == 2)] <- "A/F"
pol3d$pitfcat[(pol3d$exrec >= 1 & pol3d$exrec <= 6) &
  (pol3d$parcomp == 0 | pol3d$parcomp == 3 | pol3d$parcomp == 4 | pol3d$parcomp == 5)] <- "A/P"
pol3d$pitfcat[(pol3d$exrec == 7 | pol3d$exrec == 8) & (pol3d$parcomp == 1 | pol3d$parcomp == 2)] <- "A/P"
pol3d$pitfcat[pol3d$parcomp == 3 & (pol3d$exrec == 7 | pol3d$exrec==8)] <- "D/fact"
pol3d$pitfcat[pol3d$exrec == 8 & (pol3d$parcomp == 0 | pol3d$parcomp == 4 )] <- "D/P"
pol3d$pitfcat[pol3d$exrec == 7 & (pol3d$parcomp == 0 | pol3d$parcomp == 4 | pol3d$parcomp == 5)] <- "D/P"
pol3d$pitfcat[pol3d$exrec == 8 & pol3d$parcomp == 5] <- "D/F"

# Dummied version of polcat for RF
pol3d$polcat1 <- ifelse(pol3d$polcat==1, 1, ifelse(is.na(pol3d$polcat)==TRUE, NA, 0) )
pol3d$polcat2 <- ifelse(pol3d$polcat==2, 1, ifelse(is.na(pol3d$polcat)==TRUE, NA, 0) )
pol3d$polcat3 <- ifelse(pol3d$polcat==3, 1, ifelse(is.na(pol3d$polcat)==TRUE, NA, 0) )
pol3d$polcat7 <- ifelse(pol3d$polcat==7, 1, ifelse(is.na(pol3d$polcat)==TRUE, NA, 0) )

# Dummied version of PITF regime type for RF
pol3d$pitfcat1 <- ifelse(pol3d$pitfcat=="A/F", 1, ifelse(is.na(pol3d$pitfcat)==TRUE, NA, 0) )
pol3d$pitfcat2 <- ifelse(pol3d$pitfcat=="A/P", 1, ifelse(is.na(pol3d$pitfcat)==TRUE, NA, 0) )
pol3d$pitfcat3 <- ifelse(pol3d$pitfcat=="D/fact", 1, ifelse(is.na(pol3d$pitfcat)==TRUE, NA, 0) )
pol3d$pitfcat4 <- ifelse(pol3d$pitfcat=="D/P", 1, ifelse(is.na(pol3d$pitfcat)==TRUE, NA, 0) )
pol3d$pitfcat5 <- ifelse(pol3d$pitfcat=="D/F", 1, ifelse(is.na(pol3d$pitfcat)==TRUE, NA, 0) )
pol3d$pitfcat6 <- ifelse(pol3d$pitfcat=="other", 1, ifelse(is.na(pol3d$pitfcat)==TRUE, NA, 0) )

# Lags for modeling
require(plm)
pol3d <- rename(pol3d, c(month="yearmon"))
lags <- pdata.frame(pol3d, index = c("sftgcode", "yearmon"))
lags$durln_1 <- lag(lags$durln, 1)
lags$polcat1_1 <- lag(lags$polcat1, 1)
lags$polcat2_1 <- lag(lags$polcat2, 1)
lags$polcat3_1 <- lag(lags$polcat3, 1)
lags$polcat7_1 <- lag(lags$polcat7, 1)
lags$pitfcat1_1 <- lag(lags$pitfcat1, 1)
lags$pitfcat2_1 <- lag(lags$pitfcat2, 1)
lags$pitfcat3_1 <- lag(lags$pitfcat3, 1)
lags$pitfcat4_1 <- lag(lags$pitfcat4, 1)
lags$pitfcat5_1 <- lag(lags$pitfcat5, 1)
lags$pitfcat6_1 <- lag(lags$pitfcat6, 1)
pol3dl <- as.data.frame(lags)
# Workaround for list to df
temp <- tempfile()
write.csv(pol3dl, temp, row.names = FALSE)
pol3dl <- read.csv(temp)

pol3dl$sftgcode <- as.character(pol3dl$sftgcode)
pol3dl$country <- as.character(pol3dl$country)
pol3dl$yearmon <- as.character(pol3dl$yearmon)
pol3dl$start <- as.character(pol3dl$start)
pol3dl$end <- as.character(pol3dl$end)
pol3dl$id <- pol3dl$date <- NULL

write.csv(pol3dl, "c:/users/jay/documents/coup.monthly/outdata/polity.csv", row.names = FALSE)
