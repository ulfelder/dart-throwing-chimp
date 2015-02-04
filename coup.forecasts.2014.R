# This script goes with "Coup Forecasts for 2014":
# https://dartthrowingchimp.wordpress.com/2014/01/25/coup-forecasts-for-2014/

# The data set produced in the first section of this script can be downloaded in .csv format from Google Drive:
# https://drive.google.com/file/d/0B5wyt4eDq98GWUdQRW1BcThqdk0/edit?usp=sharing
#
# The data sets included in that file were created by a series of scripts, one for each source. If you would like
# to see any of those, please let me know.

rm(list=ls(all=TRUE))

setwd("c:/users/jay/documents/couprisk2014/")

# Get data sets
rack <- read.csv("outdata/rack.csv")
coups <- read.csv("outdata/coups.csv")
imrate <- read.csv("outdata/imrate.csv")
gdpgrowth <- read.csv("outdata/gdpgrowth.csv")
mepv <- read.csv("outdata/mepv.csv")
gdelt <- read.csv("outdata/gdelt.csv")
polity <- read.csv("outdata/polity.csv")
nelda <- read.csv("outdata/nelda.csv")
elc <- read.csv("outdata/elc.csv")

# Merge data sets
mash <- merge(rack, coups, all.x = TRUE)
mash <- merge(mash, imrate, all.x = TRUE)
mash <- mash[!duplicated(mash),]
mash <- merge(mash, gdpgrowth, all.x = TRUE)
mash <- merge(mash, mepv, all.x = TRUE)
mash <- merge(mash, gdelt, all.x = TRUE)
mash <- merge(mash, polity, all.x = TRUE)
mash <- merge(mash, nelda, all.x = TRUE)
mash <- merge(mash, elc, all.x = TRUE)
mash <- mash[order(mash$country, mash$year),]

# DEPENDENT VARIABLE (1-YEAR LEAD)
mash <- mash[order(mash$country, mash$year),]
for (i in 1:dim(mash)[1]) mash$cou.a.d.1[i] <- mash$cou.a.d[i+1]
mash$cou.a.d.1 <- replace(mash$cou.a.d.1, which(mash$year >= 2013 | mash$year==mash$yrdied), NA)

# ELECTION YEAR (1-YEAR LEAD)
mash <- mash[order(mash$country, mash$year),]
for (i in 1:dim(mash)[1]) mash$nld.any.1[i] <- mash$nld.any[i+1]
mash$nld.any.1 <- replace(mash$nld.any.1, which(mash$year >= 2014 | mash$year==mash$yrdied), NA)

# PREDICTION YEAR (to keep this shit straight)
mash$predyr <- mash$year + 1

write.csv(mash, "outdata/mash.csv", row.names=FALSE)

#############################################
# Validation
#############################################

rf.f <- formula(as.factor(cou.a.d.1) ~ reg.afr + reg.eur + reg.amr + reg.eap + reg.mna + reg.sca +
  colbrit + colfrnc + colespn + postcw + ageln + xxxcimrln + cou.tries5d + world.ln + region.ln +
  slowgrowth + polcat1 + polcat2 + polcat3 + polcat7 + durableln + elcelethc + nld.any.1 + civconc )

logit.f <- formula(cou.a.d.1 ~ colbrit + colfrnc + colespn + postcw + ageln + xxxcimrln + 
  cou.tries5d + world.ln + region.ln + slowgrowth + polcat2 + polcat3 + polcat7 + durableln + elcelethc + nld.any.1 +
  civconc )

# Variable IDs
# cou.a.d.1    Any coup attempts (successful or failed) during next calendar year (0/1)
# reg.afr      Sub-Saharan African country (0/1)
# reg.eur      Eurasian country (0/1)
# reg.amr      American or Caribbean country (0/1)
# reg.eap      East Asia or Pacific country (0/1)
# reg.mna      Middle Eastern or North African country (0/1)
# reg.sca      South or Central Asian country (0/1)
# colbrit      Former British colony (0/1)
# colfrnc      Former French colony (0/1)
# colespn      Former Spanish colony (0/1)
# postcw       Post-Cold War period, 1991 or later (0/1)
# ageln        Country age, logged
# xxxcimrln    Infant mortality rate relative to annual global median, logged
# cou.tries5d  Any coup attempts in previous 5 years (0/1)
# world.ln     Count of other countries worldwide with any coup attempts during year, logged
# region.ln    Count of other countries in same region with any coup attempts during year, logged
# slowgrowth   Annual growth in GDP per capita less than 2 percent (0/1)
# polcat1      Political regime type: dictatorship (Polity -10 to -6)
# polcat2      Political regime type: anocracy (Polity -5 to 5)
# polcat3      Political regime type: democracy (Polity 6 to 10)
# polcat7      Political regime type: transition, interruption, or interregnum (polity -66, -77, or -88)
# durableln    Years since last major change in Polity score, logged
# elcelethc    Elite ethnicity is politically salient (0/1)
# nld.any.1    Any national executive, legislative, or constituent assembly elections during next year (0/1)
# civconc      Any violent civil conflict (0/1)


require(caret)
valdat <- subset(mash, year >= 1960 & year <= 2012 & is.na(cou.a.d.1) == FALSE)
y <- valdat$cou.a.d.1
valdat$k <- createFolds(y, k = 10, list = FALSE)
table(valdat$k, valdat$cou.a.d.1)

predit <- function(x) {
     train <- subset(valdat, k != x)
     test <- subset(valdat, k == x)
     test$logit.p <- predict(glm(logit.f, family = binomial, data = train, na.action = na.exclude),
       newdata = test, type = "response")
     require(randomForest)
     test$rf.p <- predict(randomForest(rf.f, data = train, na.action="na.exclude", ntree = 1000, mtry = 4, cutoff=c(0.3,0.7)),
       newdata = test, type = "prob", na.action = "na.exclude")[,2]
     test$mean.p <- (test$logit.p + test$rf.p)/2
     out <- subset(test, select = c(sftgcode, year, predyr, cou.a.d.1, mean.p, logit.p, rf.p, k))
     return(out)
}

test1 <- predit(1)
test2 <- predit(2)
test3 <- predit(3)
test4 <- predit(4)
test5 <- predit(5)
test6 <- predit(6)
test7 <- predit(7)
test8 <- predit(8)
test9 <- predit(9)
test10 <- predit(10)
out <- rbind(test1, test2, test3, test4, test5, test6, test7, test8, test9, test10)

# Distribution of AUC scores by fold
fun.auc <- function(df, x) {
  require(verification)
  mean <- roc.area(df$cou.a.d.1[out$k==x], df$mean.p[out$k==x])
  logit <- roc.area(df$cou.a.d.1[out$k==x], df$logit.p[out$k==x])
  rf <- roc.area(df$cou.a.d.1[out$k==x], df$rf.p[out$k==x])
  all <- c(x, mean$A, logit$A, rf$A )
  names(all) <- c("fold", "mean", "logit", "RF")
  return(all)
}

auc1 <- fun.auc(out, 1)
auc2 <- fun.auc(out, 2)
auc3 <- fun.auc(out, 3)
auc4 <- fun.auc(out, 4)
auc5 <- fun.auc(out, 5)
auc6 <- fun.auc(out, 6)
auc7 <- fun.auc(out, 7)
auc8 <- fun.auc(out, 8)
auc9 <- fun.auc(out, 9)
auc10 <- fun.auc(out, 10)
auctab <- as.data.frame(rbind(auc1, auc2, auc3, auc4, auc5, auc6, auc7, auc8, auc9, auc10))

# ROC curves by model
library(ROCR)
mean.pred <- prediction(out$mean.p, out$cou.a.d.1)
mean.roc <- performance(mean.pred, "tpr", "fpr")
mean.auc <- performance(mean.pred, measure = "auc")
logit.pred <- prediction(out$logit.p, out$cou.a.d.1)
logit.roc <- performance(logit.pred, "tpr", "fpr")
logit.auc <- performance(logit.pred, measure = "auc")
rf.pred <- prediction(out$rf.p, out$cou.a.d.1)
rf.roc <- performance(rf.pred, "tpr", "fpr")
rf.auc <- performance(rf.pred, measure = "auc")

png(file = "figs/roc.by.model.png",
     width=12, height=12, units='cm', bg='white', res=150)
plot(mean.roc, col = "black", lwd=2, add = FALSE)
plot(logit.roc, col = "blue", add = TRUE)
plot(rf.roc, col = "red", add = TRUE)
text(x=1,y=0.10,
     labels = paste("mean", substring(as.character(mean.auc@y.values),1,5), sep=' = '),
     pos=2, cex=0.75, col = "black")
text(x=1,y=0.05,
     labels = paste("logit", substring(as.character(logit.auc@y.values),1,5), sep=' = '),
     pos=2, cex=0.75, col = "blue")
text(x=1,y=0,
     labels = paste("RF", substring(as.character(rf.auc@y.values),1,5), sep=' = '),
     pos=2, cex=0.75, col = "red")
dev.off()

##########################################
# Forecasts
##########################################

estdat <- subset(mash, predyr <= 2010)

logit.mod <- glm(logit.f, data = estdat, family = binomial, na.action = na.exclude)
rf.mod <- randomForest(rf.f, data = estdat, na.action="na.exclude", ntree = 1000, mtry = 4, cutoff=c(0.3,0.7))

# Hard code slow growth for countries with missing values in 2013 using last from CIA World Factbook
mash$slowgrowth[mash$predyr==2014 & mash$sftgcode=="PRK"] <- 1
mash$slowgrowth[mash$predyr==2014 & mash$sftgcode=="SOM"] <- 0
mash$slowgrowth[mash$predyr==2014 & mash$sftgcode=="SYR"] <- 1
mash$slowgrowth[mash$predyr==2014 & mash$sftgcode=="CUB"] <- 0

mash$logit.p <- predict(logit.mod, newdata = mash, type = "response")
mash$rf.p <- predict(rf.mod, newdata = mash, type = "prob", na.action = "na.exclude")[,2]
mash$mean.p <- (mash$logit.p + mash$rf.p)/2

write.csv(mash, "outdata/coup.predictions.csv", row.names=FALSE)

##########################################
# Plots
##########################################

require(lattice)
require(Hmisc)

# 2011
pred11 <- subset(mash, predyr==2011, select=c(country, cou.a.d.1, mean.p))
pred11 <- pred11[order(-pred11$mean.p),]
row.names(pred11) <- NULL
pred11$country <- as.character(pred11$country)
pred11$country[pred11$country=="Congo-Kinshasa"] <- "DRC"
pred11$country[pred11$country=="Congo-Brazzaville"] <- "Republic of Congo"
pred11$rank <- as.numeric(as.character(row.names(pred11)))
condcol <- ifelse(pred11$cou.a.d.1==1, "deeppink1", "gray")
png(file = "figs/forecast.dotplot.2011.png", width=14, height=18, units='cm', bg='white', res=150)
dotchart2(pred11$mean.p[1:40], labels=pred11$country[1:40],
  lines=TRUE, lwd=0.05, lty=3, sort=FALSE, dotsize=1.25, pch=20,
  col=condcol, cex.labels=0.75, xlim=c(0,0.25) )
title(main=list("Risk of Any Coup Attempts in 2011", cex=1),
    sub = list(paste("Coup attempts outside top 40:", paste(pred11$country[pred11$cou.a.d.1==1 & pred11$rank > 40]), sep=" "), cex=0.8))
dev.off()

# 2012
pred12 <- subset(mash, predyr==2012, select=c(country, cou.a.d.1, mean.p))
pred12 <- pred12[order(-pred12$mean.p),]
row.names(pred12) <- NULL
pred12$country <- as.character(pred12$country)
pred12$country[pred12$country=="Congo-Kinshasa"] <- "DRC"
pred12$country[pred12$country=="Congo-Brazzaville"] <- "Republic of Congo"
pred12$rank <- as.numeric(as.character(row.names(pred12)))
condcol <- ifelse(pred12$cou.a.d.1==1, "deeppink1", "gray")
png(file = "figs/forecast.dotplot.2012.png", width=14, height=18, units='cm', bg='white', res=150)
dotchart2(pred12$mean.p[1:40], labels=pred12$country[1:40],
  lines=TRUE, lwd=0.05, lty=3, sort=FALSE, dotsize=1.25, pch=20,
  col=condcol, cex.labels=0.75, xlim=c(0,0.25) )
title(main=list("Risk of Any Coup Attempts in 2012", cex=1),
    sub = list(paste("Coup attempts outside top 40:", paste(pred11$country[pred12$cou.a.d.1==1 & pred12$rank > 40]), sep=" "), cex=0.8))
dev.off()

# 2013
pred13 <- subset(mash, predyr==2013, select=c(country, cou.a.d.1, mean.p))
pred13 <- pred13[order(-pred13$mean.p),]
row.names(pred13) <- NULL
pred13$country <- as.character(pred13$country)
pred13$country[pred13$country=="Congo-Kinshasa"] <- "DRC"
pred13$country[pred13$country=="Congo-Brazzaville"] <- "Republic of Congo"
pred13$rank <- as.numeric(as.character(row.names(pred13)))
condcol <- ifelse(pred13$cou.a.d.1==1, "deeppink1", "gray")
png(file = "figs/forecast.dotplot.2013.png", width=14, height=18, units='cm', bg='white', res=150)
dotchart2(pred13$mean.p[1:40], labels=pred13$country[1:40],
  lines=TRUE, lwd=0.05, lty=3, sort=FALSE, dotsize=1.25, pch=20,
  col=condcol, cex.labels=0.75, xlim=c(0,0.25) )
title(main=list("Risk of Any Coup Attempts in 2013", cex=1),
    sub = list(paste("Coup attempts outside top 40:", paste(pred11$country[pred13$cou.a.d.1==1 & pred13$rank > 40]), sep=" "), cex=0.8))
dev.off()

# 2014
pred14 <- subset(mash, predyr==2014, select=c(country, sftgcode, mean.p, logit.p, rf.p))
pred14 <- pred14[order(-pred14$mean.p),]
pred14$country <- as.character(pred14$country)
pred14$country[pred14$country=="Congo-Kinshasa"] <- "DRC"
pred14$country[pred14$country=="Congo-Brazzaville"] <- "Republic of Congo"
png(file = "figs/forecast.dotplot.2014.png", width=14, height=18, units='cm', bg='white', res=150)
dotchart2(pred14$logit.p[1:40], labels=pred14$country[1:40],
  lines=TRUE, lwd=0.05, lty=3, sort=FALSE, dotsize=1.25, pch=20,
  col="gray", cex.labels=0.75, xlim=c(0,0.4) )
dotchart2(pred14$rf.p[1:40], labels=pred14$country[1:40],
  lines=TRUE, lwd=0.05, lty=3, sort=FALSE, dotsize=1.25, pch=20,
  col="gray", cex.labels=0.75, xlim=c(0,0.4), add = TRUE )
dotchart2(pred14$mean.p[1:40], labels=pred14$country[1:40],
  lines=TRUE, lwd=0.05, lty=3, sort=FALSE, dotsize=1.25, pch=20,
  col="firebrick", cex.labels=0.75, xlim=c(0,0.4), add = TRUE )
title(main=list("Risk of Any Coup Attempts in 2014", cex=1))
dev.off()

# Map it
require(rworldmap)

pred14$country <- as.character(pred14$country)

# Join the data to a map
map2014 <- joinCountryData2Map(pred14, nameJoinColumn = "country", joinCode = "NAME", verbose = TRUE)

# Correct mismatches
pred14$country[pred14$country=="Timor Leste"] <- "Timor-Leste"
pred14$country[pred14$country=="DRC"] <- "Democratic Republic of Congo"

# Redo with mismatches corrected
map2014 <- joinCountryData2Map(pred14, nameJoinColumn = "country", joinCode = "NAME", verbose = TRUE)

# Map the scores
png("figs/forecast.heatmap.2014.png", width=800, height=450, bg="white")
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
map.score <- mapCountryData(map2014,
  nameColumnToPlot="mean.p",
  addLegend = FALSE,
  numCats = 5, catMethod="quantiles",
  colourPalette = "heat", borderCol = "gray",
  mapTitle = "Risk of Any Coup Attempts in 2014")
do.call(addMapLegend, c(map.score, legendWidth=0.5, legendMar = 2))
mtext("map made using rworldmap             ", line=-4, side=1, adj=1, cex=0.8)
dev.off()

