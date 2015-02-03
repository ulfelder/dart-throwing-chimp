# Load required packages
library(plyr)
library(caret)
library(reshape2)
library(verification)
library(effects)
library(mgcv)
library(survival)

# Set seed to make sampling for k-fold CV replicable
set.seed(709)

# Load the various data sets, all of which can be found in a folder on my Google Drive, here:
# https://drive.google.com/folderview?id=0B5wyt4eDq98GcUVLM0VreFBmelk&usp=drive_web
Transits <- read.csv("dad2010.csv", stringsAsFactors=FALSE)
Age <- read.csv("age.csv", stringsAsFactors=FALSE)
IM <- read.csv("imr.csv", stringsAsFactors=FALSE)
GDP <- read.csv("gdp.csv", stringsAsFactors=FALSE)

# Prep for merging by matching names and removing unneeded columns
Transits <- rename(Transits, c("pitfcode" = "sftgcode"))
GDP <- subset(GDP, select=c(sftgcode, year, mad.gdppc))

# Merge them
AF <- merge(Transits, Age, all.x=TRUE)
AF <- merge(AF, IM, all.x=TRUE)
AF <- merge(AF, GDP, all.x=TRUE) # Adds five rows; Serbia, I think?

# Make a measure of median age that's centered on 30 and flattens below 25 and above 35
# http://www.fpri.org/articles/2012/01/life-begins-after-25-demography-societal-timing-arab-spring
AF$agecurve <- ifelse(AF$median.age - 30 < 0,
  -1 * sqrt(abs(AF$median.age - 30)),
  sqrt(AF$median.age - 30))

## CROSS-VALIDATION ##

# Trim data set to required variables with no missing values
varlist <- c("sftgcode", "year", "rgjtdem", "rgjdura", "rgjhd", "mad.gdppc", "cnsimr", "median.age", "agecurve")
# Get subset of authoritarian years that persist or end in transition (omit state collapses)
valdat <- subset(AF, rgjdura > 0 & rgjtdem > -6, select = varlist)
# Listwise deletion now, so comparison is always apples to apples
valdat <- na.omit(valdat) 
table(valdat$rgjtdem)

# Create folds for 10 iterations of k-fold CV
dv <- "rgjtdem"
valdat$ik1 <- createFolds(y = as.factor(valdat[,dv]), k = 5, list = FALSE)
valdat$ik2 <- createFolds(y = as.factor(valdat[,dv]), k = 5, list = FALSE)
valdat$ik3 <- createFolds(y = as.factor(valdat[,dv]), k = 5, list = FALSE)
valdat$ik4 <- createFolds(y = as.factor(valdat[,dv]), k = 5, list = FALSE)
valdat$ik5 <- createFolds(y = as.factor(valdat[,dv]), k = 5, list = FALSE)
valdat$ik6 <- createFolds(y = as.factor(valdat[,dv]), k = 5, list = FALSE)
valdat$ik7 <- createFolds(y = as.factor(valdat[,dv]), k = 5, list = FALSE)
valdat$ik8 <- createFolds(y = as.factor(valdat[,dv]), k = 5, list = FALSE)
valdat$ik9 <- createFolds(y = as.factor(valdat[,dv]), k = 5, list = FALSE)
valdat$ik10 <- createFolds(y = as.factor(valdat[,dv]), k = 5, list = FALSE)

# Create function to generate predicted probabilities by iteration and fold of k-fold CV
predit <- function(i, x) {
  var <- paste0("ik", as.character(i))
  train <- valdat[ -which(valdat[var] == x),]
  test <- valdat[ which(valdat[var] == x),]
  test$base.p <- predict(glm(rgjtdem ~ rgjhd + log(rgjdura) + I(rgjhd * log(rgjdura)),
    family=binomial, data=train, na.action=na.exclude), newdata=test, type="response")
  test$gdp.p <- predict(glm(rgjtdem ~ rgjhd + log(rgjdura) + I(rgjhd * log(rgjdura)) + log(mad.gdppc),
    family=binomial, data=train, na.action=na.exclude), newdata=test, type="response")
  test$im.p <- predict(glm(rgjtdem ~ rgjhd + log(rgjdura) + I(rgjhd * log(rgjdura)) + log(cnsimr),
    family=binomial, data=train, na.action=na.exclude), newdata=test, type="response")
  test$ageraw.p <- predict(glm(rgjtdem ~ rgjhd + log(rgjdura) + I(rgjhd * log(rgjdura)) + median.age,
    family= binomial, data=train, na.action=na.exclude), newdata=test, type="response")
  test$agecurve.p <- predict(glm(rgjtdem ~ rgjhd + log(rgjdura) + I(rgjhd * log(rgjdura)) + agecurve,
    family=binomial, data=train, na.action=na.exclude), newdata=test, type="response")
  test$iteration <- i
  test$k <- x
  out <- subset(test, select = c(sftgcode, year, rgjtdem, base.p, gdp.p, im.p, ageraw.p, agecurve.p, iteration, k))
  return(out)
}

out <- rbind(predit(1,1), predit(1,2), predit(1,3), predit(1,4), predit(1,5),
  predit(2,1), predit(2,2), predit(2,3), predit(2,4), predit(2,5),
  predit(3,1), predit(3,2), predit(3,3), predit(3,4), predit(3,5),
  predit(4,1), predit(4,2), predit(4,3), predit(4,4), predit(4,5),
  predit(5,1), predit(5,2), predit(5,3), predit(5,4), predit(5,5),
  predit(6,1), predit(6,2), predit(6,3), predit(6,4), predit(6,5),
  predit(7,1), predit(7,2), predit(7,3), predit(7,4), predit(7,5),
  predit(8,1), predit(8,2), predit(8,3), predit(8,4), predit(8,5),
  predit(9,1), predit(9,2), predit(9,3), predit(9,4), predit(9,5),
  predit(10,1), predit(10,2), predit(10,3), predit(10,4), predit(10,5))

# Melt results to make data tidy
out.melt <- melt(out, id = c("sftgcode", "year", "rgjtdem", "k", "iteration"), na.rm = FALSE)

# Get tables of accuracy stats by model
# By iteration:
acc.by.iter <- ddply(out.melt, .(iteration, variable), summarise,
  brier = mean((rgjtdem - value)^2),
  log = mean((rgjtdem * log(value)) + ((1 - rgjtdem) * log(1 - value))),
  auc = roc.area(rgjtdem, value)$A )
# Overall:  
acc.overall <- ddply(acc.by.iter, .(variable), summarise,
  brier = mean(brier),
  log = mean(log),
  auc = mean(auc))
# Showing that this is equivalent to previous version:
acc.overall2 <- ddply(out.melt, .(variable), summarise,
  brier = mean((rgjtdem - value)^2),
  log = mean((rgjtdem * log(value)) + ((1 - rgjtdem) * log(1 - value))),
  auc = roc.area(rgjtdem, value)$A)

# Density plots of accuracy
png("transit.auc.by.fold.png", width=12, height=12, units='cm', bg='white', res=150)
par(mai=c(0.5,0.5,0.5,0.5))
plot(with(subset(acc.by.iter, variable=="base.p"), density(auc)),
  xlim=c(0.7,0.85), ylim=c(0,35), lwd=2, main="", col="#fdae61",
  bty = "n", mai = c(1.5, 0.5, 0.25, 0.25), xaxt = "n", yaxt = "n", xlab="AUC", ylab="")
axis(1, tick = FALSE)
lines(with(subset(acc.by.iter, variable=="ageraw.p"), density(auc)), col="gray25", xlim=c(0.7,0.85), ylim=c(0,35), lwd=2)
lines(with(subset(acc.by.iter, variable=="gdp.p"), density(auc)), col="#2c7bb6", xlim=c(0.7,0.85), ylim=c(0,35), lwd=2)
lines(with(subset(acc.by.iter, variable=="im.p"), density(auc)), col="#d7191c", xlim=c(0.7,0.85), ylim=c(0,35), lwd=2)
text(x=0.86, y=35, paste("age", substr(as.character(acc.overall[4,4]),1,5), sep = " = "),
  col="gray25", pos=2)
text(x=0.86, y=32.5, paste("IM", substr(as.character(acc.overall[3,4]),1,5), sep = " = "),
  col="#d7191c", pos=2)
text(x=0.86, y=30, paste("base", substr(as.character(acc.overall[1,4]),1,5), sep = " = "),
  col="#fdae61", pos=2)
text(x=0.86, y=27.5, paste("GDP", substr(as.character(acc.overall[2,4]),1,5), sep = " = "),
  col="#2c7bb6", pos=2)
dev.off()

# Inspect models with age variables estimated from whole data set
ageraw <- glm(rgjtdem ~ rgjhd + log(rgjdura) + I(rgjhd * log(rgjdura)) + median.age,
  family= binomial, data=valdat, na.action=na.exclude)
summary(ageraw)
agecurve <- glm(rgjtdem ~ rgjhd + log(rgjdura) + I(rgjhd * log(rgjdura)) + agecurve,
  family= binomial, data=valdat, na.action=na.exclude)
summary(agecurve)

# Plot the marginal effect of median age
eff.age <- allEffects(ageraw)
png("transit.ageraw.effect.png", width=10, height=10, units='cm', bg='white', res=150)
plot(eff.age, "median.age")
dev.off()

# Try semiparametric models with smoothing splines for time and age
age.gam1 <- gam(rgjtdem ~ rgjhd + log(rgjdura) + s(year) + s(median.age),
  family= binomial, data=valdat, na.action=na.exclude)
png("transit.ageraw.effect.spline.with.year.png", width=16, height=10, units='cm', bg='white', res=150)
plot(age.gam1, pages=1)
dev.off()

# Do one for GDP to see if it it fits stronger with splines (it doesn't)
age.gam2 <- gam(rgjtdem ~ rgjhd + log(rgjdura) + s(year) + s(log(mad.gdppc)),
  family= binomial, data=valdat, na.action=na.exclude)
png("transit.gdp.effect.spline.with.year.png", width=10, height=10, units='cm', bg='white', res=150)
plot(age.gam2, select=2)
dev.off()
