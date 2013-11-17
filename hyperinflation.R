# HYPERINFLATION AND POLITICAL INSTABILITY
# Jay Ulfelder
# 2013-11-16

# Clear workspace and load data
# Original source: http://www.cato.org/publications/working-paper/world-hyperinflations
# URL for my .csv: https://drive.google.com/file/d/0B5wyt4eDq98GYXJIZFJWRWhOTU0/edit?usp=sharing
rm(list=ls(all=TRUE))
data <- read.csv("henketable.csv")
attach(data)

# Histogram of durations
data$duration <- ifelse(endyr == startyr, endmo - startmo, endmo + (12 * (endyr - startyr)) + (12 - startmo) ) 
png(file="c:/documents and settings/jay/my documents/blog posts/hyperinflation.dur.png", width=400, height=400, bg="white")
par(mai=c(0.5,1,0.25,0.25))
hist(data$duration, main = NULL, xlab = "Months", ylab = "Frequency", col = "blue", border = "white")
dev.off()

# Histogram of average daily rates
png(file="c:/documents and settings/jay/my documents/blog posts/hyperinflation.rate.png", width=400, height=400, bg="white")
par(mai=c(0.5,1,0.25,0.25))
hist(data$ratedaily, main = NULL, xlab = "Percent Change", ylab = "Frequency", col = "orange", border = "white")
dev.off()

# Annual counts
data$event <- 1
yrsum <- tapply(data$event, startyr, sum)
require(reshape)
yrdat <- melt(yrsum)
names(yrdat) <- c("year", "count")
year <- as.data.frame(seq(1790, 2012))
tsdat <- merge(year, yrdat, all.x = TRUE)
tsdat$count[is.na(tsdat$count) == TRUE] <- 0
png(file="c:/documents and settings/jay/my documents/blog posts/hyperinflation.ts.png", width=800, height=300, bg="white")
par(mai=c(0.5,1,0.25,0.25))
plot(tsdat$year, tsdat$count, type = "h", col = "red", xlab = "", ylab = "", axes = FALSE)
axis(1,at=c(1795,1922,1945,1992), tick=FALSE)
axis(2,at=c(0,5,10,15,20),tick=FALSE)
dev.off()

# Crosstabs
table(data$ldrchange)
require(gmodels)
CrossTable(data$democracy, data$ldrchange)

# Rough model
mod <- glm(ldrchange ~ democracy + duration + I(democracy * duration) + log(ratedaily) + I(democracy * log(ratedaily)), data = data, family = "binomial")
summary(mod)

# Plot for spell duration and survival
duration <- seq(1,36,1)
democracy <- rep(0, times = 36)
ratedaily <- rep(3.5, times = 36)
preddat <- as.data.frame(cbind(duration,democracy,ratedaily))
preddat$p <- predict(mod, newdata = preddat, type = "response")
png(file="c:/documents and settings/jay/my documents/blog posts/hyperinflation.duration.png", width=500, height=500, bg="white")
plot(preddat$duration, preddat$p, type = "l", col = "red", xlab = "duration (months)", ylab = "", axes = FALSE, lwd = 2)
axis(1,at=c(1,6,12,18,24,30,36), tick=FALSE)
axis(2,at=c(0,0.1,0.2,0.3,0.4,0.5), tick=FALSE)
dev.off()
