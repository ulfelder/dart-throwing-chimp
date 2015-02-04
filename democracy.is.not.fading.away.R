# This goes with "Democracy Is Not Fading Away" (September 21, 2012)
# http://dartthrowingchimp.wordpress.com/2012/09/21/democracy-is-not-fading-away/

# Note: The script loads the data from a local folder, but you can download them from the Dataverse:
# http://thedata.harvard.edu/dvn/dv/ulfelder

# Load data.
dad <- read.csv("dad2010.csv")

# Generate annual counts.
transitions <- tapply(ifelse(dad$rgjtdem==1, 1, 0), dad$year, sum, na.rm=T)
breakdowns <- tapply(ifelse(dad$rgjtaut==1, 1, 0), dad$year, sum, na.rm=T)
countries <- tapply(ifelse(is.na(dad$rgjtrans)==T, 0, 1), dad$year, sum, na.rm=T)
democs <- tapply(ifelse(is.na(dad$rgjtaut)==T, 0, 1), dad$year, sum, na.rm=T)
autocs <- tapply(ifelse(is.na(dad$rgjtdem)==T, 0, 1), dad$year, sum, na.rm=T)

# Make a data frame with years.
years <- seq(from=1955, to=2010, by=1)
dadyr <- as.data.frame(cbind(years, transitions, breakdowns, countries, democs, autocs))

# Generate rates.
dadyr$demrate <- dadyr$transitions/dadyr$autocs
dadyr$autrate <- dadyr$breakdowns/dadyr$democs

# Plots
jpeg(file='annual transition counts 1955 2010.jpg',
     width=16, height=14, units='cm', bg='white', res=150, quality=100)
par(mfrow=c(2,1))
par(mar=c(3,4,1,1))
plot(dadyr$years, dadyr$transitions, type='h', col="blue", lwd=2,
     ylim=c(0,10), xlab="", ylab="To Democracy")
plot(dadyr$years, dadyr$breakdowns, type='h', col="red", lwd=2,
     ylim=c(0,10), xlab="", ylab="To Autocracy")
dev.off()

jpeg(file='annual transition rates 1955 2010.jpg',
     width=16, height=14, units='cm', bg='white', res=150, quality=100)
par(mfrow=c(2,1))
par(mar=c(3,3,1,1))
plot(dadyr$years, dadyr$demrate, ylim=c(0,0.1), col="blue", pch=20, xlab="", ylab="")
lines(lowess(dadyr$years, dadyr$demrate, f=0.2), col="blue", lwd=2)
plot(dadyr$years, dadyr$autrate, ylim=c(0,0.1), col="red", pch=20, xlab="", ylab="")
lines(lowess(dadyr$years, dadyr$autrate, f=0.2), col="red", lwd=2)
dev.off()
