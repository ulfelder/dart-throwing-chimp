# ANIMATED MAP OF COUP ATTEMPTS WORLDWIDE, 1946-2013
# Jay Ulfelder
# 2013-11-22

# Code to make charts in "Animated Map of Coup Attempts Worldwide, 1946-2013" (22 November 2013)

# DATA

# Clear workspace.
rm(list=ls(all=TRUE))

# Get .csv from this link to my Google Drive
# https://docs.google.com/file/d/0B5wyt4eDq98GQkJPcGx6cFNpd3M/edit?usp=sharing

# Load it.
df <- read.csv("coups.csv")


# TIME-SERIES PLOTS
# Create annual summaries
success <- tapply(ifelse(df$cou.s.d==1, 1, 0), df$year, sum, na.rm = TRUE)
fail <- tapply(ifelse(df$cou.f.d==1, 1, 0), df$year, sum, na.rm = TRUE)
years <- seq(from=range(df$year)[1], to=range(df$year)[2], by=1)
df$age <- df$year - df$yrborn
countries <- tapply(ifelse(is.na(df$age)== FALSE, 1, 0), df$year, sum, na.rm = TRUE)
datyr <- as.data.frame(cbind(years, countries, success, fail))

png(file="coups19462013.png", width=800, height=600, bg="white")
par(mfrow=c(2,1))
plot(datyr$years, datyr$success, type="h", col="goldenrod4", lwd=2,
     ylim=c(0,12), xlab="", ylab="", main = "Countries with Any Successful Coups")
plot(datyr$years, datyr$fail, type="h", col="goldenrod1", lwd=2,
     ylim=c(0,12), xlab="", ylab="", main = "Countries with Any Failed Coups")
dev.off()

# MAPS AND ANIMATION
require(rworldmap)
# NOTE: Because country names must match, ensuing code is version dependent. This was run
# using version 1.02-6. You can check which version you're running with:
# packageDescription('rworldmap')$Version
# To install the latest version, use:
# install.packages("rworldmap", repos="http://R-Forge.R-project.org")

# Rename some countries to match the names used in 'rworldmap'
df$country <- as.character(df$country)
df$country <- replace(df$country, df$country=="Ivory Coast", "Cote d'Ivoire")
df$country <- replace(df$country, df$country=="Congo-Brazzaville", "Congo")
df$country <- replace(df$country, df$country=="Congo-Kinshasa", "Democratic Republic of the Congo")
df$country <- replace(df$country, df$country=="Iran", "Iran (Islamic Republic of)")
df$country <- replace(df$country, df$country=="Macedonia", "The former Yugoslav Republic of Macedonia")
df$country <- replace(df$country, df$country=="Laos", "Lao People's Democratic Republic")
df$country <- replace(df$country, df$country=="Moldova", "Republic of Moldova")
df$country <- replace(df$country, df$country=="Vietnam", "Viet Nam")
df$country <- replace(df$country, df$country=="Syria", "Syrian Arab Republic")
df$country <- replace(df$country, df$country=="Tanzania", "United Republic of Tanzania")
df$country <- replace(df$country, df$country=="North Korea", "Korea, Democratic People's Republic of")
df$country <- replace(df$country, df$country=="South Korea", "Korea, Republic of")
df$country <- replace(df$country, df$country=="Timor Leste", "Timor-Leste")
df$country <- replace(df$country, df$country=="Myanmar", "Burma")

# Function for adding empty country-years for pre- or post-independence spells
additup <- function(x) {
                        id <- rep(x, each = length(seq(min(df$year), max(df$year), 1)))
                        yr <- rep(seq(min(df$year), max(df$year), 1), times = length(x))
                        d <- as.data.frame(cbind(id, yr))
                        names(d) <- c("country", "year")
                        rm(id, yr)
                        d$country <- as.character(d$country)
                        d$year <- as.numeric(as.character(d$year))
                        df <- merge(df, d, all = TRUE)
                        return(df)
                        }

# Paint former Soviet republics with values for "Soviet Union" pre-1991
ssrs <- c("Russia", "Estonia", "Latvia", "Lithuania", "Belarus",
          "Republic of Moldova", "Georgia", "Armenia", "Azerbaijan", "Tajikistan",
          "Turkmenistan", "Kazakhstan", "Uzbekistan", "Kyrgyzstan", "Ukraine")
df <- additup(ssrs)
ussr <- which(df$year < 1991 & (df$country=="Russia" | df$country=="Estonia" | df$country=="Latvia" |
              df$country=="Lithuania" | df$country=="Belarus" | df$country=="Republic of Moldova" |
              df$country=="Georgia" | df$country=="Armenia" | df$country=="Azerbaijan" |
              df$country=="Tajikistan" | df$country=="Turkmenistan" | df$country=="Kazakhstan" |
              df$country=="Uzbekistan" | df$country=="Kyrgyzstan" | df$country=="Ukraine") )
df$cou.s.d <- replace(df$cou.s.d, ussr, 0)
df$cou.f.d <- replace(df$cou.f.d, ussr, 0)

# Same for Czechoslovakia
czech <- c("Czech Republic", "Slovakia")
df <- additup(czech)
czechit <- which(df$year<1993 & (df$country=="Czech Republic" | df$country=="Slovakia"))
df$cou.s.d <- replace(df$cou.s.d, czechit, df$cou.s.d[df$country=="Czechoslovakia" & df$year<1993] )
df$cou.f.d <- replace(df$cou.f.d, czechit, df$cou.f.d[df$country=="Czechoslovakia" & df$year<1993] )

# Ditto for Yugoslavia
yugo <- c("Bosnia and Herzegovina", "The former Yugoslav Republic of Macedonia", "Serbia", "Montenegro", "Croatia", "Slovenia")
df <- additup(yugo)
yuggit <- which( df$year<1991 & (df$country=="Bosnia and Herzegovina" | df$country=="The former Yugoslav Republic of Macedonia" |
  df$country=="Serbia" | df$country=="Montenegro" | df$country=="Croatia" | df$country=="Slovenia"))
df$cou.s.d <- replace(df$cou.s.d, yuggit, df$cou.s.d[df$country=="Yugoslavia" & df$year<1991] )
df$cou.f.d <- replace(df$cou.f.d, yuggit, df$cou.f.d[df$country=="Yugoslavia" & df$year<1991] )
# Broken
fyugit <- which( df$year>=1992 & df$year<=2005 & (df$country=="Serbia" | df$country=="Montenegro") )
df$cou.s.d <- replace(df$cou.s.d, fyugit, df$cou.s.d[df$sftgcode=="YGS" & df$year>=1992 & df$year<=2005] )
df$cou.f.d <- replace(df$cou.f.d, fyugit, df$cou.f.d[df$sftgcode=="YGS" & df$year>=1992 & df$year<=2005] )

# Ethiopia and Eritrea
eritrea <- "Eritrea"
df <- additup(eritrea)
eri <- which( df$year<1993 & df$country=="Eritrea" )
df$cou.s.d <- replace(df$cou.s.d, eri, df$cou.s.d[df$country=="Ethiopia" & df$year<1993] )
df$cou.f.d <- replace(df$cou.f.d, eri, df$cou.f.d[df$country=="Ethiopia" & df$year<1993] )

# Vietnam
viet <- "Viet Nam"
df <- additup(viet)
df$cou.s.d <- replace(df$cou.s.d, which(df$country=="Viet Nam" & df$year>=1954 & df$year<=1975),
  df$cou.s.d[df$country=="South Vietnam" & df$year >= 1954 & df$year<=1975])
df$cou.f.d <- replace(df$cou.f.d, which(df$country=="Viet Nam" & df$year>=1954 & df$year<=1975),
  df$cou.f.d[df$country=="South Vietnam" & df$year >= 1954 & df$year<=1975])

# Yemen
yemen <- "Yemen"
df <- additup(yemen)
df$cou.s.d <- replace(df$cou.s.d, which(df$country=="Yemen" & df$year<1990), df$cou.s.d[df$sftgcode=="YAR" & df$year<1990])
df$cou.f.d <- replace(df$cou.f.d, which(df$country=="Yemen" & df$year<1990), df$cou.f.d[df$sftgcode=="YAR" & df$year<1990])
                                   
# Germany
germany <- "Germany"
df <- additup(germany)
df$cou.s.d <- replace(df$cou.s.d, which(df$country=="Germany" & df$year < 1990), 0)
df$cou.f.d <- replace(df$cou.f.d, which(df$country=="Germany" & df$year < 1990), 0)

# South Sudan
ssudan <- "South Sudan"
df <- additup(ssudan)
df$cou.s.d <- replace(df$cou.s.d, which(df$country=="South Sudan" & df$year >= 1956 & df$year < 2011), 
     df$cou.s.d[df$country=="Sudan" & df$year >= 1956 & df$year < 2011] )
df$cou.f.d <- replace(df$cou.f.d, which(df$country=="South Sudan" & df$year >= 1956 & df$year < 2011),
     df$cou.f.d[df$country=="Sudan" & df$year >= 1956 & df$year < 2011])

# Create 3-level cat var for none, onset, cou.s.d
df$status <- ifelse(df$cou.s.d==1, 2, ifelse(df$cou.f.d==1, 1, ifelse(is.na(df$cou.s.d)==FALSE, 0, NA ))) 
df$status <- factor(df$status, levels=c(0,1,2), labels=c("none", "failed", "successful"))

# Function to make a map for a one-year slice
mapit <- function(x) {
                      require(rworldmap)
                      jpeg(file=paste("mk", as.character(x), ".jpg", sep=""),
                           width=5, height=4, units='in', bg='white', res=150, quality=100)
                      par(mai = c(0, 0, 0.2, 0), xaxs = "i", yaxs = "i")
                      mapCountryData(joinCountryData2Map(subset(df, year==x), joinCode = "NAME",
                           nameJoinColumn = "country"),
                      nameColumnToPlot="status", numCats = 3, catMethod="categorical",
                           colourPalette=c("palegoldenrod", "goldenrod1", "goldenrod4"),
                      mapTitle=paste(as.character(x)) )
                      mtext("map made using rworldmap", line=-1, side=1, adj=1, cex=0.5)
                      dev.off()
                      }

setwd("[insert yours here]")

# Run the function for a selected range of years
for( i in seq(1946,2013,1) ) { mapit(i) }

# Make a .gif.
# NOTE: The code that follows is specific to Windows and requires prior installation of
# ImageMagick (http://www.imagemagick.org/script/index.php). If you install ImageMagick,
# be sure the directory in the first ani.options() line below matches the one where it's
# installed.
library(animation)
ani.options( convert = shQuote("C:/Program Files (x86)/ImageMagick-6.7.6-q16/convert.exe") )
ani.options( outdir = getwd() )
ani.options( nmax = 2013 - 1945)
ani.options( ani.type = "jpeg", ani.dev = "jpeg")
im.convert("mk*.jpg", output = "coups19462013.gif")
