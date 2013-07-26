# This script implements the analysis in "Coups Slow Economic Growth" (11 July 2013)
# http://dartthrowingchimp.wordpress.com/2013/07/11/coups-slow-economic-growth/

##################################
# Load Data & Packages
##################################

# Get .csv called coup.impact.data.csv from
# https://docs.google.com/file/d/0B5wyt4eDq98Gd1BFcW0xNlZiaEE/edit?usp=sharing

# Load data.
beerdat <- read.csv("coup.impact.data.csv", header = TRUE)

# Load required packages.
library(MatchIt)
library(Zelig)

###################################
# Estimate Effects
###################################

# Relevant Variables
# coupd  Any successful coups (binary)
# madgdppc.n  GDP per capita, per Maddison (n-yr lag)
# SFTGTPOP.n  Population size, 1000s (n-yr lag)
# polscore.n  Polity score (n-yr lag)
# polscore2.n  Polity score squared (n-yr lag)
# couact5d.n  Any successful or failed coups in previous 5 yrs (n-yr lag)
# madgdppcg.n  Annual change in GDP per capita, per Maddison (n-yr lag)
# postcw  Post Cold War era, as 1991 or later (binary)

# Year t (where t is year when coup occurs)

# Listwise deletion of missing values.
coupdat1 <- subset(beerdat, is.na(coupd)==F &
            is.na(madgdppc.1)==F & is.na(SFTGTPOP.1)==F & is.na(polscore.1)==F & is.na(polscore2.1)==F & is.na(couact5d.1)==F &
            is.na(madgdppcg.1)==F &
            is.na(madgdppcg)==F,
            select=c(code, year, coupd, madgdppc.1, SFTGTPOP.1, polscore.1, polscore2.1, couact5d.1, postcw, madgdppcg.1, madgdppcg)  )

# Run matching.
coup1.out <- matchit(coupd ~ log(madgdppc.1) + polscore.1 + polscore2.1 + postcw + couact5d.1,
         data = coupdat1,
         method = "cem" )

coup1.z.out <- zelig(madgdppcg ~ coupd + madgdppcg.1 + log(madgdppc.1) + polscore.1 + polscore2.1 + postcw + couact5d.1,
              data = match.data(coup1.out),
              model = 'ls')

# Estimate effect.
coup1.x.out <- setx(coup1.z.out, coupd = 0)
coup1.x1.out <- setx(coup1.z.out, coupd = 1)
coup1.s.out <- sim(coup1.z.out, x = coup1.x.out, x1 = coup1.x1.out)

### Year t + 1

# Listwise deletion of missing values.
coupdat2 <- subset(beerdat, is.na(coupd.1)==F &
            is.na(madgdppc.2)==F & is.na(SFTGTPOP.2)==F & is.na(polscore.2)==F & is.na(polscore2.2)==F & is.na(couact5d.2)==F & is.na(postcw.1)==F & 
            is.na(madgdppcg.2)==F &
            is.na(madgdppcg)==F,
            select=c(code, year, coupd.1, madgdppc.2, SFTGTPOP.2, polscore.2, polscore2.2, couact5d.2, postcw.1, madgdppcg.2, madgdppcg)  )

# Run matching.
coup2.out <- matchit(coupd.1 ~ log(madgdppc.2) + polscore.2 + polscore2.2 + postcw.1 + couact5d.2,
         data = coupdat2,
         method = 'cem' )

coup2.z.out <- zelig(madgdppcg ~ coupd.1 + madgdppcg.2 + log(madgdppc.2) + polscore.2 + polscore2.2 + postcw.1 + couact5d.2,
              data = match.data(coup2.out),
              model = 'ls')

# Estimate effect.
coup2.x.out <- setx(coup2.z.out, coupd.1 = 0)
coup2.x1.out <- setx(coup2.z.out, coupd.1 = 1)
coup2.s.out <- sim(coup2.z.out, x = coup2.x.out, x1 = coup2.x1.out)

### Year t + 2

# Listwise deletion of missing values.
coupdat3 <- subset(beerdat, is.na(coupd.2)==F &
            is.na(madgdppc.3)==F & is.na(SFTGTPOP.3)==F & is.na(polscore.3)==F & is.na(polscore2.3)==F & is.na(couact5d.3)==F & is.na(postcw.2)==F & 
            is.na(madgdppcg.3)==F &
            is.na(madgdppcg)==F,
            select=c(code, year, coupd.2, madgdppc.3, SFTGTPOP.3, polscore.3, polscore2.3, couact5d.3, postcw.2, madgdppcg.3, madgdppcg)  )

# Run matching.
coup3.out <- matchit(coupd.2 ~ log(madgdppc.3) + polscore.3 + polscore2.3 + postcw.2 + couact5d.3,
         data = coupdat3,
         method = 'cem' )

coup3.z.out <- zelig(madgdppcg ~ coupd.2 + madgdppcg.3 + log(madgdppc.3) + polscore.3 + polscore2.3 + postcw.2 + couact5d.3,
              data = match.data(coup3.out),
              model = 'ls')

# Estimate effect.
coup3.x.out <- setx(coup3.z.out, coupd.2 = 0)
coup3.x1.out <- setx(coup3.z.out, coupd.2 = 1)
coup3.s.out <- sim(coup3.z.out, x = coup3.x.out, x1 = coup3.x1.out)

### Year t + 3

# Listwise deletion of missing values.
coupdat4 <- subset(beerdat, is.na(coupd.3)==F &
            is.na(madgdppc.4)==F & is.na(SFTGTPOP.4)==F & is.na(polscore.4)==F & is.na(polscore2.4)==F & is.na(couact5d.4)==F & is.na(postcw.4)==F & 
            is.na(madgdppcg.4)==F &
            is.na(madgdppcg)==F,
            select=c(code, year, coupd.3, madgdppc.4, SFTGTPOP.4, polscore.4, polscore2.4, couact5d.4, postcw.3, madgdppcg.4, madgdppcg)  )

# Run matching.
coup4.out <- matchit(coupd.3 ~ log(madgdppc.4) + polscore.4 + polscore2.4 + postcw.3 + couact5d.4,
         data = coupdat4,
         method = 'cem' )

coup4.z.out <- zelig(madgdppcg ~ coupd.3 + madgdppcg.4 + log(madgdppc.4) + polscore.4 + polscore2.4 + postcw.3 + couact5d.4,
              data = match.data(coup4.out),
              model = 'ls')

# Estimate effect.
coup4.x.out <- setx(coup4.z.out, coupd.3 = 0)
coup4.x1.out <- setx(coup4.z.out, coupd.3 = 1)
coup4.s.out <- sim(coup4.z.out, x = coup4.x.out, x1 = coup4.x1.out)

### Year t + 4

# Listwise deletion of missing values.
coupdat5 <- subset(beerdat, is.na(coupd.4)==F &
            is.na(madgdppc.5)==F & is.na(SFTGTPOP.5)==F & is.na(polscore.5)==F & is.na(polscore2.5)==F & is.na(couact5d.5)==F & is.na(postcw.4)==F & 
            is.na(madgdppcg.5)==F &
            is.na(madgdppcg)==F,
            select=c(code, year, coupd.4, madgdppc.5, SFTGTPOP.5, polscore.5, polscore2.5, couact5d.5, postcw.4, madgdppcg.5, madgdppcg)  )

# Run matching.
coup5.out <- matchit(coupd.4 ~ log(madgdppc.5) + polscore.5 + polscore2.5 + postcw.4 + couact5d.5,
         data = coupdat5,
         method = 'cem' )

coup5.z.out <- zelig(madgdppcg ~ coupd.4 + madgdppcg.5 + log(madgdppc.5) + polscore.5 + polscore2.5 + postcw.4 + couact5d.5,
              data = match.data(coup5.out),
              model = 'ls')

# Estimate effect.
coup5.x.out <- setx(coup5.z.out, coupd.4 = 0)
coup5.x1.out <- setx(coup5.z.out, coupd.4 = 1)
coup5.s.out <- sim(coup5.z.out, x = coup5.x.out, x1 = coup5.x1.out)

### Year t + 5

# Listwise deletion of missing values.
coupdat6 <- subset(beerdat, is.na(coupd.5)==F &
            is.na(madgdppc.6)==F & is.na(SFTGTPOP.6)==F & is.na(polscore.6)==F & is.na(polscore2.6)==F & is.na(couact5d.6)==F & is.na(postcw.5)==F & 
            is.na(madgdppcg.6)==F &
            is.na(madgdppcg)==F,
            select=c(code, year, coupd.5, madgdppc.6, SFTGTPOP.6, polscore.6, polscore2.6, couact5d.6, postcw.5, madgdppcg.6, madgdppcg)  )

# Run matching.
coup6.out <- matchit(coupd.5 ~ log(madgdppc.6) + polscore.6 + polscore2.6 + postcw.5 + couact5d.6,
         data = coupdat6,
         method = 'cem' )

coup6.z.out <- zelig(madgdppcg ~ coupd.5 + madgdppcg.6 + log(madgdppc.6) + polscore.6 + polscore2.6 + postcw.5 + couact5d.6,
              data = match.data(coup6.out),
              model = 'ls')

# Estimate effect.
coup6.x.out <- setx(coup6.z.out, coupd.5 = 0)
coup6.x1.out <- setx(coup6.z.out, coupd.5 = 1)
coup6.s.out <- sim(coup6.z.out, x = coup6.x.out, x1 = coup6.x1.out)

# Plots
plot(coup1.s.out, sub='Impact of Successful Coup on GDP Growth (t0)')
plot(coup2.s.out, sub='Impact of Successful Coup on GDP Growth (t+1)')
plot(coup3.s.out, sub='Impact of Successful Coup on GDP Growth (t+2)')
plot(coup4.s.out, sub='Impact of Successful Coup on GDP Growth (t+3)')
plot(coup5.s.out, sub='Impact of Successful Coup on GDP Growth (t+4)')
plot(coup6.s.out, sub='Impact of Successful Coup on GDP Growth (t+5)')

