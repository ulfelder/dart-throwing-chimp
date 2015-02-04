# This goes with "Turning Crowdsourced Preseason NFL Strength Ratings into Game-Level Forecasts" (August 11, 2014)
# https://dartthrowingchimp.wordpress.com/2014/08/11/turning-crowdsourced-preseason-nfl-strength-ratings-into-game-level-forecasts/

# NOTE: If you don't want to regenerate the simulations and just want to download and play with the ones I've
# already produced, jump to line 121 and pick up from there.

# Clear workspace
rm(list=ls(all=TRUE))

# Load some useful packages
library(arm)
library(boot)
library(mgcv)
library(lattice)
library(Hmisc)
library(plyr)
library(reshape)
library(gplots)

# Set the working directory to the folder where you've got the requisite inputs (see lines 32 and 67)
setwd("[filepath]")

#########################################
# Modeling of 2013 results
#########################################

# Load 2013 game results data
gd <- read.csv("http://www.repole.com/sun4cast/stats/nfl2013lines.csv")

# Compute net scores and outcomes
gd$score.raw <- gd$Home.Score - gd$Visitor.Score
gd$win.raw <- ifelse(gd$score.raw > 0, 1, 0)

# Load 2013 wiki survey results found at:
# https://drive.google.com/file/d/0B5wyt4eDq98GdVFyZ2xpaFFwVVE/edit?usp=sharing
wiki2013 <- read.csv("nfl2013scores.csv")

# Use merging to load wiki score pairs for each game
wiki2013$team <- as.character(wiki2013$team)
wiki2013$team[wiki2013$team=="Washington [redacted]"] <- "Washington Redskins"  # Sigh.
wiki2013$team[wiki2013$team=="St. Louis Rams"] <- "St Louis Rams"
wiki2013.home <- subset(wiki2013, select = c(team, preseason, team3))
names(wiki2013.home) <- c("Home.Team", "Wiki.Home", "Home.Team.3")
gd <- merge(gd, wiki2013.home, all.x = TRUE)
wiki2013.visitor <- subset(wiki2013, select = c(team, preseason, team3))
names(wiki2013.visitor) <- c("Visitor", "Wiki.Visitor", "Visitor.3")
gd <- merge(gd, wiki2013.visitor, all.x = TRUE)

# Calculate net wiki score by game
gd$Wiki.Diff <- gd$Wiki.Home - gd$Wiki.Visitor

# Simple linear model
mod0 <- lm(score.raw ~ Wiki.Diff, data = gd)

######################################
# 2014-2015 Forecasts
######################################

# Ingest .csv with 2014-2015 matchups copied and pasted from http://www.pro-football-reference.com/years/2014/
# with manual removal of most headers and correction of top header row
nfl2014all <- read.csv("nfl2014schedule.csv", header = TRUE)
nfl2014all$At <- nfl2014all$Time <- nfl2014all$Tickets <- NULL # Remove useless columns
nfl2014all$Visitor <- as.character(nfl2014all$Visitor)
nfl2014all$Home.Team <- as.character(nfl2014all$Home.Team)

# Load final preseason wiki survey scores, drop user-added idea, and subset to just team
# names & scores. (A user added a "San Francisco" idea, probably not understanding how the
# survey worked. This added idea was not approved, so it never appeared in the voting, but 
# the survey platform still includes it in the final set of ideas.) File called in line 65 found at:
# https://drive.google.com/file/d/0B5wyt4eDq98GWmxhVndiRGl0YjQ/edit?usp=sharing
wiki2014 <- read.csv("nfl.2014.wiki.scores.preseason.final.20140903.csv")
wiki2014 <- subset(wiki2014, User.Submitted==FALSE)
wiki2014 <- subset(wiki2014, select = c(Idea.Text, Score))

# Fix some labels for merging with schedule data.
wiki2014$Idea.Text <- as.character(wiki2014$Idea.Text)
wiki2014$Idea.Text[wiki2014$Idea.Text=="Washington [redacted]"] <- "Washington Redskins" # Sigh again.
wiki2014$Idea.Text[wiki2014$Idea.Text=="St. Louis Rams"] <- "St Louis Rams"

# Make identical copies with teams IDed as both home and visitor.
wiki2014.visitor <- wiki2014.home <- wiki2014
names(wiki2014.visitor) <- c("Visitor", "Wiki.Visitor")
names(wiki2014.home) <- c("Home.Team", "Wiki.Home")

# Merge those copies with the schedule data. This will put the right survey score
# in the right column for each game.
nfl2014all <- merge(nfl2014all, wiki2014.visitor, all.x = TRUE)
nfl2014all <- merge(nfl2014all, wiki2014.home, all.x = TRUE)

# Compute the home - visitor survey score differential used in forecast model
nfl2014all$Wiki.Diff <- nfl2014all$Wiki.Home - nfl2014all$Wiki.Visitor

# Generate distributions of score forecasts using the 'sim' command from the 'arm' package
# (Gelman & Hill, _Data Analysis Using Regression and Multilevel/Hierarchical Modeling_, pp. 146-147)
n.tilde <- dim(nfl2014all)[1]  # Number of games
X.tilde <- cbind(rep(1, n.tilde), nfl2014all$Wiki.Diff)  # Matrix with cols for intercept & coeff
n.sims <- 1000  # Set number of simulations (here, 1,000)
sim.2014 <- sim(mod0, n.sims)  # Get those simulated coefficients
y.tilde <- array(NA, c(n.sims, n.tilde))  # Create matrix for sim predictions 
# Generate sim predictions by multiplying matrices with sim coeffs and inputs
for (s in 1:n.sims) { y.tilde[s,] <- rnorm(n.tilde, X.tilde %*% sim.2014@coef[s,], sim.2014@sigma[s]) }

# Get probability of home win by counting sims with pred score > 0 & dividing by n of sims
p.home <- colSums(y.tilde > 0)/n.sims

# Transpose matrix of scores from sims
sim.2014.scores <- as.data.frame( t(y.tilde) )
simnames <- paste(rep("simscore", times = n.sims), seq(1:n.sims), sep = ".")
names(sim.2014.scores) <- simnames

# Put all of the 2014 season info in one data frame with games as rows
nfl.2014.sim <- as.data.frame(cbind(nfl2014all, p.home, sim.2014.scores))
nfl.2014.sim <- nfl.2014.sim[order(nfl.2014.sim$Week, nfl.2014.sim$Day),]

# Add means for net scores
nfl.2014.sim$mean <- round(rowSums(subset(nfl.2014.sim, select = c(simnames))/ n.sims), 1)

# Write that out for posterity
write.csv(nfl.2014.sim, file = "nfl.2014.sims.20140904.csv", row.names = FALSE)

# Or download it here:
# https://drive.google.com/file/d/0B5wyt4eDq98GUGhsd2N5ckh5Q2c/edit?usp=sharing

#############################################
# Histograms of game-level simulated scores
#############################################

# Function to plot histogram of simulated scores for a single game
hist.score <- function(home, visitor) {
  z <- subset(nfl.2014.sim, Home.Team == home & Visitor == visitor, select = c(10:(n.sims + 9)))
  z <- t(z)
  names(z) <- NULL
  m <- round(rowSums(subset(nfl.2014.sim, Home.Team == home & Visitor == visitor,
    select = c(10:(n.sims + 9))))/n.sims, 1)
  png(paste(home, visitor, "png", sep = "."),
    width=4, height=2.5, units='in', bg='white', res=150)
  par(cex.axis = 0.5)
  par(cex.lab = 0.6)
  par(cex.main = 0.7)
  par(mar = c(3,3,3,1))
  par(xpd = NA)
  hist(z, breaks = seq(-60, 60, 1), freq = FALSE,
    axes = FALSE, xlab = "", ylab = "",
    main = paste(visitor, home, sep = " at "),
    col = "gray50", border = "gray50")
  axis(2, las = 2, tick = FALSE, line = -0.75)
  axis(1, at = seq(-60,60,10), tick = FALSE, line = -0.75)
  mtext("predicted net score (home - visitor)", side = 1, line = 1.5, cex = 0.6)
  mtext("density", side = 2, line = 1.5, cex = 0.6)
  text(x = m, y = -0.002, paste0("mean = ", m), col = "red", cex = 0.5)
  dev.off()
}

# Apply function to BAL-CIN, GNB-SEA
hist.score("Baltimore Ravens", "Cincinnati Bengals")
hist.score("Seattle Seahawks", "Green Bay Packers")

# Eyeball win probabilities for a couple of games
nfl.2014.sim$p.home[nfl.2014.sim$Home.Team == "Baltimore Ravens" & nfl.2014.sim$Visitor == "Cincinnati Bengals"]
nfl.2014.sim$p.home[nfl.2014.sim$Vistor == "Baltimore Ravens" & nfl.2014.sim$Home.Team =="Cincinnati Bengals"]

#######################################
# Expected wins per team
#######################################

# Function to get expected number of wins per team
winsum <- function(teamname) {
  home <- subset(nfl.2014.sim, Home.Team == teamname, select = p.home)
  away <- subset(nfl.2014.sim, Visitor == teamname, select = p.home)
  ewins <- sum(home) + 8 - sum(away)
  return(ewins) 
}

# Apply that to the 2014 simulation results
ewins.2014 <- sapply(unique(nfl.2014.sim$Visitor), winsum)

# Function to get count of predicted wins per team based on p > 0.5
wintot <- function(teamname) {
  z <- subset(nfl.2014.sim, Home.Team == teamname | Visitor == teamname,
    select = c(Home.Team, Visitor, p.home))
  w <- ifelse((z$Home.Team == teamname & z$p.home > 0.5) | (z$Visitor == teamname & z$p.home < 0.5) , 1, 0)
  wt <- sum(w)
  return(wt) 
}

twins.2014 <- sapply(unique(nfl.2014.sim$Visitor), wintot)

# Reshape those sums and merge with wiki scores to compare rank ordering
ewins.2014 <- melt(ewins.2014)
ewins.2014$team <- row.names(ewins.2014)
ewins.2014 <- ewins.2014[order(-ewins.2014$value),]
row.names(ewins.2014) <- NULL
names(ewins.2014) <- c("WinSum", "Team")
twins.2014 <- melt(twins.2014)
twins.2014$team <- row.names(twins.2014)
row.names(twins.2014) <- NULL
names(twins.2014) <- c("WinTot", "Team")
wiki2014 <- wiki2014[order(-wiki2014$Score),]
wiki2014$Rank <- as.numeric(row.names(wiki2014))
ewins.2014.2 <- merge(ewins.2014, wiki2014, by.x = "Team", by.y = "Idea.Text", all.x = TRUE)
ewins.2014.2 <- merge(ewins.2014.2, twins.2014)
ewins.2014.2 <- ewins.2014.2[order(-ewins.2014.2$WinSum),]

row.names(ewins.2014.2) <- NULL
ewins.2014.2 <- subset(ewins.2014.2, select = c(1,4,3,2,5))
names(ewins.2014.2) <- c("Team", "WikiRank", "WikiScore", "WinSum", "WinCount")
ewins.2014.2$WikiScore <- round(ewins.2014.2$WikiScore, 2)
ewins.2014.2$WinSum <- round(ewins.2014.2$WinSum, 2)
ewins.2014.2$Team[ewins.2014.2$Team=="Washington Redskins"] <- "Washington [redacted]"  # Take that, Dan Snyder.

# Use 'textplot' from 'gplots' to print table to PNG file
library(gplots)
png("ewins.2014.png", width=6, height=6, units='in', bg='white', res=150)
textplot(ewins.2014.2, show.rownames = FALSE)
dev.off()
