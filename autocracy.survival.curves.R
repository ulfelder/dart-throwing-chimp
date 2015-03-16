# Clear workspace
rm(list=ls(all=TRUE))

# Load required packages and functions
library(foreign)
library(survival)
library(RColorBrewer)

# Download and ingest the Geddes Wright Frantz data from the web
temp <- tempfile()
download.file("http://sites.psu.edu/dictators/wp-content/uploads/sites/12570/2014/07/GWF-Autocratic-Regimes-1.2.zip", temp)
gwf <- read.delim(unz(temp, "GWF Autocratic Regimes 1.2/GWF_AllPoliticalRegimes.txt"), stringsAsFactors = FALSE)
unlink(temp)

# Simplify variable names
names(gwf) <- c(names(gwf)[1:2], gsub("gwf_", "", x = names(gwf)[3:length(names(gwf))], fixed = TRUE) )

# These are interval data, so we need to create an event marker that differentiates between intervals and
# left- or right-censoring.
gwf$event <- gwf$fail
gwf$event <- with(gwf, ifelse(fail == 0 & year == 1946, 2, event)) # Left-censored
gwf$event <- with(gwf, ifelse(fail == 0 & year == 2010, 3, event)) # Right-censored

# Now estimate Kaplan-Meier survival curves by authoritarian type
km.by.type <- survfit(Surv(time = duration - 1, time2 = duration, event = event, type = "interval") ~ regimetype,
     data = gwf,
     subset=(is.na(regimetype) == FALSE), # Restrict to autocracies
     id = cowcode,
     type = "kaplan-meier")

# Extract clean version of types for legend labels
types <- gsub("regimetype=", "", names(km.by.type$strata), fixed = TRUE)

# Get a pallette with 10 colors for the 10 types
pal <- brewer.pal(10, "Paired")

# Plot that bowl of spaghetti.
png("autocracies.survival.curves.png", res = 300, width = 6, height = 5, units = "in", bg = "white")
par(mai=c(0.75,1,0.25,0.25))
plot(km.by.type, xmax = 80, col = pal, mark.time = FALSE, axes = FALSE)
axis(1, at = seq(0,80,20), tick = FALSE, pos = 0.025)
axis(2, at = seq(0,1,0.5), labels = c("0", "0.5", "1"), tick = FALSE, las = 2, pos = 1)
segments(0,1,80,1, "gray25", lty = 1)
segments(0,0.5,80,0.5, "gray25", lty = 3)
segments(0,0,80,0, "gray25", lty = 1)
legend(50, 0.55, types, col = pal, lty = 1, cex = 0.8) # Chose position by trial and error
mtext("Years", 1, line = 1.5, cex = 1)
mtext("Survival probability", 2, line = 3, cex = 1)
dev.off()
