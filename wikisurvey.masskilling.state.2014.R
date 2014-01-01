# Data are on Google Drive at:
# https://drive.google.com/file/d/0B5wyt4eDq98GeWVGWWNtWnc0Yzg/edit?usp=sharing

# Clear workspace
rm(list=ls(all=TRUE))

# Load 'rworldmap'
require(rworldmap)

# Load the data
wiki2014 <- read.csv("wikisurvey.global.state.masskilling.2014.csv")

# Calculate predicted probabilities assuming different global totals of onset events in 2014
for (i in 1:dim(wiki2014)[1]) wiki2014$prob1[i] <- (wiki2014$score[i]/sum(wiki2014$score))*1
for (i in 1:dim(wiki2014)[1]) wiki2014$prob2[i] <- (wiki2014$score[i]/sum(wiki2014$score))*2
for (i in 1:dim(wiki2014)[1]) wiki2014$prob3[i] <- (wiki2014$score[i]/sum(wiki2014$score))*3
for (i in 1:dim(wiki2014)[1]) wiki2014$prob4[i] <- (wiki2014$score[i]/sum(wiki2014$score))*4
for (i in 1:dim(wiki2014)[1]) wiki2014$prob5[i] <- (wiki2014$score[i]/sum(wiki2014$score))*5

# Make sure country names are characters, not factor
wiki2014$country <- as.character(wiki2014$country)

# Join the data to a map
wikimap <- joinCountryData2Map(wiki2014, nameJoinColumn = "country", joinCode = "NAME", verbose = TRUE)

# Correct mismatches
wiki2014$country[wiki2014$country=="Timor Leste"] <- "Timor-Leste"
wiki2014$country[wiki2014$country=="Bosnia Herzegovina"] <- "Bosnia and Herzegovina"

# Redo with mismatches corrected
wikimap <- joinCountryData2Map(wiki2014, nameJoinColumn = "country", joinCode = "NAME", verbose = TRUE)

# Map the scores
grayscale <- c(paste(rep("gray", times = max(wiki2014$score)-min(wiki2014$score)),
    as.character(seq(max(wiki2014$score),min(wiki2014$score),-1)), sep = ""))
png("wikisurvey.masskilling.state.2014.map.png", width=800, height=450, bg="white")
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
map.score <- mapCountryData(wikimap,
    nameColumnToPlot="score",
    addLegend = FALSE,
    numCats = max(wiki2014$score)-min(wiki2014$score), catMethod="fixedWidth", colourPalette = grayscale, borderCol = "white",
    mapTitle = "Relative Risk of State-Led Mass Killing Onset in 2014")
do.call(addMapLegend, c(map.score, legendWidth=0.5, legendMar = 2))
mtext("map made using rworldmap             ", line=-4, side=1, adj=1, cex=0.8)
dev.off()

# Dot plot of scores
require(Hmisc)
wiki2014 <- wiki2014[order(-wiki2014$score),]
wiki2014$ongoing <- ifelse(wiki2014$country=="Syria" |
                           wiki2014$country=="Sudan" |
                           wiki2014$country=="North Korea" |
                           wiki2014$country=="Myanmar" |
                           wiki2014$country=="Democratic Republic of Congo" |
                           wiki2014$country=="Egypt" |
                           wiki2014$country=="Nigeria" ,
                           1, 0)
condcol <- ifelse(wiki2014$ongoing==1, "firebrick3", "gray")
png(file = "wikisurvey.masskilling.state.2014.dotplot.png",
    width=14, height=40, unit = "cm", bg="white", res=150)
dotchart2(wiki2014$score, labels=wiki2014$country,
    lines=TRUE, lwd=0.05, lty=3, sort=FALSE, dotsize=1, col=condcol, pch=20, cex.labels=0.5 )
title(main=list("Relative Risk of State-Led Mass Killing Onset in 2014", cex=0.9))
dev.off()
