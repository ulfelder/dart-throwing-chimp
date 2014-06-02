# MONTHLY COUP MODELING EXERCISE WITH ACLED
# 2014-06-01

rm(list=ls(all=TRUE))

require(reshape)

memory.limit(size = 4000)

setwd("c:/users/jay/documents/coup.monthly/outdata/")

coups <- read.csv("coups.csv")
coups$sftgcode <- as.character(coups$sftgcode)
coups$yearmon <- as.character(coups$month)
coups$year <- coups$month <- NULL

polity <- read.csv("polity.csv")
polity$sftgcode <- as.character(polity$sftgcode)
polity$country <- NULL
polity$yearmon <- as.character(polity$yearmon)

acled <- read.csv("acled.csv")
acled$sftgcode <- as.character(acled$sftgcode)
acled$yearmon <- as.character(acled$yearmon)
acled$country <- acled$year <- acled$month <- NULL

im <- read.csv("imrate.csv")
im$sftgcode <- as.character(im$sftgcode)
im$yearmon <- as.character(im$month)
im$year <- im$country <- im$month <- NULL

meld <- merge(coups, polity, all = TRUE)
meld <- merge(meld, im, all = TRUE)
meld <- merge(meld, acled, all = TRUE)

africa <- subset(meld, is.na(civviol)==FALSE)

# MODELING
# Formulae
f.coup0 <- formula(coup.a.d ~ xxxcimrln + pitfcat2 + pitfcat3 + pitfcat4 + pitfcat5 + pitfcat6 + durln_1 + coups_24d)
f.coup3 <- formula(coup.a.d ~ xxxcimrln + pitfcat2 + pitfcat3 + pitfcat4 + pitfcat5 + pitfcat6 + durln_1 + coups_24d +
  civviol_3 + protest_3 + battle_3)
f.coup6 <- formula(coup.a.d ~ xxxcimrln + pitfcat2 + pitfcat3 + pitfcat4 + pitfcat5 + pitfcat6 + durln_1 + coups_24d +
  civviol_6 + protest_6 + battle_6)
f.coup12 <- formula(coup.a.d ~ xxxcimrln + pitfcat2 + pitfcat3 + pitfcat4 + pitfcat5 + pitfcat6 + durln_1 + coups_24d +
  civviol_12 + protest_12 + battle_12)
f.rf <- formula(as.factor(coup.a.d) ~ xxxcimrln + pitfcat + durln_1 + coups_24d + civviol_12 + protest_12 + battle_12)

#Cross-validation
require(caret)
valdat <- subset(africa, is.na(coup.a.d) == FALSE)
y <- valdat$coup.a.d
valdat$k <- createFolds(y, k = 5, list = FALSE)

predit <- function(x) {
  train <- subset(valdat, k != x)
  test <- subset(valdat, k == x)
  test$coup0.p <- predict(glm(f.coup0, family = binomial, data = train, na.action = na.exclude),
    newdata = test, type = "response")
  test$coup3.p <- predict(glm(f.coup3, family = binomial, data = train, na.action = na.exclude),
    newdata = test, type = "response")
  test$coup6.p <- predict(glm(f.coup6, family = binomial, data = train, na.action = na.exclude),
    newdata = test, type = "response")
  test$coup12.p <- predict(glm(f.coup12, family = binomial, data = train, na.action = na.exclude),
    newdata = test, type = "response")
  require(randomForest)
  test$rf.p <- predict(randomForest(f.rf, data = train, na.action="na.exclude", ntree = 1000, mtry = 3,
    cutoff=c(0.05,0.95)), newdata = test, type = "prob", na.action = "na.exclude")[,2]
  out <- subset(test, select = c(sftgcode, yearmon, coup.a.d, coup0.p, coup3.p, coup6.p, coup12.p, rf.p, k))
  return(out)
}

test1 <- predit(1)
test2 <- predit(2)
test3 <- predit(3)
test4 <- predit(4)
test5 <- predit(5)
out <- rbind(test1, test2, test3, test4, test5)

# Distribution of AUC scores by fold
fun.auc <- function(df, x) {
  require(verification)
  coup0 <- roc.area(df$coup.a.d[out$k==x], df$coup0.p[out$k==x])
  coup3 <- roc.area(df$coup.a.d[out$k==x], df$coup3.p[out$k==x])
  coup6 <- roc.area(df$coup.a.d[out$k==x], df$coup6.p[out$k==x])
  coup12 <- roc.area(df$coup.a.d[out$k==x], df$coup12.p[out$k==x])
  rf <- roc.area(df$coup.a.d[out$k==x], df$rf.p[out$k==x])
  all <- c(x, coup0$A, coup3$A, coup6$A, coup12$A, rf$A )
  names(all) <- c("fold", "coup0", "coup3", "coup6", "coup12", "rf")
  return(all)
}

auc1 <- fun.auc(out, 1)
auc2 <- fun.auc(out, 2)
auc3 <- fun.auc(out, 3)
auc4 <- fun.auc(out, 4)
auc5 <- fun.auc(out, 5)
auctab <- as.data.frame(rbind(auc1, auc2, auc3, auc4, auc5))

coup0.m <- mean(auctab$coup0)
coup3.m <- mean(auctab$coup3)
coup6.m <- mean(auctab$coup6)
coup12.m <- mean(auctab$coup12)
rf.m <- mean(auctab$rf)

# ROC curves by model
library(ROCR)
coup0.pred <- prediction(out$coup0.p, out$coup.a.d)
coup0.roc <- performance(coup0.pred, "tpr", "fpr")
coup0.auc <- performance(coup0.pred, measure = "auc")
coup3.pred <- prediction(out$coup3.p, out$coup.a.d)
coup3.roc <- performance(coup3.pred, "tpr", "fpr")
coup3.auc <- performance(coup3.pred, measure = "auc")
coup6.pred <- prediction(out$coup6.p, out$coup.a.d)
coup6.roc <- performance(coup6.pred, "tpr", "fpr")
coup6.auc <- performance(coup6.pred, measure = "auc")
coup12.pred <- prediction(out$coup12.p, out$coup.a.d)
coup12.roc <- performance(coup12.pred, "tpr", "fpr")
coup12.auc <- performance(coup12.pred, measure = "auc")

png(file = "c:/documents and settings/jay/my documents/coup.monthly/figs/val.roc.by.model.png",
     width=12, height=12, units='cm', bg='white', res=150)
plot(coup0.roc, col = "black", lwd=2, add = FALSE)
plot(coup3.roc, col = "blue", add = TRUE)
plot(coup6.roc, col = "red", add = TRUE)
plot(coup12.roc, col = "forestgreen", add = TRUE)
text(x=1,y=0.15,
     labels = paste("+ ACLED (12 mo)", substring(as.character(coup12.auc@y.values),1,5), sep=' = '),
     pos=2, cex=0.75, col = "forestgreen")
text(x=1,y=0.10,
     labels = paste("+  ACLED (6 mo)", substring(as.character(coup6.auc@y.values),1,5), sep=' = '),
     pos=2, cex=0.75, col = "red")
text(x=1,y=0.05,
     labels = paste("+ ACLED (3 mo)", substring(as.character(coup3.auc@y.values),1,5), sep=' = '),
     pos=2, cex=0.75, col = "blue")
text(x=1,y=0,
     labels = paste("Base model", substring(as.character(coup0.auc@y.values),1,5), sep=' = '),
     pos=2, cex=0.75, col = "black")
dev.off()
