library(lubridate)
library(dplyr)
library(tidyr)

# counted by hand, then typed those counts in here
pre <-    c(0,0,0,2,1,4, 0,2, 0,0, 0,0,2,0)
grade <-  c(0,0,0,0,2,19,0,11,2,12,1,1,2,1)
middle <- c(0,0,0,0,0,4, 0,1, 0,9, 2,3,0,0)
high <-   c(0,0,0,0,0,2, 0,1, 0,0, 1,1,0,0)

Raw <- data.frame(time = as.POSIXct("2016-10-31 17:30 UTC") + minutes(seq(0, 13 * 15, by = 15)),
                 pre = pre,
                 grade = grade,
                 middle = middle,
                 high = high)

Long <- gather(Raw, key = age, value = value, pre:high) %>%
  mutate(age = factor(age, levels = c("pre", "grade", "middle", "high")))

time.labels <- c("5:30","5:45","6:00","6:15","6:30","6:45","7:00"
                 "7:15","7:30","7:45","8:00","8:15","8:30","8:45")

plotit <- function(i) {

  DF <- filter(Long, age == levels(age)[i])

  with(DF, plot(x = seq_along(time), y = value,
                type = "h", lwd = 2, col = ifelse(value == 0, "white", "orange"),
                axes = FALSE, xlab = "", ylab = "", ylim = c(0,20)))

  mtext(levels(Long$age)[i], side=2, line=2, las=2, cex=0.5)

  axis(1, at = seq(1, 13, 2), labels = time.labels[seq(1, 13, 2)], tick = FALSE, pos = 4)

  axis(2, at = seq(0,20,10), tick = FALSE, las = 2)

  abline(h = seq(0,20,10), lwd = 1/2, col = "gray80")

}

png("halloween.2016.png", width = 3, height = 4, unit = "in", res = 150)
par(mai = c(0.25, 0.75, 0.25, 0.25), cex.axis = 1/2, mfrow = c(4,1))
for(i in seq_along(levels(Long$age))) { plotit(i) }
dev.off()
