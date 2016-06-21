# a function to compute the probability of at least one event occurring in some period of time, given
# a vector representing a sequence of probabilities that (at least one) event will occur in each of
# a series of time steps of equal width.
#
# For example, if the hourly probabilities of rain for some 3-hour window are 88%, 76%, and %50, we
# could use:
#
#     rain <- c(0.88, 0.76, 0.50)
#
#     f.atleastone(rain)
#
# > f.atleastone(rain)
# [1] 0.9856

f.atleastone <- function(vector) {

    p.conditional <- vector()

    for (i in 1:length(vector)) {

        if (i == 1) {

            p.conditional[i] <- vector[i]

        } else {

            p.conditional[i] <- vector[i] * (1 - sum(p.conditional[1:i - 1]))

        }

    }

    sigma <- sum(p.conditional)

    return(sigma)

}

