# This script creates a function, seevee.logit(), that can be used to run iterated k-fold cross-validation on numerous
# logistic regression models with a common dependent variable/target in one shot. It produces a dataframe that contains
# averages of a few common measures of accuracy for models with binary dependent variables: Brier score, Brier skill score,
# and area under the ROC curve (AUC). Each model gets a single value for each accuracy statistic, representing the average
# of the accuracy statistics for that model across all j iterations of k-fold cross-validation.

# The function will flexibly handle three kinds of logistic regression models --- 1) generalized additive models (gams) with
# smoothing splines or loess, 2) multilevel models with at least one random effect, and 3) plain old logistic regression ---
# by inferring the model type from the user-given model formula. The function: uses gam(family=binomial) when the formula
# includes at least one term with " s(" or " lo(" in it; uses glmer(family=binomial) when the formula includes at least one
# term with " | " in it; and it uses glm(family=binomial) if neither of those conditions holds.

# The call to seevee.logit() takes:
#
# 1. A list of formula objects (formulalist) in the form formulalist = list(formula1, formula2...formulan). Naming the items
#    in that list will make it easier to read the resulting table, so formulalist = list(mod0 = formula0, mod1 = formula1...) 
#    is a good idea. The function assumes that every one of these formulas leads with the dependent variable/target in the
#    typical y ~ x1 + ... format, where y is the dependent variable/target.
#
# 2. A data frame (df) that contains all of the variables used in all of the formulas in formulalist.
# 
# 3. An integer (k) specifying the number of folds to use in each run of k-fold cross-validation.
#
# 4. An integer (iterations) specifying the number of times to repeat k-fold cross-validation.
#
# 5. An integer (userseed) to be used to set the seed for sampling in the splitting of the original dataframe for
#    cross-validation.
#
# The function depends on the following packages: caret, gam, lme4, verification
#
# This example sort of works:
#
# library(gam)
# data(kyphosis)
# kyphosis$Low <- ifelse(kyphosis$Number <=3, 1, 0)
# kyphosis$DV <- ifelse(kyphosis$Kyphosis=="present", 1, 0)
#
# formulalist = list(mod.lm = as.formula(DV ~ Age),
#                    mod.gam = as.formula(DV ~ s(Age)),
#                    mod.lme = as.formula(DV ~ Age + (1 | Low)))
#
# seevee.logit(formulalist, kyphosis, 5, 10, 20912)

seevee.logit <- function(formulalist, df, k, iterations, userseed) {

    require(caret)
    require(gam)
    require(lme4)
    require(verification)
    
    # Get partitions for iterated k-fold cross-validation

    set.seed(userseed)  # set seed to make process replicable
    dv <- all.vars(formulalist[[1]])[1] # recognize the dependent variable/target as the first item in the first formula
    cvfolds <- createMultiFolds(as.factor(df[,dv]), k = k, times = iterations) # create the sets for iterated k-fold cv; make sure dv is read as factor in next step so partitioning is stratified by level

    # This function calculates accuracy statistics for a single fold of a single iteration of iterated k-fold cv. It is
    # meant to be used in a call to lapply() on an integer sequence running from 1 to k * iterations.

    cvmachine <- function(x, formula, df, k, iterations, userseed) {

        train <- df[cvfolds[[x]],] # get the training set for the xth slice of that k * iterations stack
        test <- df[-cvfolds[[x]],] # get the associated test set, which is the complement of that training set

        # Infer model type from the model formula and apply the inferred type

        if (sum(grepl(" s\\(", formula)) > 0 | sum(grepl(" lo\\(", formula)) > 0) {

            modobj <- gam(formula, data=train, family=binomial, na.action=na.exclude)
            preds <- predict(modobj, newdata=test, type="response")

        } else if (sum(grepl(" \\| ", formula)) > 0) {

            modobj <- glmer(formula, family=binomial, data=train, na.action=na.exclude)
            preds <- predict(modobj, newdata=test, type="response", allow.new.levels=TRUE) # make predicted values for the test set

        } else {

            modobj <- glm(formula, data=train, family=binomial, na.action=na.exclude)
            preds <- predict(modobj, newdata=test, type="response") # make predicted values for the test set
        }

        obs <- test[,dv] # get the observed values from the test set
        bs <- brier(obs, preds, bins=FALSE) # compute brier score using 'verification' function brier()
        auc <- roc.area(obs, preds) # compute area under roc curve using 'verification' function roc.area()
        stats <- data.frame(brier.score=bs$bs, brier.skill.score=bs$ss, auc=auc$A) # make 1-row data frame from selected stats
        return(stats) # return that 1-row data frame
    }

    # This function takes the results of a lapplied call to cvmachine() and... 
    
    summit <- function(cvstatslist) {
        z1 <- do.call(rbind, cvstatslist) # collapses them into a table;
        z2 <- cbind(rep = rep(seq(iterations), each = k), fold = rep(seq(k), times = iterations), z1) # adds iteration & fold id columns;
        s <- aggregate(. ~ rep, data = z2[,-2], mean) # computes the averages of the accuracy statistics by iteration;
        ss <- colMeans(s[,2:ncol(s)]) # computes the averages of the accuracy statistics across all iterations;
        return(ss) # and spits that out as a one-row data frame
    }

    # Do all that crap in one shot and return the results
    
    results <- do.call(rbind, lapply(formulalist, function(ls) summit(lapply(seq(k * iterations),
        function(x) cvmachine(x, ls, df, k, iterations, userseed)))))
    return(results)
}
