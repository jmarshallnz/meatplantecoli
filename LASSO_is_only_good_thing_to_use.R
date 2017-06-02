#' Script for fitting models

# idea for weighting priors:

# The priors are on the coefficients of a logistic regression, so presumably they should be centered on either +a, 0 or 0a for each expert
# Or alternatively, we could use assymetric laplace distributions? They seem the most obvious thing to use - you then adjust the rates/scale
# so that it balances with a global rate
#
# I don't think this is doable in STAN though by the looks, but maybe the likelihood is OK??
#
# This also may not be what MPI requires...
# I should look into that...
#
# OK, trying with glmnet which allows to vary lambda for LASSO for each parameter.
# We'll weight effect such that 'unknown' has lambda=1, no effect will have larger lambda, and 'some effect' smaller lambda.
# for multiple reviewers how to best combine these? e.g. we may have some unknown, some 'no effect' and some 'some effect'.
# I guess we need to average them out?
#
# so that 5 'some effect' would have a small lambda +1,0,-1. So we combine using a sum. Only question is if we should
# scale the sum in some way, e.g. a sqrt transformation? Probably doesn't make much difference: The magnitude will be
# more important probably.

library(dplyr)
library(glmnet)

#' Read in the data
plants = read.csv("data/final_data.csv")
priors = read.csv("data/final_prior_data.csv")

# Days is missing for plant 9
plants <- plants[plants$Plant != 'Plant9',]

# ignore variables iwth missing data for now
# ignore variables which have no variance
missing <- which(apply(plants, 2, function(x) { sum(is.na(x)) }) > 0)
no_variance <- which(apply(plants, 2, function(x) { length(unique(x))}) < 2)

plants <- plants[,-c(missing, no_variance)]

# extend the data frame for the Days variable
start <- c(0, cumsum(plants$Days)) + 1
start <- start[-length(start)]
end   <- cumsum(plants$Days)

newplant <- plants[1,]
for (i in 1:nrow(plants)) {
  newplant[start[i]:end[i], ] <- plants[i,]
  newplant$STEC[start[i]:end[i]] <- 0
  newplant$STEC[start[i] + 1:plants$STEC[i]-1] <- 1
}
newplant$STEC <- factor(newplant$STEC)

outcome <- newplant$STEC
matvars <- newplant %>% select(-Kill, -Days, -STEC, -Plant)

priors <- priors[match(names(matvars), priors$Variable),]


lm <- glmnet(modelmat, outcome, family="binomial", alpha=0.9, lambda.min.ratio = 0.000001, nlambda = 1000)
plot(lm, xvar='lambda', label=TRUE)
plot(lm, xvar='dev', label=TRUE)

set.seed(3)

# iterate over different options for alpha
alpha <- c(0,0.5,1)
penalty <- seq(0,2,by=0.2)
regcoef <- array(NA, dim = c(ncol(modelmat)+1, length(alpha), length(penalty)))
dimnames(regcoef) <- list(c("(Intercept)",colnames(modelmat)), as.character(alpha), as.character(penalty))
for (a in seq_along(alpha)) {
  lambda <- matrix(NA, 100, length(penalty))
  for (p in seq_along(penalty)) {
    # compute penalty
    cat("Fitting model for penalty", p, "alpha", a, "\n")
    penalty.factor = 2^((priors$NoEffect - priors$Effect)/5 * penalty[p])
    penalty.factor <- penalty.factor[attr(modelmat, 'assign')]
    # CV fit model, and pull out coefficients at lambda min
    for (i in 1:100) {
      cat("i=",i,"\n")
      cvfit <- cv.glmnet(modelmat, outcome, family="binomial", type.measure='auc', nfolds=80, alpha=alpha[a], penalty.factor=penalty.factor)
      lambda[i,p] <- cvfit$lambda.min
    }
#    cf <- as.matrix(coef(cvfit, s='lambda.min'))
#    regcoef[rownames(cf),a,p] <- cf[,1]
  }
}

plot(cvfit)

cvfit <- cv.glmnet(modelmat, outcome, family="binomial", type.measure='auc', nfolds=200, alpha=0.5, penalty.factor=penalty.factor)
plot(cvfit)

# how lambda works:

# the difference seems to be log10(lambda.min.ratio)/(nlambda-1)

# hmm, do we want constant lambda for the fit?
LAMBDA <- 0.03

alpha <- seq(0,1,by=0.05)
penalty <- seq(0,2,by=0.1)
regcoef <- array(NA, dim = c(ncol(modelmat)+1, length(alpha), length(penalty)))
dimnames(regcoef) <- list(c("(Intercept)",colnames(modelmat)), as.character(alpha), as.character(penalty))
for (a in seq_along(alpha)) {
#  lambda <- matrix(NA, 100, length(penalty))
  for (p in seq_along(penalty)) {
    # compute penalty
    cat("Fitting model for penalty", p, "alpha", a, "\n")
    penalty.factor = 2^((priors$NoEffect - priors$Effect)/5 * penalty[p])
    penalty.factor <- penalty.factor[attr(modelmat, 'assign')]
    # CV fit model, and pull out coefficients at lambda min
#    for (i in 1:100) {
#      cat("i=",i,"\n")
#      cvfit <- cv.glmnet(modelmat, outcome, family="binomial", type.measure='auc', nfolds=80, alpha=alpha[a], penalty.factor=penalty.factor)
#      lambda[i,p] <- cvfit$lambda.min
#    }
    g <- glmnet(modelmat, outcome, family="binomial", alpha=alpha[a], penalty.factor=penalty.factor)
    cf <- as.matrix(coef(g, s=LAMBDA, exact=TRUE, x=modelmat, y=outcome))
    regcoef[rownames(cf),a,p] <- cf[,1]
  }
}

# OK, that then tells us something about the important covariates at least

# I guess ideally we'd iterate that over various lambda, then we could show which ones are most important and when they become so.

# So you could have plots of top XX variables by lambda/alpha/prior