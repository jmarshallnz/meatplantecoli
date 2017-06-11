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

lambda <- 10^(-seq(1,4, by=0.05))
alpha <- seq(0,1,by=0.1)
penalty <- seq(0,2,by=0.2)
regcoef <- array(NA, dim = c(ncol(modelmat)+1, length(lambda), length(alpha), length(penalty)))
dimnames(regcoef) <- list(c("(Intercept)",colnames(modelmat)), as.character(lambda), as.character(alpha), as.character(penalty))
for (l in seq_along(lambda)) {
  for (a in seq_along(alpha)) {
    #  lambda <- matrix(NA, 100, length(penalty))
    for (p in seq_along(penalty)) {
      # compute penalty
      cat("Fitting model for penalty", p, "alpha", a, "lambda", l, "\n")
      penalty.factor = 2^((priors$NoEffect - priors$Effect)/5 * penalty[p])
      penalty.factor <- penalty.factor[attr(modelmat, 'assign')]
      # CV fit model, and pull out coefficients at lambda min
      #    for (i in 1:100) {
      #      cat("i=",i,"\n")
      #      cvfit <- cv.glmnet(modelmat, outcome, family="binomial", type.measure='auc', nfolds=80, alpha=alpha[a], penalty.factor=penalty.factor)
      #      lambda[i,p] <- cvfit$lambda.min
      #    }
      g <- glmnet(modelmat, outcome, family="binomial", alpha=alpha[a], lambda = lambda, penalty.factor=penalty.factor)
      cf <- as.matrix(coef(g, s=lambda[l], exact=TRUE, x=modelmat, y=outcome))
      regcoef[rownames(cf),l,a,p] <- cf[,1]
    }
  }
}

# compress the array down to something smaller
all <- list()
k <- 1
for (i in 1:dim(regcoef)[1]) {
  for (j in 1:dim(regcoef)[2]) {
    cat("Up to k=", k, "of", prod(dim(regcoef)[1:2]), "\n")
    all[[k]] <- data.frame(Coefficient=dimnames(regcoef)[[1]][i], lambda=dimnames(regcoef)[[2]][j], alpha=dimnames(regcoef)[[3]], regcoef[i,j,,], check.names = FALSE)
    k <- k + 1
  }
}

a <- do.call(rbind, all)

library(tidyr)
b <- a %>% gather('penalty', 'value', `0`:`2`)
b$lambda <- as.numeric(as.character(b$lambda))
b$alpha <- as.numeric(as.character(b$alpha))
write.csv(b, "temp/lasso.csv", row.names=FALSE)









############ FINDING BEST MODEL FIT ################

# grab the foldid
set.seed(2)
lambda <- 10^(-seq(1,4, by=0.1))
cvfit <- cv.glmnet(modelmat, outcome, family="binomial", type.measure='auc', lambda=lambda, nfolds=50, alpha=1,keep=TRUE)
foldid <- cvfit$foldid

# now do the fitting - coarser than above as it takes ages
foldid <- as.numeric(newplant$Plant)
reps <- 100
alpha <- seq(0,1,by=0.2)
penalty <- seq(0,2,by=0.4)
cvm <- array(NA, dim = c(reps,length(lambda), length(alpha), length(penalty)))
dimnames(cvm) <- list(1:reps, as.character(lambda), as.character(alpha), as.character(penalty))
for (a in seq_along(alpha)) {
  #  lambda <- matrix(NA, 100, length(penalty))
  for (p in seq_along(penalty)) {
    # compute penalty
    cat("Fitting model for penalty", p, "alpha", a, "\n")
    penalty.factor = 2^((priors$NoEffect - priors$Effect)/5 * penalty[p])
    penalty.factor <- penalty.factor[attr(modelmat, 'assign')]
    # CV fit model, and pull out coefficients at lambda min
    cvm_ap <- matrix(0, reps, length(lambda))
    for (i in 1:reps) {
      cat("Rep", i, "of", reps, "\n")
      cvfit <- cv.glmnet(modelmat, outcome, family="binomial", type.measure='auc', lambda=lambda, alpha=alpha[a], penalty.factor=penalty.factor, foldid=foldid)
      cvm[i,,a,p] <- cvfit$cvm
    }
  }
}

# compress the array down to something smaller
all <- list()
k <- 1
for (i in 1:dim(cvm)[1]) {
  for (j in 1:dim(cvm)[2]) {
    cat("Up to k=", k, "of", prod(dim(cvm)[1:2]), "\n")
    all[[k]] <- data.frame(Rep=dimnames(cvm)[[1]][i], lambda=dimnames(cvm)[[2]][j], alpha=dimnames(cvm)[[3]], cvm[i,j,,], check.names = FALSE)
    k <- k + 1
  }
}

a <- do.call(rbind, all)
library(tidyr)
b <- a %>% gather('penalty', 'value', `0`:`2`)
b$lambda <- as.numeric(as.character(b$lambda))
b$alpha <- as.numeric(as.character(b$alpha))
b$penalty <- as.numeric(as.character(b$penalty))
b$Rep <- as.numeric(as.character(b$Rep))

s <- b %>% group_by(lambda, alpha, penalty) %>% summarize(mvalue = median(value),
                                                          lvalue = quantile(value,0.2),
                                                          uvalue = quantile(value,0.8)) %>%
  ungroup()

write.csv(s, "temp/lasso_fit_LOO.csv", row.names=FALSE)
# compute summary dataframe

ggplot(s) + geom_ribbon(aes(x=log10(lambda), ymin=lvalue, ymax=uvalue), alpha=0.5, fill='steelblue') + 
  geom_line(aes(x=log10(lambda), y=mvalue)) + facet_grid(alpha ~ penalty) +
  theme_bw() +
  ylab("Predictive performance (Area under curve)") +
  xlab("LASSO/Ridge penalty")

# find the best
s[which.max(s$mvalue),]
s[which.max(s$lvalue),]
s[which.max(s$uvalue),]

s %>% top_n(20, mvalue) %>% arrange(mvalue)


# seems to be around alpha = 0.7..1.0, lambda = 0.03..0.05, penalty=2
# now do the fitting - coarser than above as it takes ages
set.seed(2)
lambda <- 10^(-seq(1,2, by=0.025))
cvfit <- cv.glmnet(modelmat, outcome, family="binomial", type.measure='auc', lambda=lambda, nfolds=50, alpha=1,keep=TRUE)
foldid <- cvfit$foldid

reps <- 100
alpha <- seq(0.4,0.7,by=0.05) #10
penalty <- seq(0.9,1.1,by=0.05)  #5
cvm <- array(NA, dim = c(reps,length(lambda), length(alpha), length(penalty)))
dimnames(cvm) <- list(1:reps, as.character(lambda), as.character(alpha), as.character(penalty))
for (a in seq_along(alpha)) {
  #  lambda <- matrix(NA, 100, length(penalty))
  for (p in seq_along(penalty)) {
    # compute penalty
    cat("Fitting model for penalty", p, "alpha", a, "\n")
    penalty.factor = 2^((priors$NoEffect - priors$Effect)/5 * penalty[p])
    penalty.factor <- penalty.factor[attr(modelmat, 'assign')]
    # CV fit model, and pull out coefficients at lambda min
    cvm_ap <- matrix(0, reps, length(lambda))
    for (i in 1:reps) {
      cat("Rep", i, "of", reps, "\n")
      cvfit <- cv.glmnet(modelmat, outcome, family="binomial", type.measure='auc', nfolds=50, lambda=lambda, alpha=alpha[a], penalty.factor=penalty.factor, foldid=foldid)
      cvm[i,,a,p] <- cvfit$cvm
    }
  }
}

# compress the array down to something smaller
all <- list()
k <- 1
for (i in 1:dim(cvm)[1]) {
  for (j in 1:dim(cvm)[2]) {
    cat("Up to k=", k, "of", prod(dim(cvm)[1:2]), "\n")
    all[[k]] <- data.frame(Rep=dimnames(cvm)[[1]][i], lambda=dimnames(cvm)[[2]][j], alpha=dimnames(cvm)[[3]], cvm[i,j,,], check.names = FALSE)
    k <- k + 1
  }
}

a <- do.call(rbind, all)
library(tidyr)
b <- a %>% gather('penalty', 'value', `0.9`:`1.1`)
b$lambda <- as.numeric(as.character(b$lambda))
b$alpha <- as.numeric(as.character(b$alpha))
b$penalty <- as.numeric(as.character(b$penalty))
b$Rep <- as.numeric(as.character(b$Rep))

s <- b %>% group_by(lambda, alpha, penalty) %>% summarize(mvalue = median(value),
                                                          lvalue = quantile(value,0.2),
                                                          uvalue = quantile(value,0.8)) %>%
  ungroup()

write.csv(s, "temp/lasso_fit_zoom.csv", row.names=FALSE)
s <- read.csv("temp/lasso_fit_zoom.csv")
best_fit <- s %>% top_n(1, mvalue) %>% arrange(mvalue)
ggplot(s) + geom_ribbon(aes(x=log10(lambda), ymin=lvalue, ymax=uvalue), alpha=0.5, fill='steelblue') + 
  geom_line(aes(x=log10(lambda), y=mvalue)) + facet_grid(alpha ~ penalty) +
  theme_bw() +
  ylab("Predictive performance (Area under curve)") +
  xlab("LASSO/Ridge penalty")

# try the 'best' model
penalty.factor = 2^((priors$NoEffect - priors$Effect)/5 * best_fit$penalty)
penalty.factor <- penalty.factor[attr(modelmat, 'assign')]
mod <- glmnet(modelmat, outcome, family="binomial", lambda=best_fit$lambda, alpha=best_fit$alpha, penalty.factor=penalty.factor)
co <- as.matrix(coef(mod))
pred <- predict(mod, newx=modelmat, type='response')

# try the other 'best' model
s <- read.csv("temp/lasso_fit_zoomed.csv")
best_fit2 <- s %>% top_n(1, mvalue) %>% arrange(mvalue)
penalty.factor = 2^((priors$NoEffect - priors$Effect)/5 * best_fit2$penalty)
penalty.factor <- penalty.factor[attr(modelmat, 'assign')]
mod <- glmnet(modelmat, outcome, family="binomial", lambda=best_fit2$lambda, alpha=best_fit2$alpha, penalty.factor=penalty.factor)
co2 <- as.matrix(coef(mod))
co <- data.frame(VarName=rownames(co), Coefficient=co[,1], Coef2=co2[,1])
write.csv(co, "temp/final_coef.csv", row.names=FALSE)
pred2 <- predict(mod, newx=modelmat, type='response')
pred <- data.frame(Plant = newplant$Plant, STEC=as.numeric(as.character(outcome)), Prediction=as.numeric(pred), Prediction2=as.numeric(pred2))

# try prediction
d <- pred %>% group_by(Plant) %>% summarize(Observed=mean(STEC), Optimal=unique(Prediction), Alternate=unique(Prediction2)) %>% gather('Model', 'Predicted', Optimal:Alternate)
write.csv(d, "temp/final_prediction.csv", row.names=FALSE)

plot(Predicted ~ Observed, data=d, xlim=c(0,0.35), ylim=c(0,0.35))
abline(0,1)

# now fit the model with these parameters
penalty.factor = 2^((priors$NoEffect - priors$Effect)/5 * 2)
penalty.factor <- penalty.factor[attr(modelmat, 'assign')]
mod <- glmnet(modelmat, outcome, family="binomial", alpha=0.925, penalty.factor=penalty.factor)
c <- as.matrix(coef(mod, s=0.03162278, exact=TRUE))

pred <- predict(mod, newx=modelmat, s=0.03162278, exact=TRUE, type='response')
pred <- data.frame(Plant = newplant$Plant, STEC=as.numeric(as.character(outcome)), Prediction=as.numeric(pred))
d <- pred %>% group_by(Plant) %>% summarize(Observed=mean(STEC), Predicted=unique(Prediction))

plot(Predicted ~ Observed, data=d, xlim=c(0,0.35), ylim=c(0,0.35))
abline(0,1)

# bootstrap some confidence intervals...
covars <- which(c != 0)[-1]
reps <- 1000
coef <- matrix(NA, reps, length(covars)+1) #intercept
i <- 1
while (i <= reps) {
  # TODO: Sample with replacement within plants
  cat("Iteration", i, "of", reps, "\n")
  s <- sample(1:nrow(modelmat), nrow(modelmat), replace=TRUE)
  # check we have enough in each class
  mod <- glmnet(modelmat[s,covars], outcome[s], family="binomial", alpha=0.925, penalty.factor=penalty.factor[covars])
  co <- as.matrix(coef(mod))
  coSums <- colSums(co != 0)
  wch <- which.max(coSums)
  if (coSums[wch] == ncol(coef)) {
    coef[i,] <- as.numeric(co[,wch])
    i <- i + 1
  }
}

apply(coef, 2, function(x) { sum(x != 0) })

mod <- glmnet(modelmat[,covars], outcome, family="binomial", alpha=0.925, penalty.factor=penalty.factor[covars])
co <- as.matrix(coef(mod))

# hmm, build model containing only these covariates??

# This works really badly, as it's choosing covariates that are not really that useful???
penalty.factor = 2^((priors$NoEffect - priors$Effect)/5 * 1)
penalty.factor <- penalty.factor[attr(modelmat, 'assign')]
mod <- glmnet(modelmat, outcome, family="binomial", alpha=0.5, penalty.factor=penalty.factor)
c <- as.matrix(coef(mod, s=10^(-1.6), exact=TRUE))
which(c[,1] != 0)

covars <- which(c != 0)[-1]
reps <- 1000
coef <- matrix(NA, reps, length(covars)+1) #intercept
i <- 1

plants <- as.numeric(newplant$Plant)
mp <- max(plants)

while (i <= reps) {
  # TODO: Sample with replacement within plants
  cat("Iteration", i, "of", reps, "\n")
  w <- sample(1:mp, mp, replace=TRUE)
  s <- NULL
  for (j in seq_along(w)) {
    s <- c(s, which(plants == w[j]))
  }
  # check we have enough in each class
  mod <- glmnet(modelmat[s,covars], outcome[s], family="binomial", lambda=10^(-1.6), alpha=0.925, penalty.factor=penalty.factor[covars])
  co <- as.matrix(coef(mod))
#  coSums <- colSums(co != 0)
#  wch <- which.max(coSums)
#  if (coSums[wch] == ncol(coef)) {
    coef[i,] <- as.numeric(co[,1])
    i <- i + 1
#  }
}

apply(coef, 2, function(x) { sum(x != 0) })
apply(coef, 2, quantile, c(0.025, 0.975))
apply(coef, 2, sd)

apply(coef, 2, mean)

# Hmm, not really sure what to do: there are essentially arbitrarily large no of models that
# give the same fit to the data. It doesn't make much sense to try and limit it, nor to try
# and get 