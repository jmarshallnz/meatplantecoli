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

#' Read in the data
plants = read.csv("data/final_data.csv")
priors = read.csv("data/final_prior_data.csv")

# Days is missing for plant 9
plants <- plants[plants$Plant != 'Plant9',]

# ignore variables iwth missing data for now
missing <- which(apply(plants, 2, function(x) { sum(is.na(x)) }) > 0)
plants <- plants[,-missing]

# ignore variables which have no variance
no_variance <- which(apply(plants, 2, function(x) { length(unique(x))}) < 2)
plants <- plants[,-no_variance]

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

penalty.factor = 2^((priors$NoEffect - priors$Effect)/5)

library(glmnet)

modelmat <- model.matrix( ~.-1, matvars)
# predictors then need to be weighted accordingly...
penalty.factor <- penalty.factor[attr(modelmat, 'assign')]

lm <- glmnet(modelmat, outcome, family="binomial", alpha=0.9, lambda.min.ratio = 0.000001, nlambda = 1000)
plot(lm, xvar='lambda', label=TRUE)
plot(lm, xvar='dev', label=TRUE)

cvfit <- cv.glmnet(modelmat, outcome, family="binomial", type.measure='auc', nfolds=20, alpha=0.9, penalty.factor=penalty.factor)
plot(cvfit)

cvfit <- cv.glmnet(modelmat, outcome, family="binomial", type.measure='auc', nfolds=20, alpha=0.5, penalty.factor=penalty.factor)
plot(cvfit)

cf <- coef(cvfit, s='lambda.min')
r <- rownames(cf)
r[as.logical(cf != 0)]

# now try a forest
library(randomForest)

plants.rf = randomForest(STEC ~ . -Days -Kill - Plant, data=newplant, importance = TRUE, ntree = 10000, node_size = 1, mtry=131)
plants.rf

# try RF for class probability rather than classes themselves
plants$Prob <- plants$STEC / plants$Days
plants.rf = randomForest(Prob ~ . -STEC -Days -Kill - Plant, data=plants, weights = Days, importance = TRUE, ntree = 10000, node_size = 1, mtry=131)
plants.rf

library(rpart)
newdf <- newplant %>% select(-Days, -Kill, -Plant)
rpart(STEC ~ . , data=newdf, minsplit=1, cp=0.0001)

newdf2 <- plants %>% select(-Days, -Kill, -Plant, -STEC)
rpart(Prob ~ ., data=newdf2, weights = plants$Days)

# check whether this makes any sense at all, by working out the Gini on both sides of splits
x <- newdf$PlantDetails.PurposeBuiltBobbyCalfChain
improve <- function(x) {
  
  l <- levels(factor(x))
  g <- numeric(length(l))
  for (i in seq_along(l)) {
    p <- prop.table(table(newdf$STEC[x == l[i]]))
    gini <- sum(p*(1-p))
    g[i] <- gini * sum(x == l[i]) / length(x)
  }
  sum(g)*length(x)
}

g <- apply(newdf, 2, improve)
gr <- improve(rep(1, nrow(newdf)))
improve <- (gr - g)*nrow(newdf)

# creating data
set.seed(1324)
y <- sample(c(0,1), 30, T)
x <- y
x[1:5] <- 0
# manually making the first split
obs_L <- y[x<.5]
obs_R <- y[x>.5]
n_L <- sum(x<.5)
n_R <- sum(x>.5)
n <- length(x)
y
x
fit <- rpart(y~x, method = "class", parms=list(split='gini'))
fit$split[,3] # 5.384615

y
x

gini2 <- function(p) {sum(p*(1-p))}
calc.impurity <- function(func = gini2)
{
impurity_root <- func(prop.table(table(y)))
impurity_L <- func(prop.table(table(obs_L)))
impurity_R <-func(prop.table(table(obs_R)))
imp <- impurity_root - ((n_L/n)*impurity_L + (n_R/n)*impurity_R) # 0.3757
imp*n
}

calc.impurity(gini2)

# fuck!
y2 <- as.numeric(newdf$STEC)-1
x2 <- as.numeric(newdf$BoningRoom.EdibleBeltsCleanedDuringShift)-1

y <- y2[1:100+670]
x <- x2[1:100+670]
obs_L <- y[x<.5]
obs_R <- y[x>.5]
n_L <- sum(x<.5)
n_R <- sum(x>.5)
n <- length(x)
calc.impurity(gini2)

f <- rpart(y ~ x, method='class', minsplit=1, cp=0.0001)
f$splits


y <- c(rep(0,65),rep(1,15),rep(0,20))
x <- c(rep(0,70),rep(1,30))
#y <- y2[1:100+670]
#x <- x2[1:100+670]
obs_L <- y[x<.5]
obs_R <- y[x>.5]
n_L <- sum(x<.5)
n_R <- sum(x>.5)
n <- length(x)
calc.impurity(gini2)

f <- rpart(y ~ x, method='class', minsplit=1, cp=0.0001)
f$splits

y <- c(rep(0,65),rep(1,15),rep(0,20))
x <- c(rep(0,70),rep(1,30))
f <- rpart(y ~ x, method='class', minsplit=1, cp=0.0001, parms=list(split='gini'))

obs_L <- y[x<.5]
obs_R <- y[x>.5]
n_L <- sum(x<.5)
n_R <- sum(x>.5)
gini <- function(p) {sum(p*(1-p))}
impurity_root <- gini(prop.table(table(y)))
impurity_L <- gini(prop.table(table(obs_L)))
impurity_R <- gini(prop.table(table(obs_R)))
impurity_root * (n_L+n_R) - (n_L*impurity_L + n_R*impurity_R) # 2.880952


y <- c(rep(0,39),rep(1,16),rep(0,15))
x <- c(rep(0,40),rep(1,30))
rpart(y ~ x, method='class', parms=list(split='information'))

y <- c(rep(0,51),rep(1,25),rep(0,26))
x <- c(rep(0,51),rep(1,51))
rpart(y ~ x, method='class')$splits

library(randomForest)
y <- c(rep(0,51),rep(1,25),rep(0,26))
x <- c(rep(0,51),rep(1,51))
randomForest(as.factor(y) ~ x, ntree=10000)
rpart(as.factor(y) ~ x)
table(y,predict(rpart(as.factor(y) ~ x),type='class'))

y <- c(rep(0,51),rep(1,25),rep(0,26))
x <- c(rep(0,51),rep(1,51))
z <- c(rep(0,25),rep(1,26),rep(1,15),rep(0,10+26))
rpart(y ~ x + z, method='class')

y <- c(rep(0,65),rep(1,15),rep(0,20))
x <- c(rep(0,70),rep(1,30))
rpart(y ~ x, method='class', parms=list(split='gini'))
table(y,predict(rpart(as.factor(y) ~ x),type='class'))

# try with per-group probabilities instead
y <- newdf2$Prob
x <- as.numeric(newdf2$BoningRoom.EdibleBeltsCleanedDuringShift)-1
rpart(y ~ x, minsplit=1, cp=0.0000001)

obs_L <- y[x<.5]
obs_R <- y[x>.5]
n_L <- sum(x<.5)
n_R <- sum(x>.5)
#gini <- function(p) {sum(p*(1-p))}
impurity_root <- var(y)
impurity_L <- var(obs_L)
impurity_R <- var(obs_R)
impurity_root * (n_L+n_R-1) - (n_L*impurity_L + n_R*impurity_R) # 2.880952

predict(rpart(y ~ x, minsplit=1, cp=0.0000001))

y <- c(rep(0,10),rep(0.5,2))
x <- c(rep(0,10),rep(1,2))
rpart(y ~ x, minsplit=1, cp=0.0000000001)

test_rpart <- function(offset) {
  y <- c(rep(0,10),rep(0.5,2)) + offset
  x <- c(rep(0,10),rep(1,2))
  if (is.null(rpart(y ~ x, minsplit=1, cp=0, xval=0)$splits)) 0 else 1
}


test_split <- function(offset) {
  y <- c(rep(0,10),rep(0.5,2)) + offset
  x <- c(rep(0,10),rep(1,2))
  if (is.null(rpart(y ~ x, minsplit=1, cp=0, xval=0)$splits)) 0 else 1
}

sum(replicate(1000, test_split(0))) # 1000, i.e. always splits
sum(replicate(1000, test_split(0.5))) # 2, i.e. splits only sometimes...

offset <- 0.5
y <- c(rep(0,10),rep(0.5,2)) + offset
x <- c(rep(0,10),rep(1,2))
(mean(y[x<0.5]) - mean(y))^2 + (mean(y[x>0.5]) - mean(y))^2
rpart2(y ~ x, minsplit=1, cp=0, xval=0)$splits
predict(rpart(y ~ x, minsplit=1, cp=0, xval=0))

library(rpart)
test_split <- function(offset) {
  y <- c(rep(0,10),rep(0.5,2)) + offset
  x <- c(rep(0,10),rep(1,2))
  if (is.null(rpart(y ~ x, minsplit=1, cp=0, xval=0)$splits)) 0 else 1
}

sum(replicate(1000, test_split(0))) # 1000, i.e. always splits
sum(replicate(1000, test_split(0.5))) # 2, i.e. splits only sometimes...
sum(replicate(1000, test_split(0.007000000000015))) # 2, i.e. splits only sometimes...
sum(replicate(1000, test_split(0.00700000000001505))) # 2, i.e. splits only sometimes...
sum(replicate(1000, test_split(0.0070000000000151))) # 2, i.e. splits only sometimes...
sum(replicate(1000, test_split(0.00700000000001505))) # 2, i.e. splits only sometimes...
sum(replicate(1000, test_split(0.007000000000015))) # 2, i.e. splits only sometimes...

test_split <- function(g1, g2, offset) {
  y <- c(g1,g2) + offset
  x <- c(rep(0,length(g1)),rep(1,length(g2)))
  if (is.null(rpart(y ~ as.factor(x), minsplit=1, cp=0, xval=0)$splits)) 0 else 1
}

sum(replicate(1000, test_split(0))) # 1000, i.e. always splits
sum(replicate(1000, test_split(0.5))) # 2, i.e. splits only sometimes...

g1 <- rnorm(10,0,0.1)
g2 <- rnorm(2,1,0.1)
test_split <- function(g1, g2, offset) {
  y <- c(g1,g2) + offset
  x <- c(rep(0,length(g1)),rep(1,length(g2)))
  if (is.null(rpart(y ~ x, minsplit=1, cp=0, xval=0)$splits)) 0 else 1
}
sum(replicate(1000, test_split(g1,g2,0))) # 1000, i.e. always splits
sum(replicate(1000, test_split(g1,g2,1000))) # 2, i.e. splits only sometimes...
