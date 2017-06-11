library(dplyr)

#' Read in the data
plants = read.csv("data/final_data.csv")
priors = read.csv("data/final_prior_data.csv")

# Days is missing for plant 9
plants <- plants[plants$Plant != 'Plant9',]

# ignore variables iwth missing data for now
# ignore variables which have no variance
missing <- apply(plants[,1:146], 2, function(x) { sum(is.na(x)) }) > 0
no_variance <- apply(plants[,1:146], 2, function(x) { length(unique(x))}) < 2

# table up the names of the plants along with the various functions they have
vars <- plants[,1:146]
nvar <- names(vars)
ovar <- unlist(lapply(seq_along(nvar), function(x) { paste(sort(unique(plants[,x])), collapse=', ') }))

priors <- priors[match(nvar, priors$Variable),]

# split up the variable names and add a space between capitals
nvar <- gsub("Head\\.Tongue", "Head & Tongue", nvar)
nvar <- gsub("Shoulder\\.NeckClearing\\.Flaying", "Shoulder & Neck clearing - flaying", nvar)
nvar <- gsub("PullingSocks\\.ShouldersManually", "Pulling socks / shoulders manually", nvar)
nvar <- gsub("Navel\\.Umbilicus", "Navel / Umbilicus", nvar)
nvar <- gsub("Changeover\\.GambrelUpOperator", "Changeover / Gambrel Up Operator", nvar)
nvar <- gsub("CarcassWashing.WaterContact", "CarcassWashing / WaterContact", nvar)
nvar <- gsub("MethodUsedI.e.FlayingByKnifeOrAirknifeOrByRoller", "MethodUsed", nvar)
nvar <- gsub("([a-z])([A-Z])", "\\1 \\2", nvar)

nsplit <- strsplit(nvar, "\\.")
ncat <- unlist(lapply(nsplit, function(x) { x[[1]] }))
nsub <- unlist(lapply(nsplit, function(x) { paste(unlist(x[-1]), collapse=' ') }))

baseline <- unlist(lapply(vars, function(x) { if (is.factor(x)) levels(x)[1] else 0 }))

d <- data.frame(VarName=names(vars),
                Category=ncat, Variable=nsub,
                Missing=ifelse(missing, "Yes", ""),
                `No Variation`=ifelse(no_variance, "Yes", ""),
                `Effect Likely`=priors$Effect,
                `Effect Unlikely`=priors$NoEffect,
                `Effect Unknown`=priors$Unknown,
                Baseline=baseline,
                 stringsAsFactors = FALSE, check.names = FALSE)

write.csv(d, "temp/table.csv", row.names=FALSE)
