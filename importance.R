# code for assessing whether anything is important. This will be interesting...

plants = read.csv("data/final_data.csv")

library(randomForest)

# ignore variables iwth missing data for now
missing <- which(apply(plants, 2, function(x) { sum(is.na(x)) }) > 0)
plants <- plants[,-missing]

# ignore variables which have no variance
no_variance <- which(apply(plants, 2, function(x) { length(unique(x))}) < 2)
plants <- plants[,-no_variance]

plants.rf = randomForest(STEC ~ . -Kill - Plant, data=plants, importance = TRUE, ntree = 10000, node_size = 1, mtry=131)

pdf("rf_importance.pdf", width=11, height=8)
varImpPlot(plants.rf, cex=0.5)
dev.off()

# try a LASSO
library(penalized)

plants.pr = penalized(STEC ~ . - Kill - Plant, data=plants, model="poisson", lambda1=1, lambda2 = 1, steps=20)
pdf("lasso_importance.pdf", width=11, height=8)
plotpath(plants.pr)
dev.off()
