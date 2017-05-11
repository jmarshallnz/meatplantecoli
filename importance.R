# code for assessing whether anything is important. This will be interesting...

plants = read.csv("data/final_data.csv")

library(randomForest)

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
  newplant$STEC[start[i] + 1:plants$STEC[i]] <- 1
}
newplant$STEC <- factor(newplant$STEC)

plants.rf = randomForest(STEC ~ . -Days -Kill - Plant, data=newplant, importance = TRUE, ntree = 10000, node_size = 1, mtry=131)


newplant$STEC <- as.numeric(as.character(newplant$STEC))
plants.pr = penalized(STEC ~ . - Kill - Days - Plant, data=newplant, model="logistic", lambda1=1, lambda2 = 1, steps=20)
pdf("lasso_importance.pdf", width=11, height=8)
plotpath(plants.pr)
dev.off()


plants.rf = randomForest(STEC ~ . -Kill - Plant, data=plants, importance = TRUE, ntree = 10000, node_size = 1, mtry=131)


pdf("rf_importance.pdf", width=11, height=8)
varImpPlot(plants.rf, cex=0.5)
dev.off()

# try a LASSO
library(penalized)

plants.pr = penalized(STEC ~ . - Kill - Plant, data=plants, model="binomial", lambda1=1, lambda2 = 1, steps=20)
pdf("lasso_importance.pdf", width=11, height=8)
plotpath(plants.pr)
dev.off()
