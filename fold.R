load("hla.RData")

set.seed(1)
n.folds <- 10
fold <- sample(rep(1:n.folds, l=nrow(hla$feature.mat)))

save(n.folds, fold, file="fold.RData")
