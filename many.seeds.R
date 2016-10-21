source("packages.R")

load("input.features.RData")
load("output.diseases.RData")
load("models.RData")

seed <- 1
n.folds <- 4
n.seeds <- 100
foreach(seed=1:n.seeds) %dopar% {
  label.vec <- output.diseases$diseased[, "asthma"]
  set.seed(seed)
  fold.vec <- sample(rep(1:n.folds, l=length(label.vec)))
  is.test <- fold.vec==1
  is.train <- !is.test
  result.list.RData <- file.path("many.seeds", seed, "result.list.RData")
  if(!file.exists(result.list.RData)){
    cat(sprintf("%4d / %4d %s\n", seed, n.seeds, result.list.RData))
    result.list <- models$glmnet.weightBalanced.standardizeFALSE(
      input.features[is.train,],
      label.vec[is.train],
      input.features[is.test,])
    seed.dir <- dirname(result.list.RData)
    dir.create(seed.dir, showWarnings=FALSE, recursive=TRUE)
    save(result.list, file=result.list.RData)
  }
}
