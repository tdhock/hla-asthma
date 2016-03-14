works_with_R("3.2.3",
             "tdhock/WeightedROC@3452d61638e16f547f73c1a0f3bf852a3751f29d")

load("fold.RData")
load("hla.RData")
load("trivial.RData")
objs <- load("glmnet.list.RData")

error.list <- list()
roc.list <- list()
for(test.fold in 1:n.folds){
  is.test <- fold == test.fold
  test.labels <- hla$clinical[is.test, ]
  label.counts <- table(test.labels$status)
  weight.list <- list(
    one=rep(1, nrow(test.labels)),
    balanced=1/label.counts[paste(test.labels$status)])
  prediction.list <- list(
    major.class=trivial[[test.fold]]
    )
  glmnet.by.weight <- glmnet.list[[test.fold]]
  for(weight.name in names(glmnet.by.weight)){
    model <- paste0("glmnet.", weight.name)
    prediction.list[[model]] <- glmnet.by.weight[[weight.name]]
  }
  for(model in names(prediction.list)){
    prob.asthma <- as.numeric(prediction.list[[model]])
    pred.label <- ifelse(0.5 < prob.asthma, "asthma", "healthy")
    is.error <- pred.label != test.labels$status
    test.label <- ifelse(test.labels$status=="asthma", 1, -1)
    for(test.weights in names(weight.list)){
      weight.vec <- weight.list[[test.weights]]
      roc <- WeightedROC(prob.asthma, test.label, weight.vec)
      auc <- WeightedAUC(roc)
      total.weight <- sum(weight.vec)
      weighted.error <- sum(is.error * weight.vec)
      error.list[[paste(test.fold, model, test.weights)]] <- 
        data.frame(test.fold, model, test.weights,
                   auc, total.weight, weighted.error)
      roc.list[[paste(test.fold, model, test.weights)]] <-
        data.frame(test.fold, model, test.weights, roc)
    }#for(test.weights
  }#for(model
}#for(test.fold

test.error <- list(
  error=do.call(rbind, error.list),
  roc=do.call(rbind, roc.list))

save(test.error, file="test.error.RData")
