source("packages.R")
load("fold.RData")
load("input.features.RData")
load("output.diseases.RData")
load("models.RData")

model.grid <- 
  expand.grid(
    test.fold=1:n.folds,
    input.name=names(feature.sets),
    output.name=colnames(output.diseases$diseased),
    model.name=names(models))
##model.grid <- subset(model.grid, model.name %in% paste0("glmnet.weight", c("Balanced", 1)))

error.list <- list()
roc.list <- list()
result.list <- foreach(model.i=1:nrow(model.grid)) %dopar% {
  model.info <- model.grid[model.i, ]
  arg.vec <- sapply(model.info, paste)
  out.file <- paste0(paste(c("models", arg.vec), collapse="/"), ".RData")
  if(!file.exists(out.file)){
    cmd <- paste(c("R --no-save --args", shQuote(arg.vec), "< one.model.R"), collapse=" ")
    status <- system(cmd)
    if(status != 0){
      stop(status, " error in model fitting R program")
    }
  }else{
    cat("already computed model\n")
    print(model.info)
  }
  load(out.file)
  prob.diseased <- as.numeric(result.list$probability)
  pred.label <- ifelse(0.5 < prob.diseased, TRUE, FALSE)
  is.test <- model.info$test.fold == fold
  input.col.vec <- feature.sets[[paste(model.info$input.name)]]
  all.input.mat <- input.features[, input.col.vec]
  all.output.vec <- output.diseases$diseased[, paste(model.info$output.name)]
  test.output.vec <- all.output.vec[is.test]
  is.error <- pred.label != test.output.vec
  test.label <- ifelse(test.output.vec, 1, -1)
  label.positive <- test.output.vec == TRUE
  guess.positive <- pred.label == TRUE
  label.negative <- test.output.vec == FALSE
  guess.negative <- pred.label == FALSE
  is.false.negative <- label.positive & guess.negative
  is.false.positive <- label.negative & guess.positive
  FP <- sum(is.false.positive)
  FN <- sum(is.false.negative)
  total.positive <- sum(label.positive)
  total.negative <- sum(label.negative)
  FPR <- FP/total.negative
  FNR <- FN/total.positive
  label.counts <- table(test.output.vec)
  weight.list <- list(
    one=rep(1, length(test.output.vec)),
    balanced=1/label.counts[paste(test.output.vec)])
  results.by.weight <- list()
  for(test.weights in names(weight.list)){
    weight.vec <- weight.list[[test.weights]]
    roc <- WeightedROC(prob.diseased, test.label, weight.vec)
    auc <- WeightedAUC(roc)
    total.weight <- sum(weight.vec)
    weighted.error <- sum(is.error * weight.vec)
    results.by.weight[[test.weights]] <- list(
      error=data.frame(model.info, test.weights,
        TPR=1-FNR, FPR, FNR, FP, FN, total.positive, total.negative,
        auc, total.weight, weighted.error,
        row.names=NULL),
      roc=data.frame(model.info, test.weights, roc,
        row.names=NULL))
    ## error.list[[paste(model.i, test.weights)]] <- 
    ##   data.frame(model.info, test.weights,
    ##              auc, total.weight, weighted.error,
    ##              row.names=NULL)
    ## roc.list[[paste(model.i, test.weights)]] <-
    ##   data.frame(model.info, test.weights, roc,
    ##              row.names=NULL)
  }#for(test.weights
  results.by.weight
}#for(model.i

for(model.i in seq_along(result.list)){
  results.by.weight <- result.list[[model.i]]
  for(test.weights in names(results.by.weight)){
    result <- results.by.weight[[test.weights]]
    error.list[[paste(model.i, test.weights)]] <- result$error
    roc.list[[paste(model.i, test.weights)]] <- result$roc
  }
}

test.error <- list(
  error=do.call(rbind, error.list),
  roc=do.call(rbind, roc.list))
## test.error <- list(
##   error=do.call(rbind, error.list),
##   roc=do.call(rbind, roc.list))

save(test.error, file="test.error.RData")
