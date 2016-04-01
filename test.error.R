source("packages.R")
load("fold.RData")
load("input.features.RData")
load("output.diseases.RData")
load("models.RData")

model.grid <- 
  expand.grid(
    test.fold=1:n.folds,
    input.name=names(input.features),
    output.name=colnames(output.diseases),
    model.name=names(models))

error.list <- list()
roc.list <- list()
for(model.i in 1:nrow(model.grid)){
  model.info <- model.grid[model.i, ]
  arg.vec <- sapply(model.info, paste)
  out.file <- paste0(paste(c("models", arg.vec), collapse="/"), ".RData")
  if(!file.exists(out.file)){
    cmd <- paste(c("R --no-save --args", arg.vec, "< one.model.R"), collapse=" ")
    system(cmd)
  }
  load(out.file)
  prob.diseased <- as.numeric(result.list$probability)
  pred.label <- ifelse(0.5 < prob.diseased, TRUE, FALSE)
  is.test <- model.info$test.fold == fold
  all.input.mat <- input.features[[paste(model.info$input.name)]]
  all.output.vec <- output.diseases[, paste(model.info$output.name)]
  test.output.vec <- all.output.vec[is.test]
  is.error <- pred.label != test.output.vec
  test.label <- ifelse(test.output.vec, 1, -1)
  label.counts <- table(test.output.vec)
  weight.list <- list(
    one=rep(1, length(test.output.vec)),
    balanced=1/label.counts[paste(test.output.vec)])
  for(test.weights in names(weight.list)){
    weight.vec <- weight.list[[test.weights]]
    roc <- WeightedROC(prob.diseased, test.label, weight.vec)
    auc <- WeightedAUC(roc)
    total.weight <- sum(weight.vec)
    weighted.error <- sum(is.error * weight.vec)
    error.list[[paste(model.i, test.weights)]] <- 
      data.frame(model.info, test.weights,
                 auc, total.weight, weighted.error,
                 row.names=NULL)
    roc.list[[paste(model.i, test.weights)]] <-
      data.frame(model.info, test.weights, roc,
                 row.names=NULL)
  }#for(test.weights
}#for(model.i

test.error <- list(
  error=do.call(rbind, error.list),
  roc=do.call(rbind, roc.list))

save(test.error, file="test.error.RData")
