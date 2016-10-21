source("packages.R")
load("fold.RData")
load("models.RData")
load("input.features.RData")
load("output.diseases.RData")

model.grid <- 
  expand.grid(
    test.fold=1:n.folds,
    input.name=names(feature.sets),
    output.name="asthma",
    model.name="glmnet.weightBalanced.standardizeFALSE")

variable.pattern <- paste0(
  "(?<gene>[^ ]+)",
  " ",
  "(?<precision>2|4)",
  " ",
  "(?<allele>.*)")

i.vec <- 1:nrow(model.grid)
##i.vec <- with(model.grid, which(output.name %in% c("asthma", "type 2 diabetes") & test.fold==1))
model.results <- foreach(file.i=i.vec) %dopar% {
  model.info <- model.grid[file.i, ]
  arg.vec <- sapply(model.info, paste)
  RData.file <- paste0(paste(c("models", arg.vec), collapse="/"), ".RData")
  objs <- load(RData.file)
  test.fold <- as.integer(model.info$test.fold)
  ## Compute train and test auc as a function of lambda.
  output.name <- paste(model.info$output.name)
  cat(sprintf("%4d / %4d models\n", file.i, nrow(model.grid)))
  is.test <- fold == test.fold
  input.col.vec <- feature.sets[[paste(model.info$input.name)]]
  all.features <- input.features[, input.col.vec]
  all.labels <- output.diseases$diseased[, output.name]
  coef.vec <- coef(result.list$fit)
  is.zero <- as.logical(coef.vec == 0)
  all.names <- rownames(coef.vec)[!is.zero]
  variable <- all.names[all.names != "(Intercept)"]
  one.result <- list()
  if(0 < length(variable)){
    meta.mat <- str_match_named(variable, variable.pattern)
    ##selected.by.fold[[test.fold]] <-
    one.result$selected <- data.table(
      model.info,
      variable, meta.mat, weight=coef.vec[variable,])
  }
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
  weight.vec <- rep(1, length(test.label))
  roc <- WeightedROC(prob.diseased, test.label, weight.vec)
  auc <- WeightedAUC(roc)
  total.weight <- sum(weight.vec)
  weighted.error <- sum(is.error * weight.vec)
  one.result$error <- data.table(model.info, 
    TPR=1-FNR, FPR, FNR, FP, FN, total.positive, total.negative,
    auc, total.weight, weighted.error)
  one.result$roc <- data.table(model.info, roc)
  one.result
}#for(test.fold

input.compare <- list()
for(data.type in names(model.results[[1]])){
  input.compare[[data.type]] <-
    do.call(rbind, lapply(model.results, "[[", data.type))
}

save(input.compare, file="input.compare.RData")
