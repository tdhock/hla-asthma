source("packages.R")
load("fold.RData")
load("input.features.RData")
load("output.diseases.RData")

RData.file.vec <- Sys.glob("models/*/*/*/glmnet.weightBalanced.RData")
file.pattern <- paste0(
  "models/",
  "(?<test_fold>[^/]+)",
  "/",
  "(?<input_name>[^/]+)",
  "/",
  "(?<output_name>[^/]+)",
  "/glmnet.weightBalanced.RData")
match.mat <- str_match_named(RData.file.vec, file.pattern)

variable.pattern <- paste0(
  "(?<gene>[^ ]+)",
  " ",
  "(?<precision>2|4)",
  " ",
  "(?<allele>.*)")
selected.by.fold <- list()
auc.by.fold <- list()
cv.by.fold <- list()
vline.by.fold <- list()
for(file.i in seq_along(RData.file.vec)){
  RData.file <- RData.file.vec[[file.i]]
  match.row <- match.mat[file.i,,drop=FALSE]
  objs <- load(RData.file)
  test.fold <- as.integer(match.row[, "test_fold"])
  ## Compute train and test auc as a function of lambda.
  input.name <- paste(match.row[, "input_name"])
  output.name <- paste(match.row[, "output_name"])
  cat(sprintf("%4d / %4d models\n", file.i, length(RData.file.vec)))
  is.test <- fold == test.fold
  all.features <- input.features[[input.name]]
  all.labels <- output.diseases[, output.name]
  sets <- list(
    test=is.test,
    train=!is.test)
  auc.list <- list()
  for(set in names(sets)){
    is.set <- sets[[set]]
    set.features <- all.features[is.set,]
    set.labels <- all.labels[is.set]
    set.pred.mat <- predict(result.list$fit$glmnet.fit, set.features)
    for(lambda.i in seq_along(result.list$fit$glmnet.fit$lambda)){
      lambda <- result.list$fit$glmnet.fit$lambda[[lambda.i]]
      set.pred.vec <- set.pred.mat[, lambda.i]
      roc <- WeightedROC(set.pred.vec, set.labels)
      auc <- WeightedAUC(roc)
      auc.list[[paste(set, lambda.i)]] <- data.table(
        set, lambda, auc)
    }
  }
  auc.dt <- do.call(rbind, auc.list)
  auc.by.fold[[test.fold]] <- data.table(
    input.name, output.name, test.fold, auc.dt)
  cv.err <- with(result.list$fit, {
    data.table(lambda, cvm, cvsd, cvup, cvlo, nzero)
  })
  cv.by.fold[[test.fold]] <- data.table(
    input.name, output.name, test.fold, cv.err)
  vline.dt <- with(result.list$fit, data.table(lambda.1se))
  vline.by.fold[[test.fold]] <- data.table(
    input.name, output.name, test.fold, vline.dt)
  with.legend <- ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(what ~ ., scales="free")+
    geom_line(aes(-log(lambda), auc, color=set),
              data=data.table(auc.dt, what="auc"))+
    scale_x_continuous("model complexity -log(lambda)")+
    ylab("")+
    geom_vline(aes(xintercept=-log(lambda.1se)),
               data=vline.dt)+
    geom_ribbon(aes(-log(lambda), ymin=cvlo, ymax=cvup),
                data=data.table(cv.err, what="binomial deviance"),
                alpha=0.5)+
    geom_line(aes(-log(lambda), cvm),
              data=data.table(cv.err, what="binomial deviance"))+
    geom_line(aes(-log(lambda), nzero),
              data=data.table(cv.err, what="nzero"))
  direct.label(with.legend, "last.qp")
  coef.vec <- coef(result.list$fit)
  is.zero <- as.logical(coef.vec == 0)
  all.names <- rownames(coef.vec)[!is.zero]
  variable <- all.names[all.names != "(Intercept)"]
  meta.mat <- str_match_named(variable, variable.pattern)
  selected.by.fold[[test.fold]] <- data.table(
    input.name, output.name, test.fold,
    variable, meta.mat, weight=coef.vec[variable,])
}#for(test.fold

glmnet.list <- list(
  selected=do.call(rbind, selected.by.fold),
  auc=do.call(rbind, auc.by.fold),
  cv=do.call(rbind, cv.by.fold),
  vline=do.call(rbind, vline.by.fold))
save(glmnet.list, file="glmnet.list.RData")
