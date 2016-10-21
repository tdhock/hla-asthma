source("packages.R")

load("input.features.RData")
load("output.diseases.RData")
load("models.RData")

n.folds <- 100
label.vec <- output.diseases$diseased[, "asthma"]
set.seed(1)
fold.vec <- sample(rep(1:n.folds, l=length(label.vec)))
test.fold <- 1

foreach(test.fold=1:n.folds) %dopar% {
  is.test <- fold.vec == test.fold
  is.train <- !is.test
  result.list.RData <- file.path("many.folds", test.fold, "result.list.RData")
  if(!file.exists(result.list.RData)){
    cat(sprintf("%4d / %4d %s\n", test.fold, n.folds, result.list.RData))
    result.list <- models$glmnet.weightBalanced.standardizeFALSE(
      input.features[is.train,],
      label.vec[is.train],
      input.features[is.test,])
    fold.dir <- dirname(result.list.RData)
    dir.create(fold.dir, showWarnings=FALSE, recursive=TRUE)
    save(result.list, file=result.list.RData)
  }
}

variable.pattern <- paste0(
  "(?<gene>[^ ]+)",
  " ",
  "(?<precision>2|4)",
  " ",
  "(?<allele>.*)")

model.results <- foreach(test.fold=1:n.folds) %dopar% {
  RData.file <- file.path("many.folds", test.fold, "result.list.RData")
  objs <- load(RData.file)
  ## Compute train and test auc as a function of lambda.
  cat(sprintf("%4d / %4d test folds\n", test.fold, n.folds))
  is.test <- fold.vec == test.fold
  sets <- list(
    test=is.test,
    train=!is.test)
  auc.list <- list()
  for(set in names(sets)){
    is.set <- sets[[set]]
    set.features <- input.features[is.set,]
    set.labels <- ifelse(label.vec[is.set], 1, -1)
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
  ## auc.by.fold[[test.fold]] <- data.table(
  ##   input.name, output.name, test.fold, auc.dt)
  cv.err <- with(result.list$fit, {
    data.table(lambda, cvm, cvsd, cvup, cvlo, nzero)
  })
  ## cv.by.fold[[test.fold]] <- data.table(
  ##   input.name, output.name, test.fold, cv.err)
  vline.dt <- with(result.list$fit, data.table(lambda.1se))
  ## vline.by.fold[[test.fold]] <- data.table(
  ##   input.name, output.name, test.fold, vline.dt)
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
  ##direct.label(with.legend, "last.qp")
  coef.vec <- coef(result.list$fit)
  is.zero <- as.logical(coef.vec == 0)
  all.names <- rownames(coef.vec)[!is.zero]
  variable <- all.names[all.names != "(Intercept)"]
  one.result <- list(
    auc=data.table(
      test.fold,
      auc.dt),
    cv=data.table(
      test.fold,
      cv.err),
    vline=data.table(
      test.fold,
      vline.dt))
  if(0 < length(variable)){
    meta.mat <- str_match_named(variable, variable.pattern)
    ##selected.by.fold[[test.fold]] <-
    one.result$selected <- data.table(
      test.fold,
      variable, meta.mat, weight=coef.vec[variable,])
  }
  one.result
}#for(test.fold

many.folds <- list()
for(data.type in names(model.results[[1]])){
  many.folds[[data.type]] <-
    do.call(rbind, lapply(model.results, "[[", data.type))
}

save(many.folds, file="many.folds.RData")
