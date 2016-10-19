source("packages.R")
load("fold.RData")
load("models.RData")
load("input.features.RData")
load("output.diseases.RData")

full.model.grid <- 
  expand.grid(
    test.fold=1:n.folds,
    input.name=names(feature.sets),
    output.name=colnames(output.diseases$diseased),
    model.name=names(models))
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
  sets <- list(
    test=is.test,
    train=!is.test)
  auc.list <- list()
  for(set in names(sets)){
    is.set <- sets[[set]]
    set.features <- all.features[is.set,]
    set.labels <- ifelse(all.labels[is.set], 1, -1)
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
      model.info,
      auc.dt),
    cv=data.table(
      model.info,
      cv.err),
    vline=data.table(
      model.info,
      vline.dt))
  if(0 < length(variable)){
    meta.mat <- str_match_named(variable, variable.pattern)
    ##selected.by.fold[[test.fold]] <-
    one.result$selected <- data.table(
      model.info,
      variable, meta.mat, weight=coef.vec[variable,])
  }
  one.result
}#for(test.fold

glmnet.list <- list()
for(data.type in names(model.results[[1]])){
  glmnet.list[[data.type]] <-
    do.call(rbind, lapply(model.results, "[[", data.type))
}

save(glmnet.list, file="glmnet.list.RData")
