works_with_R("3.2.3", glmnet="1.9.5", doParallel="1.0.6")
registerDoParallel()

load("fold.RData")
load("hla.RData")

glmnet.list <- list()
for(test.fold in 1:n.folds){
  is.test <- fold == test.fold
  is.train <- !is.test
  train.labels <- hla$clinical[is.train, ]
  train.features <- hla$feature.mat[is.train, ]
  test.features <- hla$feature.mat[is.test, ]
  label.counts <- table(train.labels$status)
  weight.list <- list(
    one=rep(1, nrow(train.features)),
    balanced=1/label.counts[paste(train.labels$status)])
  sapply(weight.list, sum)
  glmnet.list[[test.fold]] <- foreach(weight.name=names(weight.list)) %dopar% {
    cat(sprintf("%4d / %4d folds weights=%s\n",
                test.fold, n.folds, weight.name))
    weight.vec <- weight.list[[weight.name]]
    fit <- cv.glmnet(train.features, train.labels$status, weight.vec,
                     family="binomial")
    prob.vec <- predict(fit, test.features, type="response")
    ## prob = probability of having asthma.
    ## prob > 0.5 => asthma.
    ## prob < 0.5 => healthy.
    list(probability=prob.vec,
         fit=fit)
  }
}

save(glmnet.list, file="glmnet.list.RData")

