library(glmnet)

models <- list(
  glmnet.weight1=function(train.feature.mat, train.label.vec, test.feature.mat){
    label.counts <- table(train.label.vec)
    weight.vec <- rep(1, length(train.label.vec))
    fit <- cv.glmnet(train.feature.mat, train.label.vec, weight.vec,
                     family="binomial")
    prob.vec <- predict(fit, test.feature.mat, type="response")
    ## prob = probability of having asthma.
    ## prob > 0.5 => asthma.
    ## prob < 0.5 => healthy.
    list(probability=prob.vec,
         fit=fit)
  }, glmnet.weightBalanced=function(
       train.feature.mat, train.label.vec, test.feature.mat){
    label.counts <- table(train.label.vec)
    weight.vec <- 1/label.counts[paste(train.label.vec)]
    fit <- cv.glmnet(train.feature.mat, train.label.vec, weight.vec,
                     family="binomial")
    prob.vec <- predict(fit, test.feature.mat, type="response")
    list(probability=prob.vec,
         fit=fit)
  }, glmnet.weightBalanced.standardizeFALSE=function(
       train.feature.mat, train.label.vec, test.feature.mat){
    label.counts <- table(train.label.vec)
    weight.vec <- 1/label.counts[paste(train.label.vec)]
    fit <- cv.glmnet(train.feature.mat, train.label.vec, weight.vec,
                     family="binomial", standardize=FALSE)
    prob.vec <- predict(fit, test.feature.mat, type="response")
    list(probability=prob.vec,
         fit=fit)
  }, major.class=function(train.feature.mat, train.TF.vec, test.feature.mat){
    stopifnot(is.logical(train.TF.vec))
    train.label.vec <- ifelse(train.TF.vec, "diseased", "healthy")
    label.counts <- table(train.label.vec)
    major.class <- names(label.counts)[which.max(label.counts)]
    prob.vec <- c(healthy=0, diseased=1)
    probability <- prob.vec[[major.class]]
    list(probability=rep(probability, nrow(test.feature.mat)))
  })

save(models, file="models.RData")
