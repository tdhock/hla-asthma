load("fold.RData")
load("hla.RData")

prob.vec <- c(
  asthma=1,
  healthy=0)

trivial <- list()
for(test.fold in 1:n.folds){
  is.test <- fold == test.fold
  is.train <- !is.test
  train.labels <- hla$clinical[is.train, ]
  label.counts <- table(train.labels$status)
  major.class <- names(label.counts)[which.max(label.counts)]
  probability <- prob.vec[[major.class]]
  trivial[[test.fold]] <- rep(probability, sum(is.test))
}

save(trivial, file="trivial.RData")
