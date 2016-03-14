load("hla.RData")

set.seed(1)
n.folds <- 10
## stratified random sampling for the test sets.
fold <- rep(NA, nrow(hla$clinical))
for(status in c("healthy", "asthma")){
  is.status <- hla$clinical$status == status
  n.status <- sum(is.status)
  fold[is.status] <- sample(rep(1:n.folds, l=n.status))
}
table(fold, hla$clinical$status)

save(n.folds, fold, file="fold.RData")
