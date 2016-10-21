works_with_R("3.2.3",
             glmnet="1.9.5")

arg.vec <- c("1", "hla", "Age6", "glmnet.weightBalanced.standardizeFALSE")
arg.vec <- c("1", "hla", "type 1 diabetes", "glmnet.weight1")
arg.vec <- c("1", "hla", "type 1 diabetes", "major.class")
arg.vec <- commandArgs(trailingOnly=TRUE)
stopifnot(length(arg.vec)==4)
print(arg.vec)

load("fold.RData")
load("input.features.RData")
load("output.diseases.RData")
load("models.RData")

test.fold <- as.integer(arg.vec[1])
is.test <- test.fold == fold
stopifnot(0 < sum(is.test))

input.name <- arg.vec[2]
input.col.vec <- feature.sets[[input.name]]
all.input.mat <- input.features[, input.col.vec]
stopifnot(inherits(all.input.mat, "Matrix"))
stopifnot(0 < nrow(all.input.mat))
stopifnot(0 < ncol(all.input.mat))

output.name <- arg.vec[3]
all.output.vec <- output.diseases$diseased[, output.name]
stopifnot(is.logical(all.output.vec))
stopifnot(length(all.output.vec) == nrow(all.input.mat))

model.name <- arg.vec[4]
model.fun <- models[[model.name]]
stopifnot(is.function(model.fun))

is.train <- !is.test
train.input.mat <- all.input.mat[is.train,]
train.output.vec <- all.output.vec[is.train]
test.input.mat <- all.input.mat[is.test,]
keep <- !is.na(train.output.vec)
result.list <-
  model.fun(train.input.mat[keep,], train.output.vec[keep], test.input.mat)

out.file <- paste0(paste(c("models", arg.vec), collapse="/"), ".RData")
out.dir <- dirname(out.file)
dir.create(out.dir, showWarnings=FALSE, recursive=TRUE)
save(result.list, file=out.file)
cat(out.file, "computed.\n")

