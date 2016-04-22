source("packages.R")

(objs <- load("output.diseases.RData"))
load("hla.RData")

input.features <- hla$feature.mat[rownames(output.diseases$diseased),]
stopifnot(all(!is.na(input.features)))
input.features[1:4,1:4]

feature.sets <- list(
  hla=colnames(input.features)
  )

## feature.sets$hlaNoProb <-
##   grep("PROB", colnames(input.features), invert=TRUE, value=TRUE))

save(input.features, feature.sets, file="input.features.RData")
