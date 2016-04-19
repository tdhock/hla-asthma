(objs <- load("output.diseases.RData"))
load("hla.RData")

input.features <- hla$feature.mat[rownames(output.diseases),]
stopifnot(all(!is.na(input.features)))
input.features[1:4,1:4]

feature.sets <- list(hla=colnames(input.features))

save(input.features, feature.sets, file="input.features.RData")
