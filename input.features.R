load("hla.RData")

input.features <- list(
  hla=hla$feature.mat)

save(input.features, file="input.features.RData")
