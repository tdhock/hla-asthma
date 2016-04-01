load("output.diseases.RData")
load("hla.RData")

all.features <- hla$feature.mat
  

save(input.features, file="input.features.RData")
