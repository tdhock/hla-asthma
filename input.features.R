source("packages.R")

(objs <- load("output.diseases.RData"))
load("hla.RData")
load("markers.dosages.RData")

not.markers <- c("FID", "IID", "SEX")
is.marker <- ! names(markers.dosages) %in% not.markers
marker.mat <- as.matrix(markers.dosages[, is.marker, with=FALSE])
sex.vec <- Matrix(ifelse(markers.dosages$SEX==1, 1, 0))
marker.Mat <- Matrix(marker.mat)
rownames(sex.vec) <- rownames(marker.Mat) <- markers.dosages$IID

row.name.vec <- rownames(output.diseases$diseased)
input.features <- cbind(
  sex1=sex.vec[row.name.vec,],
  marker.Mat[row.name.vec,],
  hla$feature.mat[row.name.vec,])
stopifnot(all(!is.na(input.features)))
input.features[1:4,1:40]

feature.sets <- list(
  hla=colnames(hla$feature.mat),
  markers=colnames(marker.mat)
  )
feature.sets$hla.markers <- with(feature.sets, c(hla, markers))
feature.sets$hla.markers.sex <- with(feature.sets, c("sex1", hla, markers))

## feature.sets$hlaNoProb <-
##   grep("PROB", colnames(input.features), invert=TRUE, value=TRUE))

save(input.features, feature.sets, file="input.features.RData")
