source("packages.R")

markers.dosages <- fread("ukbb_asthma_32markers_dosages")

save(markers.dosages, file="markers.dosages.RData")
