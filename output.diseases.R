source("packages.R")

load("hla.RData")
load("all.autoimmune.RData")

hla.dt <- data.table(hla$clinical)
diseases.ordered <- all.autoimmune$ageAtOnset[paste(hla.dt$ID), ]

autoimmune <- data.table(all.autoimmune$disease.info)[order(-cases),]
output.diseases <- diseases.ordered[, autoimmune$name]

stopifnot(identical(rownames(output.diseases), paste(hla.dt$ID)))
hla.dt$new.ageAtOnset <- output.diseases[, "asthma"]
hla.dt[, new.status := ifelse(new.ageAtOnset==0, "healthy", "diseased")]
(inconsistent <- hla.dt[new.status != status,])
with(hla.dt, table(status, new.status))
with(hla.dt, table(Aff, new.status))
stopifnot(nrow(inconsistent)==0)

save(output.diseases, file="output.diseases.RData")
