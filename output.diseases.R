source("packages.R")

load("hla.RData")
load("all.autoimmune.RData")

hla.dt <- data.table(hla$clinical)
ageAtOnset.ordered <- all.autoimmune$ageAtOnset[paste(hla.dt$ID), ]
diseased.ordered <- all.autoimmune$diseased[paste(hla.dt$ID), ]

hla.dt$new.asthma <- diseased.ordered[, "asthma"]
hla.dt$new.ageAtOnset <- ifelse(
  diseased.ordered[, "asthma"], ageAtOnset.ordered[, "asthma"], NA)
hla.dt[, new.status := ifelse(new.asthma, "diseased", "healthy")]
(inconsistent.status <- hla.dt[new.status != status,])
with(hla.dt, table(status, new.status))
with(hla.dt, table(Aff, new.status))
stopifnot(nrow(inconsistent.status)==0)
hla.dt[Age==0,]
classify <- function(x){
  ifelse(is.na(x), "missing",
         ifelse(x==0, "zero", "non-zero"))
}
hla.dt[, table(old=classify(Age), new=classify(new.ageAtOnset))]
(inconsistent.age <- hla.dt[classify(Age)!=classify(new.ageAtOnset),])
stopifnot(nrow(inconsistent.age) == 0)

save(output.diseases, file="output.diseases.RData")
