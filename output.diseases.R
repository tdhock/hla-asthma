source("packages.R")

load("hla.RData")
load("all.autoimmune.RData")

asthma.ages <- fread("ages_toby.txt")
setkey(asthma.ages, IID)

hla.dt <- data.table(hla$clinical)
ageAtOnset.ordered <- all.autoimmune$ageAtOnset[paste(hla.dt$ID), ]
diseased.ordered <- all.autoimmune$diseased[paste(hla.dt$ID), ]
asthmaAge.ordered <- asthma.ages[J(hla.dt$ID)]
table(
  diseased.ordered[, "asthma"],
  asthmaAge.ordered$Age1)
table(
  diseased.ordered[, "asthma"],
  asthmaAge.ordered$Age8)
makeLogical <- function(num.vec){
  ifelse(num.vec == -9, NA, num.vec == 1)
}
asthmaAge.mat <- Matrix(makeLogical(as.matrix(asthmaAge.ordered[, .(Age6, Age8)])))
rownames(asthmaAge.mat) <- asthmaAge.ordered$IID
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
  ## Audrey says that when age < 0, we know that the person has
  ## asthma, but we do not know when that person got it.
  ifelse(is.na(x) | x < 0, "missing",
         ifelse(x==0, "zero", "non-zero"))
}
hla.dt[, table(old=classify(Age), new=classify(new.ageAtOnset))]
(inconsistent.age <- hla.dt[classify(Age)!=classify(new.ageAtOnset),])
stopifnot(nrow(inconsistent.age) == 0)

output.diseases <- list(
  diseased=cbind(diseased.ordered, asthmaAge.mat),
  ageAtOnset=ageAtOnset.ordered)

save(output.diseases, file="output.diseases.RData")
