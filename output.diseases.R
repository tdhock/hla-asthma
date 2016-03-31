load("hla.RData")
load("all.autoimmune.RData")

clinical.dt <- data.table(hla$clinical)
clinical.dt[, ID.str := paste(ID)]
setkey(clinical.dt, ID.str)
clinical.ordered <- clinical.dt[rownames(all.autoimmune$patient.status),]

autoimmune <- data.table(all.autoimmune$disease.info)[type=="autoimmune",][order(-cases),][1:10,]
output.diseases <- all.autoimmune$patient.status[, autoimmune$name]
clinical.ordered$new.asthma <- output.diseases[, "asthma"]
clinical.ordered[, new.status := ifelse(new.asthma, "diseased", "healthy")]
inconsistent <- clinical.ordered[new.status != status,]
stopifnot(nrow(inconsistent)==0)
with(clinical.ordered, table(status, new.status))

save(output.diseases, file="output.diseases.RData")
