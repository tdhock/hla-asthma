works_with_R("3.2.3", data.table="1.9.7")

library(Matrix)

big.dt <- fread("all_autoimmune.txt", header=TRUE)
disease.id.vec <- names(big.dt)[-c(1:3)]

relevant.dt <- fread("Autoimmune_diseases_codes_relevant.csv")
only.relevant <- relevant.dt[autoimmun_code==1 & !grepl("N/A", `decoded pheno code`),]

codes.dt <- fread("Autoimmune_diseases_codes.csv")
codes.dt[, code.str := paste(code)]
setkey(codes.dt, code.str)
disease.meta <- codes.dt[disease.id.vec, ]
keep.diseases <- disease.meta[!is.na(code),]
stopifnot(only.relevant[["pheno_code or combinations of pheno codes"]] %in% keep.diseases$code)
keep.diseases$type <- ifelse(
  keep.diseases$code %in%
  only.relevant[["pheno_code or combinations of pheno codes"]],
  "autoimmune", "other")
keep.diseases[, table(type)]

disease.dt <- big.dt[, keep.diseases$code.str, with=FALSE]
disease.mat <- Matrix(as.logical(0), nrow(disease.dt), ncol(disease.dt))
dimnames(disease.mat) <- list(
  ID=big.dt$IID,
  disease=keep.diseases$name)

for(disease.i in 1:nrow(keep.diseases)){
  disease.row <- keep.diseases[disease.i,]
  int.vec <- disease.dt[[disease.row$code.str]]
  disease.mat[, disease.row$name] <- ifelse(int.vec==1, TRUE, FALSE)
}

colSums(disease.mat)

all.autoimmune <- list(
  disease.info=data.frame(keep.diseases),
  patient.status=disease.mat)

save(all.autoimmune, file="all.autoimmune.RData")
