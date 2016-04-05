source("packages.R")

##big.dt <- fread("all_autoimmune.txt", header=TRUE)
corrected.dt <- fread("all_autoimmune_pheno_age.tab", header=TRUE)
setkey(corrected.dt, eid)
corrected.dt[eid==3881742,]
corrected.dt[`_1111_age`==0,]

pattern <- paste0(
  "^(?<colname>",
  "_",
  "(?<diseaseID>[0-9]+)",
  "(?<suffix>_age)?",
  ")")
match.df <- str_match_named(
  names(corrected.dt), pattern, list(diseaseID=as.integer))
match.df$variable <- ifelse(match.df$suffix=="_age", "ageAtOnset", "diseased")
meta.by.disease <- split(match.df, match.df$diseaseID)
disease.id.vec <- names(meta.by.disease)

relevant.dt <- fread("Autoimmune_diseases_codes_relevant.csv")
only.relevant <- relevant.dt[autoimmun_code==1 & !grepl("N/A", `decoded pheno code`),]

codes.dt <- fread("Autoimmune_diseases_codes.csv")
codes.dt[, code.str := paste(code)]
setkey(codes.dt, code.str)
disease.meta <- codes.dt[disease.id.vec, ]
keep.diseases <- disease.meta[!is.na(code),]
##stopifnot(only.relevant[["pheno_code or combinations of pheno codes"]] %in% keep.diseases$code)
keep.diseases$type <- ifelse(
  keep.diseases$code %in%
  only.relevant[["pheno_code or combinations of pheno codes"]],
  "autoimmune", "other")
keep.diseases[, table(type)]
stopifnot(sum(!is.na(keep.diseases$name)) == length(disease.id.vec))

##disease.dt <- big.dt[, keep.diseases$code.str, with=FALSE]
ageAtOnset.mat <- Matrix(
  as.numeric(0), nrow(corrected.dt), nrow(keep.diseases),
  dimnames=list(
    ID=paste(corrected.dt$eid),
    disease=keep.diseases$name)
  )
diseased.mat <- Matrix(
  FALSE, nrow(corrected.dt), nrow(keep.diseases),
  dimnames=list(
    ID=paste(corrected.dt$eid),
    disease=keep.diseases$name)
  )

for(disease.i in 1:nrow(keep.diseases)){
  disease.row <- keep.diseases[disease.i,]
  cat(sprintf("%4d / %4d diseases %s\n", disease.i, nrow(keep.diseases), disease.row$name))
  meta <- meta.by.disease[[disease.row$code.str]]
  one.disease <- list()
  for(variable.i in 1:nrow(meta)){
    meta.row <- meta[variable.i, ]
    one.disease[[paste(meta.row$variable)]] <-
      corrected.dt[[paste(meta.row$colname)]]
  }
  stopifnot(one.disease$diseased %in% c(1, 2))
  ## Audrey says affected codes are 2=yes, 1=no, 0=unknown.
  with(one.disease, stopifnot(identical(is.na(ageAtOnset), diseased==1)))
  with(one.disease, stopifnot(identical(!is.na(ageAtOnset), diseased==2)))
  negative.dt <- do.call(data.table, one.disease)[ageAtOnset < 0, ]
  with(negative.dt, table(diseased, ageAtOnset))
  stopifnot(nrow(negative.dt) == 0)
  ## (n.zero <- sum(one.disease$ageAtOnset==0, na.rm=TRUE))
  ## stopifnot(n.zero == 0)
  diseased.mat[, disease.row$name] <- one.disease$diseased == 2
  ageAtOnset.mat[, disease.row$name] <- ifelse(
    is.na(one.disease$ageAtOnset), 0, one.disease$ageAtOnset)
}

## Compute the proportion of matrix entries that we actually need to
## store.
sum(diseased.mat)/prod(dim(diseased.mat))

all.autoimmune <- list(
  disease.info=data.frame(keep.diseases),
  ageAtOnset=ageAtOnset.mat,
  diseased=diseased.mat)
str(all.autoimmune)

save(all.autoimmune, file="all.autoimmune.RData")
