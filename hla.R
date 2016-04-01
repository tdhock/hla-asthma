source("packages.R")

asthma.pattern <- paste0(
  "(?<name>",
  "hla_asthma_",
  "(?<precision>2|4)",
  "_",
  "(?<gene>.*)",
  ")")

e <- new.env()
load("test.RData", e)
(asthma.obj.mat <- str_match_named(ls(e), asthma.pattern))
(not.na.mat <- asthma.obj.mat[!is.na(asthma.obj.mat[,1]),])
stopifnot(nrow(not.na.mat) == 14)
obj.name <- rownames(not.na.mat)[[1]]
id.vec <- e[[obj.name]]$ID

feature.list <- list()
for(obj.name in rownames(not.na.mat)){
  obj.meta <- data.table(not.na.mat[obj.name,,drop=FALSE])
  gene.dt <- unique(data.table(e[[obj.name]]))
  stopifnot(nrow(gene.dt) == 120286)
  stopifnot(gene.dt$ID == id.vec)
  ## Audrey says Aff means affected with asthma, 2=yes, 1=no,
  ## 0=unknown.
  allele.mat <- as.matrix(gene.dt[,.(A1, A2)])
  count.tab <- table(allele.mat)
  cat(sprintf("%d rows %d alleles for %s\n",
              nrow(gene.dt), length(count.tab), obj.name))
  for(allele in names(count.tab)){
    n.alleles <- rowSums(allele.mat==allele)
    feature.name <- obj.meta[, paste(gene, precision, allele)]
    feature.list[[feature.name]] <- Matrix(n.alleles)
  }
  feature.name <- obj.meta[, paste(gene, precision, "PROB")]
  feature.list[[feature.name]] <- gene.dt$PROB
  ## Checking for duplicates...
  id.tab <- table(gene.dt$ID)
  table(id.tab)
  dup.names <- names(id.tab)[1 < id.tab]
  dup.id.dt <- gene.dt[ID %in% dup.names,]
  dup.id.dt[order(ID),]
}

clinical <- gene.dt[, .(ID, Aff, Age)]
clinical[, status := factor(Aff, 1:2, c("healthy", "diseased"))]
table(clinical$Aff)

hla <- list(
  feature.mat=do.call(cbind, feature.list),
  clinical=data.frame(clinical)
  )
rownames(hla$feature.mat) <- id.vec
colnames(hla$feature.mat) <- names(feature.list)
str(hla)

save(hla, file="hla.RData")
