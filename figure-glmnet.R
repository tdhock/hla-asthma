works_with_R("3.2.3", ggplot2="1.0.1", glmnet="1.9.5",
             "tdhock/namedCapture@05175927a45c301a18e8c6ebae67ea39a842d264")

load("glmnet.list.RData")
load("fold.RData")

variable.pattern <- paste0(
  "(?<gene>[^ ]+)",
  " ",
  "(?<precision>2|4)",
  " ",
  "(?<allele>.*)")
selected.by.fold <- list()
for(test.fold in 1:n.folds){
  glmnet.by.weight <- glmnet.list[[test.fold]]
  if(is.null(names(glmnet.by.weight))){
    names(glmnet.by.weight) <- c("one", "balanced")
  }
  fit <- glmnet.by.weight$balanced$fit
  coef.vec <- coef(fit)
  is.zero <- as.logical(coef.vec == 0)
  all.names <- rownames(coef.vec)[!is.zero]
  variable <- all.names[all.names != "(Intercept)"]
  meta.mat <- str_match_named(variable, variable.pattern)
  selected.by.fold[[test.fold]] <- data.table(
    test.fold, variable, meta.mat, weight=coef.vec[variable,])
}#for(test.fold
selected <- do.call(rbind, selected.by.fold)
variable.counts <- selected[, list(folds=.N), by=variable]
setkey(variable.counts, variable)
sorted.counts <- variable.counts[order(folds, variable),]
setkey(selected, variable)
show.points <- selected[variable.counts]
show.points[, variable.importance := factor(variable, sorted.counts$variable)]

##dput(RColorBrewer::brewer.pal(Inf, "Set1"))
gene.colors <-
  c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
    ##"#FFFF33",#yellow
    "#A65628",#brown
    "#F781BF",#pink
    "#999999")#grey
names(gene.colors) <- unique(selected$gene)

gg.dots <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(folds ~ ., scales='free', space='free')+
  scale_color_manual(values=gene.colors)+
  geom_point(aes(weight, variable.importance, color=gene),
             shape=1,
             data=show.points)
png("figure-glmnet.png")
print(gg.dots)
dev.off()
