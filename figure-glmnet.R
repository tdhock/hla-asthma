works_with_R("3.2.3", ggplot2="1.0.1", glmnet="1.9.5",
             "tdhock/directlabels@7b4b08a5dd0ab86e0b90902b3a233903ddd42311",
             "tdhock/WeightedROC@3452d61638e16f547f73c1a0f3bf852a3751f29d",
             data.table="1.9.7",
             "tdhock/namedCapture@05175927a45c301a18e8c6ebae67ea39a842d264")

load("glmnet.list.RData")
load("fold.RData")
load("hla.RData")

variable.pattern <- paste0(
  "(?<gene>[^ ]+)",
  " ",
  "(?<precision>2|4)",
  " ",
  "(?<allele>.*)")
selected.by.fold <- list()
auc.by.fold <- list()
cv.by.fold <- list()
vline.by.fold <- list()
for(test.fold in 1:n.folds){
  glmnet.by.weight <- glmnet.list[[test.fold]]
  if(is.null(names(glmnet.by.weight))){
    names(glmnet.by.weight) <- c("one", "balanced")
  }
  ## Compute train and test auc as a function of lambda.
  print(test.fold)
  fit <- glmnet.by.weight$balanced$fit
  is.test <- fold == test.fold
  sets <- list(
    test=is.test,
    train=!is.test)
  auc.list <- list()
  for(set in names(sets)){
    is.set <- sets[[set]]
    set.features <- hla$feature.mat[is.set,]
    set.labels <- hla$clinical[is.set, "status"]
    set.pred.mat <- predict(fit$glmnet.fit, set.features)
    for(lambda.i in seq_along(fit$glmnet.fit$lambda)){
      lambda <- fit$glmnet.fit$lambda[[lambda.i]]
      set.pred.vec <- set.pred.mat[, lambda.i]
      roc <- WeightedROC(set.pred.vec, set.labels)
      auc <- WeightedAUC(roc)
      auc.list[[paste(set, lambda.i)]] <- data.table(
        set, lambda, auc)
    }
  }
  auc.dt <- do.call(rbind, auc.list)
  auc.by.fold[[test.fold]] <- data.table(
    test.fold, auc.dt)
  cv.err <- with(fit, data.table(lambda, cvm, cvsd, cvup, cvlo, nzero))
  cv.by.fold[[test.fold]] <- data.table(
    test.fold, cv.err)
  vline.dt <- with(fit, data.table(lambda.1se))
  vline.by.fold[[test.fold]] <- data.table(
    test.fold, vline.dt)
  with.legend <- ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(what ~ ., scales="free")+
    geom_line(aes(-log(lambda), auc, color=set),
              data=data.table(auc.dt, what="auc"))+
    scale_x_continuous("model complexity -log(lambda)")+
    ylab("")+
    geom_vline(aes(xintercept=-log(lambda.1se)),
               data=vline.dt)+
    geom_ribbon(aes(-log(lambda), ymin=cvlo, ymax=cvup),
                data=data.table(cv.err, what="binomial deviance"),
                alpha=0.5)+
    geom_line(aes(-log(lambda), cvm),
              data=data.table(cv.err, what="binomial deviance"))+
    geom_line(aes(-log(lambda), nzero),
              data=data.table(cv.err, what="nzero"))
  direct.label(with.legend, "last.qp")
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
show.auc <- do.call(rbind, auc.by.fold)
show.cv <- do.call(rbind, cv.by.fold)
show.vline <- do.call(rbind, vline.by.fold)
setkey(show.vline, test.fold, lambda.1se)
setkey(show.cv, test.fold, lambda)
show.text <- show.cv[show.vline]

set.colors <-
  c(validation="#4DAF4A",
    train="#E41A1C",
    test="#377EB8",
    "#984EA3", "#FF7F00",
    ##"#FFFF33",#yellow
    "#A65628",#brown
    "#F781BF",#pink
    "#999999")#grey
with.legend <-
  ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(what ~ test.fold, scales="free", labeller=function(var, val){
    if(var=="test.fold"){
      paste("test fold", val)
    }else{
      paste(val)
    }
  })+
  scale_color_manual(values=set.colors, breaks=names(set.colors))+
  scale_fill_manual(values=set.colors)+
  guides(fill="none")+
  geom_vline(aes(xintercept=-log(lambda.1se), color=set),
             data=data.table(show.vline, set="validation"))+
  geom_ribbon(aes(-log(lambda), ymin=cvlo, ymax=cvup, fill=set),
              data=data.table(
                show.cv, set="validation", what="binomial deviance"),
              alpha=0.5)+
  geom_line(aes(-log(lambda), cvm, color=set),
            data=data.table(
              show.cv, set="validation", what="binomial deviance"))+
  geom_line(aes(-log(lambda), nzero),
            data=data.table(show.cv, what="nzero"))+
  geom_text(aes(-log(lambda), nzero,
                label=sprintf(" %d features", nzero)),
            vjust=1,
            hjust=0,
            size=4,
            data=data.table(show.text, what="nzero"))+
  geom_line(aes(-log(lambda), auc, color=set),
            data=data.table(show.auc, what="auc"))+
  scale_x_continuous("model complexity -log(lambda)")+
  ylab("")
png("figure-glmnet-train.png", width=1100)
print(with.legend)
dev.off()

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
