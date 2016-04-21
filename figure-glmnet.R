source("packages.R")
load("glmnet.list.RData")

variable.counts <- glmnet.list$selected[, list(folds=.N), by=variable]
setkey(variable.counts, variable)
sorted.counts <- variable.counts[order(folds, variable),]
setkey(glmnet.list$selected, variable)
show.points <- glmnet.list$selected[variable.counts]
show.points[, variable.importance := factor(variable, sorted.counts$variable)]
setkey(glmnet.list$vline, test.fold, lambda.1se)
setkey(glmnet.list$cv, test.fold, lambda)
show.text <- glmnet.list$cv[glmnet.list$vline]

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
  facet_grid(what ~ test.fold + output.name, scales="free", labeller=function(var, val){
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
             data=data.table(glmnet.list$vline, set="validation"))+
  geom_ribbon(aes(-log(lambda), ymin=cvlo, ymax=cvup, fill=set),
              data=data.table(
                glmnet.list$cv, set="validation", what="binomial deviance"),
              alpha=0.5)+
  geom_line(aes(-log(lambda), cvm, color=set),
            data=data.table(
              glmnet.list$cv, set="validation", what="binomial deviance"))+
  geom_line(aes(-log(lambda), nzero),
            data=data.table(glmnet.list$cv, what="nzero"))+
  geom_text(aes(-log(lambda), nzero,
                label=sprintf(" %d features", nzero)),
            vjust=1,
            hjust=0,
            size=4,
            data=data.table(show.text, what="nzero"))+
  geom_line(aes(-log(lambda), auc, color=set),
            data=data.table(glmnet.list$auc, what="auc"))+
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
names(gene.colors) <- unique(glmnet.list$selected$gene)

gg.dots <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(folds ~ output.name, scales='free', space='free')+
  scale_color_manual(values=gene.colors)+
  geom_point(aes(weight, variable.importance, color=gene),
             shape=1,
             data=show.points)
pdf("figure-glmnet.pdf", h=9)
print(gg.dots)
dev.off()
png("figure-glmnet.png", h=600)
print(gg.dots)
dev.off()

