source("packages.R")
load("glmnet.list.RData")

variable.counts <- glmnet.list$selected[, list(
  folds=.N), by=.(variable, output.name)]
setkey(variable.counts, variable, output.name)
sorted.counts <- variable.counts[order(folds, variable),]
variable.counts[, folds.fac := factor(folds, 8:1)]
stopifnot(all(!is.na(variable.counts$folds.fac)))
setkey(glmnet.list$selected, variable, output.name)
all.points <- glmnet.list$selected[variable.counts]

output.name.vec <- unique(sorted.counts$output.name)
for(output.i in seq_along(output.name.vec)){
  output <- output.name.vec[[output.i]]
  cat(sprintf("%4d / %4d %s\n", output.i, length(output.name.vec), output))
  output.path <- gsub("[ /']", "_", output)
  output.counts <- sorted.counts[output.name==output,]
  show.points <- all.points[output.name==output, ]
  show.points[, variable.importance := factor(variable, output.counts$variable)]
  stopifnot(all(!is.na(show.points$variable.importance)))
  show.points[, variable.type := ifelse(grepl("PROB", variable), "PROB score", "allele count")]
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
  oneOutput <- function(dt){
    dt[output.name==output,]
  }
  addY <- function(dt, what, ...){
    dt <- oneOutput(dt)
    L <- list(...)
    for(col.name in names(L)){
      dt[[col.name]] <- L[[col.name]]
    }
    data.table(dt, what=factor(what, c("binomial deviance", "nzero", "auc")))
  }
  with.legend <-
    ggplot()+
      ggtitle(output)+
      theme_bw()+
      theme(panel.margin=grid::unit(0, "lines"))+
      facet_grid(what ~ test.fold + model.name, scales="free", labeller=function(var, val){
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
                 data=data.table(
                   oneOutput(glmnet.list$vline), set="validation"))+
      geom_ribbon(aes(-log(lambda), ymin=cvlo, ymax=cvup, fill=set),
                  data=addY(glmnet.list$cv, "binomial deviance", set="validation"),
                  alpha=0.5)+
      geom_line(aes(-log(lambda), cvm, color=set),
                data=addY(
                  glmnet.list$cv, "binomial deviance", set="validation"))+
      geom_line(aes(-log(lambda), nzero),
                data=addY(glmnet.list$cv, "nzero"))+
      geom_text(aes(-log(lambda), nzero,
                    label=sprintf(" %d features", nzero)),
                vjust=1,
                hjust=0,
                size=4,
                data=addY(show.text, "nzero"))+
      geom_line(aes(-log(lambda), auc, color=set),
                data=addY(glmnet.list$auc, "auc"))+
      scale_x_continuous("model complexity -log(lambda)")+
      ylab("")
  
  png(sprintf("figure-glmnet-train-%s.png", output.path), width=1100)
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

  vline.df <- data.table(xintercept=0)
  gg.dots <- ggplot()+
    ggtitle(output)+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(folds.fac ~ model.name, scales='free', space='free_y')+
    scale_color_manual(values=gene.colors)+
    scale_fill_manual(values=c("PROB score"="grey50", "allele count"="white"))+
    geom_vline(aes(xintercept=xintercept),
               data=vline.df, color="grey")+
    geom_point(aes(weight, variable.importance, color=gene, fill=variable.type),
               shape=21,
               data=show.points)+
    xlab("<- more protective --- weight / effect size --- more deleterious ->")
  print(gg.dots)

  pdf(sprintf("figure-glmnet-%s.pdf", output.path), w=9, h=max(3, nlevels(show.points$variable.importance)/5))
  print(gg.dots)
  dev.off()
  png(sprintf("figure-glmnet-%s.png", output.path), w=600, h=max(300, nlevels(show.points$variable.importance)*10))
  print(gg.dots)
  dev.off()
}

