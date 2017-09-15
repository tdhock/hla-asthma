source("packages.R")

library(data.table)
library(glmnet)
load("input.compare.RData")
load("input.features.RData")
feature.sets$markers %in% input.compare$selected$variable

some <- function(dt){
  dt[input.name != "hla.oldMarkers.sex"]
}

variable.counts <- some(input.compare$selected)[, list(
  mean.weight=mean(weight),
  folds=.N), by=.(variable, output.name, input.name)]
setkey(variable.counts, variable, output.name, input.name)
sorted.counts <- variable.counts[order(-folds, -abs(mean.weight)),]
variable.counts[, variable.fac := factor(variable, rev(unique(paste(sorted.counts$variable))))]
stopifnot(all(!is.na(variable.counts$variable.fac)))
setkey(input.compare$selected, variable, output.name, input.name)
all.points <- some(input.compare$selected[variable.counts])

some.counts <- variable.counts[input.name=="hla.markers.sex"]
levs <- some.counts[order(folds, mean.weight), variable]
some.points <- all.points[input.name=="hla.markers.sex" & folds==4]
some.points[, variable.fac := factor(variable, levs)]
some.points[, variable.type := ifelse(variable=="sex1", "sex", ifelse(grepl("rs", variable), "marker", "hla"))]
some.points[, table(variable.type, test.fold)]
gg <- ggplot()+
  ggtitle("Features used in models to predict asthma in all 4 cross-validation folds\n12/328 hla, 31/32 markers, 44/361 total")+
  geom_vline(xintercept=0, color="grey")+
  scale_color_brewer(palette="Dark2")+
  theme_bw()+
  geom_point(aes(weight, variable.fac, color=variable.type),
             data=some.points, shape=1)+
  xlab("input feature weight / effect size")+
  ylab("input feature")
pdf("../jobs-dec-2016/figure-asthma-4folds.pdf")
print(gg)
dev.off()
some.points[variable.type=="marker",  table(folds)]

test.roc <- some(input.compare$roc)[, {
  FPR.grid <- seq(0, 1, l=101)
  TPR.grid <- approx(FPR, TPR, FPR.grid)$y
  data.table(
    FPR=FPR.grid,
    TPR=TPR.grid
    )
}, by=.(input.name, test.fold)]
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(test.fold ~ .)+
  coord_equal()+
  geom_path(aes(FPR, TPR, color=input.name), data=test.roc)


test.dots <- some(input.compare$error)
test.means <- test.dots[, list(mean.auc=mean(auc)), by=input.name][order(mean.auc),]
feature.counts <- sort(sapply(feature.sets, length))
dname <- function(input.name){
  n.features <- feature.counts[paste(input.name)]
  sprintf("%s\n%d features", input.name, n.features)
}
levs <- dname(names(feature.counts))
test.dots[, input.fac := factor(dname(input.name), levs)]
gg.old <- ggplot()+
  scale_x_continuous(
    "Test AUC (4-fold cross-validation)",
    limits=c(0.5, 0.61)
    )+
  scale_y_discrete("Input feature set")+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  geom_point(aes(auc, input.fac),
             shape=1,
             data=test.dots)
pdf("figure-asthma-old.pdf", 4, 2.5)
print(gg.old)
dev.off()

load("output.compare.RData")
test.roc <- data.table(output.compare$roc)
orig.types <- c(hla="solid", hla.markers="dashed")
other.types <- orig.types
names(other.types) <- dname(names(orig.types))
gg <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(. ~ output.name, labeller=function(df){
    ##df$output.name <- gsub("[/]", "\n", df$output.name)
    df$output.name <- paste(df$output.name)
    df
  })+
  scale_linetype_manual(
    "input features",
    values=orig.types)+
  scale_color_discrete("input features")+
  scale_x_continuous(
    "False Positive Rate = Probability(Disease | Healthy)",
    breaks=c(0, 0.5, 1),
    labels=c("0", "0.5", "1"))+
  scale_y_continuous(
    "True Positive Rate\n= Probability(Disease | Disease)",
    breaks=c(0, 0.5, 1))+
  geom_path(aes(FPR, TPR, group=paste(test.fold, input.name),
                linetype=input.name,
                color=input.name),
            size=0.3,
            data=test.roc)+
  coord_equal()+
  geom_abline(slope=1, intercept=0, color="grey", size=0.3)+
  theme(legend.position="bottom")+
  ggtitle("Test ROC curves (4-fold cross-validation)")
pdf("figure-asthma-outcome.pdf", h=4, 10)
print(gg)
dev.off()


ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(. ~ input.name)+
  geom_point(aes(weight, variable.fac),
             data=all.points,
             shape=1)

viz <- list(
  title="L1-regularized logistic regression models for predicting asthma",
  auc=ggplot()+
    ggtitle("Test AUC, select test fold")+
    xlab("Test AUC")+
    ylab("Input feature set")+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    guides(color="none")+
    geom_point(aes(auc, input.name, clickSelects=test.fold,
                   showSelected=input.name,
                   color=input.name),
               alpha=0.7,
               size=5,
               data=test.dots),
  selector.types=list(input.name="multiple"),
  roc=ggplot()+
    ggtitle("ROC curves for selected test fold")+
    xlab("False Positive Rate = Prob(Asthma | Healthy)")+
    ylab("True Positive Rate = Prob(Asthma | Asthma)")+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    coord_equal()+
    geom_path(aes(FPR, TPR, color=input.name, group=input.name,
                  showSelected=test.fold),
              data=test.roc),
  weights=ggplot()+
    ggtitle("Non-zero weights in linear model, select test fold")+
    xlab("input feature weight / effect size")+
    ylab("input feature")+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(. ~ input.name)+
    guides(color="none")+
    theme_animint(height=1000, width=800)+
    geom_point(aes(
      weight, variable.fac,
      clickSelects=test.fold,
      showSelected=input.name,
      tooltip=sprintf(
        "%s selected in %d/4 folds, test fold %d weight=%.3f",
        variable,
        folds,
        test.fold,
        weight),
      color=input.name),
               size=5,
               data=all.points,
               alpha=0.7,
               shape=1))  
animint2dir(viz, "figure-asthma")
  
  
