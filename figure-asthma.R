source("packages.R")

load("test.error.RData")
load("glmnet.list.RData")
load("input.features.RData")
feature.sets$markers %in% glmnet.list$selected$variable

variable.counts <- glmnet.list$selected[, list(
  mean.weight=mean(weight),
  folds=.N), by=.(variable, output.name, input.name)]
setkey(variable.counts, variable, output.name, input.name)
sorted.counts <- variable.counts[order(-folds, -abs(mean.weight)),]
variable.counts[, variable.fac := factor(variable, rev(unique(paste(sorted.counts$variable))))]
stopifnot(all(!is.na(variable.counts$variable.fac)))
setkey(glmnet.list$selected, variable, output.name, input.name)
all.points <- glmnet.list$selected[variable.counts]

test.roc <- data.table(test.error$roc)[test.weights=="one", {
  FPR.grid <- seq(0, 1, l=101)
  TPR.grid <- approx(FPR, TPR, FPR.grid)$y
  data.table(
    FPR=FPR.grid,
    TPR=TPR.grid
    )
}, by=.(input.name, test.fold)]
test.dots <- data.table(test.error$error)[test.weights=="one",]

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(test.fold ~ .)+
  coord_equal()+
  geom_path(aes(FPR, TPR, color=input.name), data=test.roc)

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  geom_point(aes(auc, input.name, color=model.name),
             shape=1,
             data=test.dots)

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
  
  
