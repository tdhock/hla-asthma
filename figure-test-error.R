source("packages.R")

load("test.error.RData")

library(data.table)
error.dt <- data.table(test.error$error)
error.dt[, prop.weighted.error := weighted.error/total.weight]
best.model.error <-
  error.dt[test.weights=="balanced" & model.name=="glmnet.weightBalanced.standardizeFALSE",]
output.error <- best.model.error[, list(
  mean.auc=mean(auc),
  min.auc=min(auc),
  sd.auc=sd(auc),
  mean.positive=mean(total.positive),
  sd.positive=sd(total.positive)
  ), by=.(output.name, model.name)]
(output.sorted <- output.error[order(-min.auc),])
error.dt[, output.fac := factor(output.name, output.sorted$output.name)]

error.melted <- melt(
  error.dt,
  id.vars=c(
    "test.fold", "input.name",
    "output.fac", "output.name",
    "model.name", "test.weights"),
  measure.vars=c("auc", "prop.weighted.error"))

##dput(RColorBrewer::brewer.pal(Inf, "Paired"))
model.colors <- 
  c(glmnet.weight1="#A6CEE3", #lite blue
    glmnet.weightBalanced="#1F78B4", #dark blue
    "#B2DF8A", #lite green
    "#33A02C", #dark green
    "#FB9A99", #pink
    glmnet.weightBalanced.standardizeFALSE="#E31A1C", #red
    "#FDBF6F", #lite orange
    "#FF7F00", #dark orange
    "#CAB2D6", #violet
    "#6A3D9A", #purple
    "#FFFF99", #yellow
    major.class="#B15928") #brown
model.sizes <- rep(1, length(model.colors))
names(model.sizes) <- names(model.colors)
model.sizes[["major.class"]] <- 0.5

random <- data.frame(auc=0.5, slope=1, intercept=0)
select.output <- ggplot()+
  ggtitle("select output disease")+
  geom_vline(aes(xintercept=auc),
             data=random,
             color="grey")+
  geom_point(aes(min.auc, mean.positive, color=model.name,
                 clickSelects=output.name),
             size=5,
             fill=NA,
             shape=21,
             data=output.error)+
  geom_point(aes(auc, total.positive, color=model.name,
                 showSelected=output.name,
                 clickSelects=test.fold),
             size=4,
             data=best.model.error)+
  scale_color_manual(values=model.colors)+
  guides(color="none")+
  ylab("Positive examples in test set")+
  xlab("Test AUC = Area Under the ROC Curve")
viz <- list(selectOutput=select.output)

test.roc <- data.table(test.error$roc)[test.weights=="one",]
test.roc[, test.fold.fac := factor(paste("test fold", test.fold), paste("test fold", 1:10))]
test.roc[, output.fac := factor(output.name, output.sorted$output.name)]
model.stats <-
  error.dt[test.weights=="one", list(mean.auc=mean(auc)), by=model.name]
model.sorted <- model.stats[order(-mean.auc),]
levs <- paste(model.sorted$model.name)
test.roc[, model.fac := factor(model.name, levs)]
error.dt[, model.fac := factor(model.name, levs)]
error.dt[, test.fold.fac := factor(paste("test fold", test.fold), paste("test fold", 1:10))]
error.melted[, model.fac := factor(model.name, rev(levs))]
roc.points <- error.dt[model.name=="glmnet.weightBalanced.standardizeFALSE" & test.weights=="balanced",]

gg.roc <- ggplot()+
  coord_equal()+
  scale_x_continuous(
    "False positive rate = Probability(diseased | healthy)",
    breaks=seq(0, 1, by=0.2))+
  scale_y_continuous(
    "True positive rate = Probability(diseased | diseased)",
    breaks=seq(0, 1, by=0.2))+
  scale_color_manual(values=model.colors)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  geom_path(aes(FPR, TPR, color=model.fac,
                group=model.fac,
                showSelected=test.fold,
                showSelected2=output.name),
            chunk_vars=c("test.fold", "output.name"),
            data=test.roc)+
  geom_point(aes(FPR, TPR, color=model.fac,
                showSelected=test.fold,
                showSelected2=output.name),
             shape=21,
             size=4,
             fill="white",
             data=roc.points)

viz$ROC <- gg.roc+
  ggtitle("ROC curves for selected test set")+
  geom_abline(aes(slope=slope, intercept=intercept),
              data=random,
              color="grey")+
  geom_text(aes(
    1, 0.3, label=test.fold.fac,
    showSelected=test.fold,
    showSelected2=output.name,
    color=model.fac),
            data=roc.points,
            hjust=1)+
  geom_text(aes(
    1, 0.2, label=output.name,
    showSelected=test.fold,
    showSelected2=output.name,
    color=model.fac),
            data=roc.points,
            hjust=1)+
  geom_text(aes(
    1, 0.1, label=sprintf(
            "%d FP / %d healthy",
            FP, total.negative),
    showSelected=test.fold,
    showSelected2=output.name,
    color=model.fac),
            data=roc.points,
            hjust=1)+
  geom_text(aes(
    1, 0, label=sprintf(
            "%d FN / %d diseased",
            FN, total.positive),
    showSelected=test.fold,
    showSelected2=output.name,
    color=model.fac),
            data=roc.points,
            hjust=1)

viz$error <- ggplot()+
  ggtitle("select test fold")+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  theme_animint(width=800, height=200)+
  scale_color_manual(values=model.colors)+
  guides(color="none")+
  facet_grid(. ~ variable, scales="free")+
  xlab("")+
  geom_vline(aes(xintercept=auc),
             data=random,
             color="grey")+
  geom_point(aes(value, model.fac, color=model.fac,
                 clickSelects=test.fold,
                 showSelected=output.name),
             shape=1,
             size=4,
             data=error.melted[test.weights=="balanced",])

animint2dir(viz, "figure-test-error")

png("figure-test-error-roc.png", h=800, w=3800)
print(gg.roc+
  geom_text(aes(
    1, 0, label=sprintf(
            "%d FP / %d healthy\n%d FN / %d diseased",
            FP, total.negative, FN, total.positive),
    showSelected=test.fold,
    showSelected2=output.name,
    color=model.fac),
            size=4,
            data=roc.points,
            hjust=1,
            vjust=0)+
  facet_grid(test.fold.fac ~ output.fac, labeller=function(var, val){
    if(var=="output.fac"){
      gsub("[ /]", "\n", val)
    }else{
      paste(val)
    }
  }))        
dev.off()

gg.error <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  scale_color_manual(values=model.colors)+
  guides(color="none")+
  facet_grid(output.fac ~ variable + test.weights, scales="free", labeller=label_both)+
  geom_point(aes(value, model.fac, color=model.fac),
             shape=1,
             data=error.melted[test.weights=="balanced",])

pdf("figure-test-error.pdf", h=3, w=10)
print(gg.error)
dev.off()
png("figure-test-error.png", w=600, h=800)
print(gg.error)
dev.off()
