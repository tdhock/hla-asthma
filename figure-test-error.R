source("packages.R")

load("test.error.RData")

library(data.table)
error.dt <- data.table(test.error$error)
error.dt[, prop.weighted.error := weighted.error/total.weight]
error.melted <- melt(
  error.dt,
  id.vars=c(
    "test.fold", "input.name", "output.name", "model.name", "test.weights"),
  measure.vars=c("auc", "prop.weighted.error"))

##dput(RColorBrewer::brewer.pal(Inf, "Paired"))
model.colors <- 
  c(glmnet.weight1="#A6CEE3", #lite blue
    glmnet.weightBalanced="#1F78B4", #dark blue
    "#B2DF8A", #lite green
    "#33A02C", #dark green
    "#FB9A99", #pink
    "#E31A1C", #red
    "#FDBF6F", #lite orange
    "#FF7F00", #dark orange
    "#CAB2D6", #violet
    "#6A3D9A", #purple
    "#FFFF99", #yellow
    major.class="#B15928") #brown
model.sizes <- rep(1, length(model.colors))
names(model.sizes) <- names(model.colors)
model.sizes[["major.class"]] <- 0.5

test.roc <- data.table(test.error$roc)[test.weights=="one",]
test.roc[, facet := factor(paste("test fold", test.fold), paste("test fold", 1:10))]
model.stats <-
  error.dt[test.weights=="one", list(mean.auc=mean(auc)), by=model.name]
model.sorted <- model.stats[order(-mean.auc),]
levs <- paste(model.sorted$model.name)
test.roc[, model.fac := factor(model.name, levs)]
error.melted[, model.fac := factor(model.name, rev(levs))]
gg.roc <- ggplot()+
  coord_equal()+
  scale_x_continuous(
    "False positive rate = Probability(Asthma | Healthy)",
    breaks=seq(0, 1, by=0.2))+
  scale_y_continuous(
    "True positive rate = Probability(Asthma | Asthma)",
    breaks=seq(0, 1, by=0.2))+
  scale_color_manual(values=model.colors)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_wrap("facet")+
  geom_path(aes(FPR, TPR, color=model.fac),
            data=test.roc)
png("figure-test-error-roc.png", h=800, w=800)
print(gg.roc)
dev.off()

gg.error <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  scale_color_manual(values=model.colors)+
  guides(color="none")+
  facet_grid(. ~ variable + test.weights, scales="free", labeller=label_both)+
  geom_point(aes(value, model.fac, color=model.fac),
             shape=1,
             data=error.melted)
pdf("figure-test-error.pdf", h=3, w=10)
print(gg.error)
dev.off()
png("figure-test-error.png", w=800)
print(gg.error)
dev.off()
