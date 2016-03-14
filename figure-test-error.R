works_with_R("3.2.3", ggplot2="1.0.1")

load("test.error.RData")

library(data.table)
error.dt <- data.table(test.error$error)
error.dt[, prop.weighted.error := weighted.error/total.weight]
error.melted <- melt(
  error.dt,
  id.vars=c("test.fold", "model", "test.weights"),
  measure.vars=c("auc", "prop.weighted.error"))

##dput(RColorBrewer::brewer.pal(Inf, "Paired"))
model.colors <- 
  c(glmnet.one="#A6CEE3", #lite blue
    glmnet.balanced="#1F78B4", #dark blue
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
model.sizes[["major.class"]] <- 2

test.roc <- data.table(test.error$roc)[test.weights=="one",]
test.roc[, facet := factor(paste("test fold", test.fold), paste("test fold", 1:10))]
gg.roc <- ggplot()+
  scale_color_manual(values=model.colors)+
  scale_size_manual(values=model.sizes)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_wrap("facet")+
  geom_path(aes(FPR, TPR, color=model, size=model),
            data=test.roc)
png("figure-test-error-roc.png")
print(gg.roc)
dev.off()

gg.error <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  scale_color_manual(values=model.colors)+
  facet_grid(. ~ variable + test.weights, scales="free", labeller=label_both)+
  geom_point(aes(value, model, color=model),
             shape=1,
             data=error.melted)
png("figure-test-error.png")
print(gg.error)
dev.off()

