library(data.table)
library(ggplot2)
load("many.folds.RData")
##load("glmnet.list.RData")

marker.positions <- fread("marker_positions.tsv")
marker.positions[, discovery := ifelse(is.na(oldMissingNew1), "old", "new")]
library(namedCapture)
range.pattern <- paste0(
  "(?<megaStart>.*?)",
  "-",
  "(?<megaEnd>.*)")
mega.df <- str_match_named(marker.positions$megabases, range.pattern, list(megaStart=as.numeric, megaEnd=as.numeric))
marker.labels <- data.table(mega.df, marker.positions)
marker.labels[, long.label := sprintf("%s marker %2d chr%02d:%06.2f-%06.2f", marker, regionID, chromosome, megaStart, megaEnd)]

many.folds$selected[, variable.type := ifelse(grepl("rs", variable), "SNP", ifelse(grepl("sex", variable), "sex", "HLA"))]
variable.counts <- many.folds$selected[, list(
  folds=.N,
  mean.weight=mean(weight),
  sd.weight=sd(weight)
  ), by=.(variable, variable.type)][order(folds, abs(mean.weight)),]
levs <- variable.counts$variable
variable.counts[, variable.fac := factor(variable, levs)]
many.folds$selected[, variable.fac := factor(variable, levs)]
marker.labels[, variable.fac := factor(marker, levs)]

range.vec <- range(many.folds$selected$weight)
gg <- ggplot()+
  geom_segment(aes(-1, variable.fac,
                   xend=1, yend=variable.fac,
                   color=discovery),
             size=3,
               data=marker.labels)+
  scale_color_manual(values=c(new="grey90", old="white"))+
  geom_vline(xintercept=0)+
  geom_point(aes(weight, variable.fac, fill=variable.type),
             shape=21,
             data=many.folds$selected)+
  geom_point(aes(mean.weight, variable.fac, fill=variable.type),
             size=5,
             shape=21,
             data=variable.counts)+
  ## geom_segment(aes(mean.weight-sd.weight, variable.fac,
  ##                  xend=mean.weight+sd.weight, yend=variable.fac,
  ##                  fill=variable.type),
  ##              data=variable.counts)+
  coord_cartesian(xlim=c(-0.35, 0.15))+
  scale_x_continuous(
    "learned feature weight / effect size",
    breaks=c(range.vec, -0.1, 0.1, 0),
    labels=c(sprintf("%.3f", range.vec), "-0.1", "0.1", "0"))+
  geom_text(aes(-0.37, variable.fac, label=sprintf("chr%02d:%06.2f-%06.2f", chromosome, megaStart, megaEnd)),
            size=2.5,
            hjust=0,
            data=marker.labels)+
  geom_text(aes(mean.weight, variable.fac, label=folds),
            size=2.5,
            data=variable.counts)+
  scale_y_discrete(
    "input feature (ordered by number of times selected, and absolute value of mean weight)",
    drop=FALSE)+
  theme(legend.position=c(0.2,0.2))
print(gg)
png("figure-many-folds-color.png", w=5, h=11, res=200, units="in")
print(gg)
dev.off()

gg <- ggplot()+
  geom_point(aes(mean.weight, variable.fac),
             color="grey",
             size=5,
             data=variable.counts)+
  geom_point(aes(weight, variable.fac),
             color="grey",
             shape=1,
             data=many.folds$selected)+
  geom_segment(aes(mean.weight-sd.weight, variable.fac,
                   xend=mean.weight+sd.weight, yend=variable.fac),
               data=variable.counts,
               color="grey")+
  geom_text(aes(mean.weight, variable.fac, label=folds),
            size=3,
            data=variable.counts)+
  xlab("learned feature weight / effect size")+
  ylab("input feature")
print(gg)
png("figure-many-folds.png", w=5, h=10, res=200, units="in")
print(gg)
dev.off()
