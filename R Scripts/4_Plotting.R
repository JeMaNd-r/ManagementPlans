#- - - - - - - - - - - - - - - - -#
# Text mining of management plans #
#     author: Romy Zeiss          #
#                                 #
#        4. Plotting              #
#- - - - - - - - - - - - - - - - -#

## Assign working directories ####
folder.wd; work.wd; figu.wd
setwd(work.wd); getwd()

# Load packages
library(influential)
library(igraph)
library(tidyverse)
library(reshape2)
library(ggraph)
library(wordcloud)

# Check objects
years; decades
yr.deca
no.doc
categories

#- - - - - - - - - - - - - - - - - - - - - - -
## Load co-occurrence MATRIX and as dataframe
setwd(folder.wd)

load(file=paste0("CooccurMatrix_subcate_total_mean.RData"))   #com
#load(file=paste0("CooccurMatrix_subcate_total_tfidf.RData")) #com with tfidf
load(file=paste0("CooccurDF_subcate_total.RData"))     #occur.total
load(file=paste0("CooccurDF_subcate_perDecade.RData")) #occur.deca
#load(file=paste0("CooccurDF_subcate_total_all0asNA.RData"))     #occur.total
#load(file=paste0("CooccurDF_subcate_perDecade_all0asNA.RData")) #occur.deca

# load template for network layout
setwd(work.wd)
template.layout <- read.csv("../Network_template.csv")

# define colors for each category
barcolors <- c("forestgreen", "tan2", "burlywood4", "mediumpurple3", 
               "deepskyblue3", "yellow3", "orangered3")

# asign colors for labels
text.color <- c(categories$subcategory); length(text.color)
text.color[1:4] <- "forestgreen"     #color for biodiversity
text.color[5:9] <- "tan2"           #color for drivers
text.color[10:15] <- "burlywood4"    #color for features
text.color[15:22] <- "mediumpurple3" #functions
text.color[23:26] <- "deepskyblue3"  #protection
text.color[27:29] <- "yellow3"       #soil
text.color[30:41] <- "orangered3"    #threat

#- - - - - - - - - - - - - - - - - - - - - - -
## extract word occurrences (i.e. diagonal of matrix) ####
#word.occ <- occur.total[occur.total$subcategory==occur.total$word2, ]
word.occ <- occur.deca[occur.deca$subcategory==occur.deca$word2,]

# add categories
word.occ <- merge(word.occ, categories, by="subcategory")

d <- word.occ

# Change order of labels by asigning factor levels
slevels <- categories$subcategory  # define order (by categories)
d$subcategory <- factor(d$subcategory, levels = rev(slevels))

## bar plot: total subcategories
#setwd(figu.wd); pdf("Barplot_subcategories_mean.pdf", width=10)
ggplot(data=d, aes(mean, subcategory, fill=category)) +
  geom_bar(stat="identity", show.legend = T) +
  scale_fill_manual(values=barcolors) +
  #facet_grid(cols =vars(deca))+ scale_y_discrete(labels=NULL) 
  theme(text = element_text(size = 20)) 
dev.off()

## word clouds
temp.data <- occur.total[occur.total$subcategory==occur.total$word2,]
temp.data <- merge(temp.data, categories, by="subcategory")
set.seed(1230)
setwd(figu.wd)
pdf(file=("Wordcloud_mean_total.pdf"))
wordcloud(words = temp.data$subcategory, freq = temp.data$mean, min.freq = 0,
          max.words=100, random.order=FALSE, rot.per=0.25,random.color = FALSE,
          colors=barcolors[factor(temp.data$category)], ordered.colors=TRUE)
dev.off()
setwd(work.wd)

for(i in 1:length(decades)){
  deca <- decades[i]
  temp.data <- word.occ[word.occ$deca==deca,]
  
  set.seed(1234)
  setwd(figu.wd)
  pdf(file=paste0("Wordcloud_mean_d", deca, ".pdf"))
  wordcloud(words = temp.data$subcategory, freq = temp.data$mean, min.freq = 1,
            max.words=100, random.order=FALSE, rot.per=0.35,random.color = FALSE,
            colors=barcolors[factor(temp.data$category)], ordered.colors=TRUE)
  dev.off()

  pdf(file=paste0("Wordcloud_df.n_d", deca, ".pdf"))
  wordcloud(words = temp.data$subcategory, freq = temp.data$df.n, min.freq = 0.2,
            max.words=100, random.order=FALSE, rot.per=0.35,random.color = FALSE,
            colors=barcolors[factor(temp.data$category)], ordered.colors=TRUE)
  dev.off()
  setwd(work.wd)
}

## pie chart: categories & subcategories
ggplot(data=d, aes("", mean, fill=category)) +
  geom_bar(stat="identity", width=1, color="white") +
  #facet_wrap(~category)+
  coord_polar("y", start=0)+
  scale_fill_manual(values=barcolors)

d <- d[order(d$category),]
pie(d$mean, labels=c(as.character(d$label)), col=text.color)

d <- aggregate(mean~category, data=word.occ[word.occ$subcategory==word.occ$word2,], sum)
d
# note: we have to take the occur.total data frame here to get the mean per category


## bar plot: categories
#setwd(figu.wd); pdf("Barplot_categories_mean.pdf", width=10)
ggplot(data=d, aes(mean, category, fill=category)) +
  geom_bar(stat="identity", show.legend = T) +
  scale_fill_manual(values=barcolors) +
  #facet_grid(cols =vars(deca))+ scale_y_discrete(labels=NULL) +
  theme(text = element_text(size = 20)) 
dev.off()

# ## estimate degree of association: Dice coefficient based on review (Evert 2005)
# # check out http://www.collocations.de/AM/
# # formula: Dice = (2*O)/(R+C), with
# # O... observed joint frequency of term 1 and 2
# # R... marginal frequency, number of tokens where term 1 is present
# # C... marginal frequency, number of tokens where term 2 is present
# occur.total$dice <- c()
# for(i in 1:nrow(occur.total)){
#   #temp.row <- occur.total[i,]
#   term1 <- occur.total[i,]$subcategory
#   term2 <- occur.total[i,]$word2
#   occur.total[i, "dice"] <- (2*occur.total[i,"presence"])/
#     (word.occ[word.occ$subcategory==term1,"presence"] + word.occ[word.occ$subcategory==term2, "presence"])
# }
# 
# ## estimate degree of association: Jaccard coefficient based on review (Evert 2005)
# # formula: Dice = (O11)/(O11+O12+O21), with
# # O11... observed joint frequency of term 1 and 2
# # O12... observed frequency: number of chunks where term 1 but not 2 is present
# # O21... observed frequency: number of chunks where term 2 but not 1 is present
# #...

#- - - - - - - - - - - - - - - - - - - - - - -
## Investigate data ####
d <- occur.total[occur.total$subcategory!=occur.total$word2,]

d[order(d$mean, decreasing = T),c("subcategory", "word2", "mean")]
d[order(d$tf, decreasing = T),c("subcategory", "word2", "tf", "mean")]
d[order(d$df, decreasing = T),c("subcategory", "word2", "df", "mean")]
d[order(d$df.n, decreasing = T),c("subcategory", "word2", "df.n", "mean")]

d <- occur.total[occur.total$subcategory==occur.total$word2,]
summary(d$df.n, na.rm=T)
summary(d$tf, na.rm=T)

d.colsum <- as.numeric(colSums(d[,c(3:3508)], na.rm=T))
hist(d.colsum); summary(d.colsum); length(d.colsum[d.colsum==0])

# number of terms mentioned per document 
d.colno <- as.numeric(colSums(d[,c(3:3508)] >= 1, na.rm=T))
hist(d.colno); summary(d.colno); length(d.colno[d.colno==41])

# number of documents mentioning "environment" or "habitat"
d.colno <- as.numeric(colSums(d[d$subcategory=="environment",c(3:3508)] >= 1, na.rm=T))
hist(d.colno); summary(d.colno); length(d.colno[d.colno==41])

#- - - - - - - - - - - - - - - - - - - - - - -
## Save summary of occurrences ####
# extract relevant columns from analysis, and save data
occur.data <- occur.deca[,c("subcategory", "word2", "deca", "mean", "tf", "df", "df.n")]
#setwd(folder.wd); write.csv(occur.data, "Occurrence_summary_perDecade.csv", row.names = F)

occur.data <- occur.total[,c("subcategory", "word2", "mean", "tf", "df", "df.n")]
View(occur.data[occur.data$subcategory!=occur.data$word2,])

x <- occur.deca[,c("subcategory", "word2", "deca", "mean", "tf", "df", "df.n")]
occur.data <- merge(occur.data, pivot_wider(data = x, 
            id_cols = c(subcategory, word2), 
            names_from = deca, 
            values_from = c("mean", "tf", "df", "df.n")))

occur.data <- occur.data[order(occur.data$subcategory),]
head(occur.data)

# setwd(folder.wd); write.csv(occur.data[occur.data$subcategory==occur.data$word2,],
#                             "Occurrence_summary_total.csv", row.names = F)

occur.data <- merge(occur.data, categories)

occur.cate <- aggregate(.~category, data=occur.data[,3:19], sum)
setwd(folder.wd); write.csv(occur.cate,"Occurrence_summary_total_perCategory.csv", row.names = F)


#- - - - - - - - - - - - - - - - - - - - - - -
## Plot co-occurrence MATRIX with corrplot ####

# order matrix columns and rows by categories
ordering <- categories$subcategory
com <- com[,ordering] #order colnames
com <- com[ordering,] #order rownames

# asign colors for labels
text.color <- c(rownames(com)); length(text.color)
text.color[1:4] <- "forestgreen"     #color for biodiversity
text.color[5:9] <- "tan2"           #color for drivers
text.color[10:15] <- "burlywood4"    #color for features
text.color[15:22] <- "mediumpurple3" #functions
text.color[23:26] <- "deepskyblue3"  #protection
text.color[27:29] <- "yellow3"       #soil
text.color[30:41] <- "orangered3"    #threat

# asign colors for background
bgcolor <- matrix("white", nrow(com), ncol(com),dimnames = dimnames(com))
bgcolor[,1:4] <- "forestgreen"     #color for biodiversity
bgcolor[,5:9] <- "tan2"           #color for drivers
bgcolor[,10:15] <- "burlywood4"    #color for features
bgcolor[,15:22] <- "mediumpurple3" #functions
bgcolor[,23:26] <- "deepskyblue3"  #protection
bgcolor[,27:29] <- "yellow3"       #soil
bgcolor[,30:41] <- "orangered3"    #threat
bgcolor <- bgcolor[upper.tri(bgcolor, diag=TRUE)]

#setwd(figu.wd); pdf("Corrplot_subcategories_total_mean.pdf")
corrplot::corrplot(com, is.corr=F, diag=T, order = "original", 
                   tl.col = text.color, cl.pos="n", col="white", tl.pos = "td", 
                   tl.cex = 0.5, method = "circle", type = "upper", bg=bgcolor)
dev.off()

#- - - - - - - - - - - - - - - - - - - - - - -
## Plot single-word occurrences (mean term frequencies) ####
## bar plot of subcategories
d <- word.occ

# Change order of labels by asigning factor levels
slevels <- categories$subcategory  # define order (by categories)
d$subcategory <- factor(d$subcategory, levels = rev(slevels))

## bar plot: subcategories
#setwd(figu.wd); pdf("Barplot_subcategories_median.pdf")
#setwd(figu.wd); pdf("Barplot_subcategories_perDecade_mean.pdf", width=10)
ggplot(data=d, aes(mean, subcategory, fill=category)) +
  geom_col(show.legend = T) +
  facet_grid(cols=vars(deca)) +
  scale_fill_manual(values=barcolors)
dev.off()

## bar plot: categories per decade
#setwd(figu.wd); pdf("Barplot_categories_perDecade_mean.pdf", width=10)
ggplot(data=d, aes(mean, category, fill=category)) +
  geom_bar(stat="identity", show.legend = T) +
  scale_fill_manual(values=barcolors) +
  #facet_grid(cols =vars(deca))+ scale_y_discrete(labels=NULL) +
  theme(text = element_text(size = 20)) 
dev.off()


#- - - - - - - - - - - - - - - - - - - - - - -
## boxplot
d2 <- melt(d[,c(1, 3519, 3:3507)], id.vars = c("subcategory", "category", "deca"))

#setwd(figu.wd); pdf("Boxplot_subcategories_perDecade_noOutlier.pdf", width=10)
ggplot(data=d2, aes(value, fill=category)) +
  geom_histogram() +
  # geom_boxplot(na.rm=T, show.legend = T, 
  #              outlier.shape=NA) +
  #              #outlier.shape=1, outlier.alpha = 0.1) +
  coord_flip() +
  facet_grid(cols=vars(deca)) + 
  scale_fill_manual(values=barcolors) +
  #scale_color_manual(values=barcolors) +
  scale_y_continuous(limits=c(0,27))
dev.off()

d$deca <- as.character(d$deca)
d2 <- melt(d[,c(1, 3, 3519, 4:3508)], na.rm=T)
d2[is.na(d2)] <- 0
#setwd(figu.wd); pdf("Boxplot_categories_perDecade.pdf", width=10)
ggplot(data=d2, aes(category, value, fill=category, color=deca)) +
  geom_boxplot(na.rm=T, show.legend = T, 
               outlier.shape=NA,
               #outlier.shape=1, outlier.alpha = 0.1,
               position=position_dodge(1)) +
  #coord_flip() +
  #facet_grid(cols=vars(deca)) + 
  scale_fill_manual(values=barcolors) +
  scale_color_manual(values=rep("black", 3)) +
  scale_y_continuous(limits=c(0,8)) #check why different plots (crop...)
dev.off()

#setwd(figu.wd); pdf("Boxplot_categories.pdf", width=10)
ggplot(data=d2, aes(category, value, fill=category)) +
  geom_boxplot(na.rm=T, show.legend = T, outlier.shape=NA) +
  #coord_flip() +
  scale_fill_manual(values=barcolors) +
  #scale_color_manual(values=rep("black", 3)) +
  scale_y_continuous(limits=c(0,8)) #check why different plots (crop...)
dev.off()

#- - - - - - - - - - - - - - - - - - - - - - -
## effect size compared to 2010 as baseline ####
d1 <- word.occ[word.occ$deca==2010,]
d1 <- d1[order(d1$subcategory),]

d2 <- word.occ[word.occ$deca==2015,]
d2 <- d2[order(d2$subcategory),]
d3 <- word.occ[word.occ$deca==2020,]
d3 <- d3[order(d3$subcategory),]

d <- data.frame(label=d1$subcategory, deca=sort(rep(c(2015, 2020), 41)),
                eff.mean=NA, eff.low=NA, eff.upp=NA) #, eff.d=NA)

d <- merge(d, categories, by.x="label", by.y="subcategory")

# calculate effect size as Cohen's d = (mean1 - mean2) / sd
# based on https://www.statology.org/effect-size/
d[d$deca==2015,]$eff.mean <- (d2$mean - d1$mean)/ mean(sd(d2$mean), sd(d1$mean))
d[d$deca==2020,]$eff.mean <- (d3$mean - d1$mean)/ mean(sd(d3$mean), sd(d1$mean))

# # make t-test to check difference
# d$ttest <- NA
# d <- d[order(d$deca),]
# for(i in 1:nrow(d1)){
#   try({
#   temp.1 <- as.numeric(d1[i,4:3508]); temp.1[is.na(temp.1)]<- 0
#   temp.2 <- as.numeric(d2[i,4:3508]); temp.2[is.na(temp.2)] <- 0
#   temp.3 <- as.numeric(d3[i,4:3508]); temp.3[is.na(temp.3)] <- 0
#   d[i,]$ttest <- as.numeric(t.test(temp.2, temp.1)["statistic"])
#   d[i+nrow(d1),]$ttest <- as.numeric(t.test(temp.3, temp.1)["statistic"])
# })}

# get confidence intervals
library(psych)
# # calculate Cohen's d from t statistic
# d[d$deca==2015,]$eff.d <- t2d(d[d$deca==2015,]$ttest, n=(d2$df+d1$df), n2=d2$df, n1=d1$df)
# d[d$deca==2020,]$eff.d <- t2d(d[d$deca==2020,]$ttest, n=(d3$df+d1$df), n2=d3$df, n1=d1$df)

# # or by hand with pooled/ averaged sd (exactly the same)
# d[d$deca==2015,]$eff.d <- d[d$deca==2015,]$ttest * sqrt( (1/d1$df) + (1/d2$df) )
# d[d$deca==2020,]$eff.d <- d[d$deca==2020,]$ttest * sqrt( (1/d1$df) + (1/d3$df) )

# # correction for bias: Hedge's g
# d$eff.g <- NA
# d[d$deca==2015,]$eff.g <- d[d$deca==2015,]$eff.d * (1 - (3/(4*d1$df + d2$df)-9))
# d[d$deca==2020,]$eff.g <- d[d$deca==2020,]$eff.d * (1 - (3/(4*d1$df + d3$df)-9))

d[d$deca==2015,]$eff.low <- cohen.d.ci(d[d$deca==2015,]$eff.mean, 
                                       n=(d2$df+d1$df), n2=d2$df, n1=d1$df)[,1]
d[d$deca==2015,]$eff.upp <- cohen.d.ci(d[d$deca==2015,]$eff.mean, 
                                       n=(d2$df+d1$df), n2=d2$df, n1=d1$df)[,3]

d[d$deca==2020,]$eff.low <- cohen.d.ci(d[d$deca==2020,]$eff.mean, 
                                       n=(d3$df+d1$df), n2=d3$df, n1=d1$df)[,1]
d[d$deca==2020,]$eff.upp <- cohen.d.ci(d[d$deca==2020,]$eff.mean, 
                                       n=(d3$df+d1$df), n2=d3$df, n1=d1$df)[,3]

## Save effect sizes
#setwd(folder.wd); write.csv(d, "Effectsize_decades_withCI.csv", row.names = F)

## get confidence intervals corrected for small sample size
#library(MBESS)
#conf.limits.ncf(F.value = 7, conf.level = 0.95, df.1 <- 4, df.2 <- 50)

## or based on Goulet-Pelletier & Cousineau 2018
#ci.manual <- function(d, n1, n2){}
# noncentrality parameter lambda for two independent groups design: 
# lambda = d * sqrt(n/2), with
# d... effect size (e.g. Cohen'sd)
# n... harmonic mean of both n1 and n2
#psych::harmonic.mean

# calculate confidence intervals:
# CI(lambda) = [tL = tv, lambda(0.025), tU=tv, lambda(0.975)]

# transform back into a CI:
# CId = [ dL=tL/(lambda/d), dU=tU/(lambda/d) ]
# where square brackets denote the extremity of the interval, 
# tL ... lower limit of the lambda-interval and tU ... upper limit

# replace confidence interval of subcategories with 0 if co-occurring in less than 10 % of documents
d[d$label %in% word.occ[word.occ$df.n<=0.1, "subcategory"],]$eff.low <- 0
d[d$label %in% word.occ[word.occ$df.n<=0.1, "subcategory"],]$eff.upp <- 0

# change factor level ordering for labels after coord_flip()
d$label <- factor(d$label, levels=(categories$subcategory))
#d$label2 <- d$label %>% stringi::stri_trans_totitle()

doc.freq <- occur.total[occur.total$subcategory==occur.total$word2 & !is.na(occur.total$subcategory),]$df
doc.freq.n <- round(occur.total[occur.total$subcategory==occur.total$word2 & !is.na(occur.total$subcategory),]$df.n,2)

symbol.size <- occur.total[occur.total$subcategory==occur.total$word2,"mean"]
symbol.size <- symbol.size[!is.na(symbol.size)]

setwd(figu.wd); pdf("Effect_mean_to2010_CohensD_ci_df.n0.1to0_vertical.pdf", height=15)
ggplot(data=d, aes(x=reorder(stringi::stri_trans_totitle(label), desc(label)), y=eff.mean, ymin=eff.low, ymax=eff.upp,
                   color=stringi::stri_trans_totitle(category), shape=as.factor(deca), linetype=as.factor(deca))) +
  geom_pointrange(position=position_dodge(width=0.7)) + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  #geom_text(aes(y=-2.25, label=rep(doc.freq.n, each=2)), cex=4, hjust=0, angle=45) +
  geom_text(aes(y=0.7, label=rep(categories[order(categories$subcategory),]$label, each=2)), cex=4.5, angle=0) +
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab(NULL) + ylab("Cohens d of mean term frequency to 2010 baseline") +
  scale_color_manual(values=barcolors) +
  scale_x_discrete(position="top")+
  scale_shape_manual(values=c(1, 17)) +
  scale_linetype_manual(values=c("solid", "longdash"))+
  theme_bw() + # use a white background
  theme(axis.text.x = element_text(angle = 0, hjust=0, size=15, colour="black"), 
        axis.text.y = element_text(size=15, colour="black"),
        legend.position = "bottom", plot.margin = unit(c(1,2,1,1),"cm")) 
dev.off()

# again, bar plot: subcategories
d4 <- word.occ[word.occ$deca==2010,]
# Change order of labels by asigning factor levels
slevels <- categories$subcategory  # define order (by categories)
d4$subcategory <- factor(d4$subcategory, levels = rev(slevels))
a <- ggplot(data=d4, aes(mean, subcategory, fill=category)) +
  geom_col(show.legend = F) +
  #facet_grid(cols=vars(deca)) +
  scale_fill_manual(values=barcolors)
a

# combine into 1 plot
#setwd(figu.wd); pdf("Effect_mean_to2010_CohensD_ci_with2010_df.n.pdf", width=15)
ggpubr::ggarrange(a,b)
dev.off()

#- - - - - - - - - - - - - - - - - - - - - - -
## pointrange plot with mean (and sd) ####
d <- word.occ
slevels <- categories$subcategory  # define order (by categories)
d$subcategory <- factor(d$subcategory, levels = rev(slevels))

#d[d$df.n>=0.1,]$mean.sd <- apply(d[d$df.n>=0.1,4:3509], 1, sd, na.rm=T)

#setwd(figu.wd); pdf("Effect_mean_decade.pdf", height=12)
ggplot(data=d, aes(x=subcategory, y=mean, ymin=(mean), ymax=(mean), 
                        fill=as.factor(deca), shape=as.factor(deca), linetype=as.factor(deca))) +
  geom_bar(position=position_dodge(width=0.6), stat="identity") + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  #geom_text(aes(y=rep(c(0.7,1.1),48), label=c(rbind(round(d2$df.n,2), round(d3$df.n,2)))), cex=2) +
  #geom_text(aes(y=-2.3, label=rep(round(d1$df.n,2), each=2)), cex=2) +
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Mean term frequency per decade") +
  #scale_fill_manual(values=barcolors) +
  scale_fill_manual(values=c("orange", "dodgerblue2", "darkred"))+
  scale_shape_manual(values=c(17, 1, 0)) +
  scale_linetype_manual(values=c("solid", "longdash", "dotted"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 18))+
  theme_bw()+
  theme(legend.position=c(0.85,0.1))# use a white background
dev.off()


#- - - - - - - - - - - - - - - - - - - - - - -
## 2D Plot of tf and df ####
d <- word.occ

plot(d$mean ~ d$df)

#setwd(figu.wd); pdf("2Dplot_subcategories_mean.pdf", width=10)
ggplot(data=d, aes(category, x=mean, y=df, color=category)) +
  geom_point(cex=2) +
  #coord_flip() +
  scale_color_manual(values=barcolors) +
  geom_text(label=d$subcategory, cex=2.5, nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T)
  #scale_color_manual(values=rep("black", 3)) +
  theme()
dev.off()

#- - - - - - - - - - - - - - - - - - - - - - -
## Plot NETWORK per decade ####
d <- occur.deca[!is.na(occur.deca$subcategory),]
d <- d[order(d$df.n),c("subcategory", "word2","deca", "df.n", "mean")]

# remove double links if only word order is changed
d$bigram <- apply(cbind(d$subcategory, d$word2),1, function(x) paste(sort(x), collapse=" "))
d <- d %>% separate(bigram, c("subcategory", "word2"), " ")
d <- unique(d)

# create df.n as categorical values
d$df.n.f <- as.numeric(d$df.n)
d[d$df.n<0.2,]$df.n.f <- 0
d[d$df.n>=0.2 & d$df.n<0.4,]$df.n.f <- 0.2
d[d$df.n>=0.4 & d$df.n<0.6,]$df.n.f <- 0.4
d[d$df.n>=0.6,]$df.n.f <- 0.6
d$df.n.f <- as.factor(d$df.n.f)

# centrality <- data.frame(subcate=NA, deca=NA)
# for(deca in decades){
#   temp.g <- d[d$deca==deca,] %>%  igraph::graph_from_data_frame()
# 
#   # calculate centrality
#   temp.c <- as.matrix(closeness(temp.g, normalized = T))
#   temp.c <- as.data.frame(temp.c[order(rownames(temp.c)),])
#   temp.c$deca <- deca; temp.c$centr <- temp.c$`temp.c[order(rownames(temp.c)), ]`
#   temp.c$subcate <- rownames(temp.c)
#   
#   centrality <- full_join(centrality, temp.c[,2:4])
# }
# 
# # add row(s) where subcategory is missing
# table(centrality$subcate) #here only 2x consumption
# centrality[centrality$subcate=="consumption",] #check which year is missing
# 
# centrality <- centrality[order(centrality$subcate),]
# centrality <- centrality[order(centrality$deca),]
# head(centrality)

# assign data frame as network
g <- d[d$subcategory!=d$word2,] %>% select(subcategory, word2, deca, df.n.f) %>% igraph::graph_from_data_frame()
#g <- d[d$subcategory!=d$word2,] %>% igraph::graph_from_data_frame()

#V(g)$name <- sort(V(g)$name)
V(g)$label <- categories[order(match(categories$subcategory, V(g)$name)),]$label

# define node size
deca.occ <- occur.deca[occur.deca$subcategory==occur.deca$word2,]
deca.occ <- deca.occ[order(match(deca.occ$subcategory, V(g)$name)),]
node.size <- na.omit(deca.occ[order(deca.occ$deca),]$df.n) #mean, or df.n+10

## add categories to graph data
categories <- categories[order(match(categories$subcategory, V(g)$name)),]
V(g)$category <- as.character(categories[categories$subcategory %in% V(g)$name,]$category)
#V(g)$category <- as.character(categories$category)
categories <- categories[order(categories$category),]

#V(g)$centrality <- centrality$centr
#V(graphNetwork)$size <- scales::rescale(log(degree(graphNetwork)), to = c(5, 15))

# # define categories for mean
# E(g)$mean.f <- as.numeric(cut(E(g)$mean, 4))

# delete edges lower than lim.set (set before to 0)
summary(d$mean)
summary(d$df.n.f)
summary(d$df.n)
#quantile(d$df.n, c(0.25, 0.5, 0.75,0.95))
#summary(d$tf.df)

#lim.set <- median(d$mean)
lim.set <- 0.1 # 0.153 = 3rd Quartile of mean, 0.078 (3rd Qu.) or 0.0178 (median) for df.n, 
# note: normalization has been necessary as df influenced by total df per decade

g <- delete.edges(g, which(E(g)$df.n.f<lim.set))
#g <- delete.edges(g, which(E(g)$mean.f<=1))


## plot network

# labels for decades
lab.deca <- c("2001 - 2010", "2011 - 2015", "2016 - 2020")
names(lab.deca) <- c("2010", "2015", "2020")

# adapt position in network according to order of labels
template.layout <- template.layout[order(match(template.layout$label, V(g)$label)),]

#setwd(figu.wd); pdf("Word_network_cooccur_perDecade_layoutkk.pdf", width=15, height=15)
setwd(figu.wd); pdf(paste0("Word_network_cooccur_lim", round(lim.set,2), "_perDecade_df.n.f_labs_df.n.pdf"), height=15, width=15)

#ggraph(g, layout =  "kk") +   # you may use "grid", "sphere", "lgl", "kk"
ggraph(g, layout =  as.matrix(template.layout[,1:2])) +
  #geom_edge_link()+   # to draw all links
  #geom_edge_bend(aes(edge_alpha = weight, edge_width=weight), strength=0.2, edge_color = "grey") +
  geom_edge_link(aes(edge_width=E(g)$df.n.f, 
                     edge_alpha=E(g)$df.n.f), #edge_color="grey45", #grey45
                 show.legend = T)+
  geom_node_point( aes(size = node.size,col=rep(V(g)$category, 3)), show.legend = T) +
  scale_size(range=c(10, 20))+
  #geom_node_text(aes(label = name), size=2, repel = TRUE) +
  geom_node_text(aes(label = label), cex=7)+#, col=rep(V(g)$category, 3)), repel = TRUE) +
  facet_edges(~deca, labeller = labeller(.cols=lab.deca), nrow=2, ncol=2) +
  scale_color_manual(values=barcolors) +
  #scale_edge_color_grey() +
  #scale_edge_width_continuous(breaks=waiver(),n.breaks = 4)+  #breaks for (factoral) mean
  #scale_edge_alpha_continuous(breaks=waiver(),n.breaks = 4)+
  #scale_edge_color_manual(values=c("grey95", "grey", "grey65")) +
  #scale_edge_width_manual(values=c(0,1,2)) +
  theme_void() +
  theme(strip.text.x = element_text(size=20), plot.margin=unit(c(1,1,1,1),"cm"), 
        legend.text = element_text(size=20), legend.position = c(0.8,0.2))

  # to change boxes around facets:
  #theme(strip.background = element_rect(fill=alpha("blue",0.2) , size=1.5, linetype="solid"))
dev.off()

# plot for node legend
x <- ggraph(g, layout =  as.matrix(template.layout[,1:2])) +
  geom_edge_link(aes(edge_width=E(g)$df.n.f, 
                     edge_alpha=E(g)$df.n.f), #edge_color="grey45", #grey45
                 show.legend = F)+
  geom_node_point(aes(size = node.size,col=rep(V(g)$category, 3)), show.legend=T)+
  facet_edges(~deca, labeller = labeller(.cols=lab.deca), nrow=2, ncol=2) +
  scale_color_manual(values=barcolors)
  
legend <- cowplot::get_legend(x)

setwd(figu.wd); pdf(paste0("Word_network_cooccur_nodeLegend.pdf"))
plot(legend)
dev.off()


#- - - - - - - - - - - - - - - - - - - - - - -
## Plot word network OVERALL, not split by decade ####
d <- occur.total[!is.na(occur.total$subcategory),]

# create df.n as categorical values
d$df.n.f <- as.numeric(d$df.n)
d[d$df.n<0.2,]$df.n.f <- 0
d[d$df.n>=0.2 & d$df.n<0.4,]$df.n.f <- 0.2
d[d$df.n>=0.4 & d$df.n<0.6,]$df.n.f <- 0.4
d[d$df.n>=0.6,]$df.n.f <- 0.6
d$df.n.f <- as.factor(d$df.n.f)

# d$tf.idf <- log(1+d$tf) * d$idf
# d <- d[,c("subcategory", "word2", "tf.idf")]

#d <- com.all
#d$subcategory <- factor(d$subcategory, levels=categories$subcategory)
#d$word2 <- factor(d$word2, levels=categories$subcategory)
#d$bigram <- paste(d$subcategory, d$word2)

summary(d$mean)

g1 <- d[d$subcategory!=d$word2,] %>%
  #filter(mean > lim.set) %>%  #threshold based on corr. value of top 100 correlations
  igraph::graph_from_data_frame()

V(g1)$label <- categories[order(match(categories$subcategory, V(g1)$name)),]$label

## add categories to graph data
categories <- categories[order(match(categories$subcategory, V(g1)$name)),]
V(g1)$category <- as.character(categories[categories$subcategory %in% V(g1)$name,]$category)
#V(g)$category <- as.character(categories$category)
categories <- categories[order(categories$category),]

# netcolors <- c()
# netcolors[1:4] <- "forestgreen"     #color for biodiversity
# netcolors[5:12] <- "tan2"           #color for drivers
# netcolors[13:17] <- "burlywood4"    #color for features
# netcolors[18:25] <- "mediumpurple3" #functions
# netcolors[26:31] <- "deepskyblue3"  #protection
# netcolors[32:34] <- "yellow3"       #soil
# netcolors[35:48] <- "orangered3"    #threat

## calculate centrality of each value
# http://www.sthda.com/english/articles/33-social-network-analysis/136-network-analysis-and-manipulation-using-r/
# "Closeness centrality measures how many steps are 
# required to access every other nodes from a given nodes. 
# It describes the distance of a node to all other nodes. 
# The more central a node is, the closer it is to all other nodes."
# However, we use functions from igraph
V(g1)$centrality <- closeness(g1, normalized=T)

# "The betweenness centrality for each nodes is the 
# number of the shortest paths that pass through the nodes."
#V(g1)$centrality <- betweenness(g1, normalized=F)

# ## plot network
# fixed.layout <- layout_with_kk(g1)
# fixed.layout <- layout.circle(g1)
# fixed.layout <- layout.fruchterman.reingold(g1, repulserad=vcount(g1)^3, area=vcount(g1)^2.4)
# fixed.layout <- layout.grid(g1)

# delete edges lower than lim.set (set before to 0)
#g1 <- delete.edges(g1, which(E(g1)$df.n==0))

# adapt position in network according to order of labels
template.layout <- template.layout[order(match(template.layout$label, V(g1)$label)),]

# define node size
deca.occ <- occur.total[occur.total$subcategory==occur.total$word2,]
deca.occ <- deca.occ[order(match(deca.occ$subcategory, V(g1)$name)),]

V(g1)$mean <- na.omit(deca.occ$mean)
V(g1)$df.n <- na.omit(deca.occ$df.n)

#setwd(figu.wd); pdf("Word_network_cooccur.pdf")
#setwd(figu.wd); pdf(paste0("Word_network_cooccur_lim", round(lim.set,2), "_grid.pdf"))
g1.a <- delete.edges(g1, which(E(g1)$mean<1))

a <- ggraph(g1.a, layout =  as.matrix(template.layout[,1:2])) +
  geom_edge_link(aes(edge_width=E(g1.a)$mean, 
                     edge_alpha=E(g1.a)$mean), #edge_color="grey35", #grey45
                 show.legend = T)+
  geom_node_point(aes(size = V(g1)$mean,col=V(g1.a)$category), show.legend = T) +
  scale_size(range = c(10,20)) + 
  geom_node_text(aes(label = label), cex=7)+#, col=rep(V(g)$category, 3)), repel = TRUE) +
  scale_color_manual(values=barcolors) +
  theme_void() +
  theme(strip.text.x = element_text(size=20), plot.margin=unit(c(1,1,1,1),"cm"), 
        legend.text = element_text(size=20), legend.position = "right")
a

g1.b <- delete.edges(g1, which(E(g1)$df.n.f<0.2))
b <- ggraph(g1.b, layout =  as.matrix(template.layout[,1:2])) +
  geom_edge_link(aes(edge_width=E(g1.b)$df.n.f, 
                     edge_alpha=E(g1.b)$df.n.f), #edge_color="grey35", #grey45
                 show.legend = T)+
  geom_node_point(aes(size = V(g1)$df.n,col=V(g1.b)$category), show.legend = T) +
  scale_size(range = c(10,20)) +
  geom_node_text(aes(label = label), cex=7)+#, col=rep(V(g)$category, 3)), repel = TRUE) +
  scale_color_manual(values=barcolors) +
  theme_void() +
  theme(strip.text.x = element_text(size=20), plot.margin=unit(c(1,1,1,1),"cm"), 
        legend.text = element_text(size=20), legend.position = "right")
b

setwd(figu.wd); pdf("Word_network_total_cooccur_mean_df.n.pdf", width=20)
ggpubr::ggarrange(a,b)
dev.off()

#- - - - - - - - - - - - - - - - - - - - - - -
## Some helpful links, e.g. for network ANALYSES like centrality etc. ####

## Check out the following introduction:
# http://www.sthda.com/english/articles/33-social-network-analysis/136-network-analysis-and-manipulation-using-r/

## also: https://kateto.net/networks-r-igraph
# and: https://ggraph.data-imaginist.com/ for time-splitted graph

# https://cran.r-project.org/web/packages/EcoSimR/vignettes/CoOccurrenceVignette.html
# https://tm4ss.github.io/docs/Tutorial_5_Co-occurrence.html
#... to do

# https://www.tidytextmining.com/ngrams.html

#plot(1:10, sample(2:11), col=1:10, pch=16, cex=5)

#- - - - - - - - - - - - - - - - - - - - - - -
## Plot thesaurus frequencies ####
library(ggplot2)

d <- thesauri

# Change order of labels by asigning factor levels
slevels <- categories$subcategory  # define order (by categories)
d$subcategory <- factor(d$subcategory, levels = rev(slevels))

# define colors for each category
barcolors <- c("forestgreen", "tan2", "burlywood4", "mediumpurple3", 
               "deepskyblue3", "yellow3", "orangered3")

# count thesauri words
x.labs <- c("soil-related term", "soil terms")
names(x.labs) <- c("no", "yes")

#setwd(figu.wd); pdf(file="Barplot_ThesaurusCount_subcategories_soilterms.pdf")
ggplot(data=d[d$category!="remove",], aes(subcategory, fill=category)) +
  geom_bar(show.legend = T, stat="count") +
  coord_flip() +
  scale_fill_manual(values=barcolors) +
  facet_grid(~soil.term, labeller=labeller(soil.term=x.labs))+
  ylab("Mean term frequency per document")+ xlab("")+
  theme_bw()+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 85))
dev.off()

#- - - - - - - - - - - - - - - - - - - - - - -
## Plot number of management plans over time ####

# load meta data
setwd(folder.wd)
meta.all <- read.csv("../Document_Year_all_filled.csv")

# plot number of plans per year
#setwd(figu.wd); pdf(file="Barplot_NumberManagementplans_years.pdf")
ggplot(data=meta.all, aes(x=year))+
  geom_bar()+
  theme_bw()+
  xlab("Year")+ ylab("Number of plans")+
  theme(axis.title = element_text(size=40), axis.text = element_text(size=20))
dev.off()





