#- - - - - - - - - - - - - - - - -#
# Text mining of management plans #
#     author: Romy Zeiss          #
#                                 #
#   3. occurrence of keywords     #
#- - - - - - - - - - - - - - - - -#

# Assign working directories
folder.wd; work.wd; figu.wd
setwd(work.wd); getwd()

# Load packages
library(ggraph)
#library(influential)
library(tidyverse)
#library(data.table) #to aggregate (i.e. sum up) double entries of same bi-grams
library(EnvStats)    #to detect outliers
library(naniar)      #to replace outliers with NAs

# Check objects
years; decades
yr.deca
keywords

# assign columns corresponding to the documents for each decade
no.doc
no.doc$cols.start <- c(4, 543, 1718)
no.doc$cols.end <- c(542, 1717, 3508)

#- - - - - - - - - - - - - - - - - - - - - - -
## Combine all bi-gram tables ####

# Load all data frame csv files & combine them
setwd(folder.wd); getwd()

# empty file for all years (and per Decade)
occur.total <- tibble::tibble(word1="a", word2="a")[0,]
occur.deca <- tibble::tibble(word1="a", word2="a", deca=0)[0,]

for(yr in years){
  # Set directory where files are stored
  setwd(paste0(folder.wd, "/_tdm"))
  
  # load file
  load(paste0("CooccurPairs_subcate_perDoc_", yr, ".RData")) #occur.all
 
  # combine all years
  occur.total <- full_join(occur.total, occur.all, by=c("word1", "word2"))
  
  # add column with decade and add to overall result table
  occur.all$deca <- yr.deca[yr.deca$years==yr, "deca.seq"]
  occur.deca <- full_join(occur.deca, occur.all, by=c("word1", "word2", "deca"))
}

# replace NA by 0
for(col in 3:ncol(occur.total)){
  temp.df <- occur.total[,col]
  temp.df[is.na(temp.df)] <- 0
  #temp.df[temp.df==0] <- NA  #or replace 0 by NA
  occur.total[,col] <- temp.df
  rm(temp.df)
}

# same for data per decade
for(deca in decades){
  temp.start <- no.doc[no.doc$deca==deca, "cols.start"]
  temp.end <- no.doc[no.doc$deca==deca, "cols.end"]
  temp.df <- occur.deca[occur.deca$deca==deca,]
  
  for(col in temp.start:temp.end){
    temp.df2 <- temp.df[,col]
    temp.df2[is.na(temp.df2)] <- 0
    #temp.df2[temp.df2==0] <- NA     #or replace 0 by NA
    occur.deca[occur.deca$deca==deca, col] <- temp.df2
  }
}

#- - - - - - - - - - - - - - - - - - - - - - -
# ## note: not relevant to sum up double bigrams... function already accounted for
# # but here would be the code to do it
# ## Sum up double bigrams (i.e. if terms are just other way round)
# occur.total$bigram <- apply(cbind(occur.total$word1, occur.total$word2),1, function(x) paste(sort(x), collapse=" "))
# occur.total <- aggregate(.~bigram, data=occur.total[,-c(1,2)], FUN=sum)
# occur.total <- occur.total %>% separate(bigram, c("word1", "word2"), " ")
# 
# # same for data frame with decades
# occur.deca$bigram <- apply(cbind(occur.deca$word1, occur.deca$word2),1, function(x) paste(sort(x), collapse=" "))
# occur.deca2 <- aggregate(.~deca +bigram, occur.deca[,-c(1,2)], FUN=sum, na.action=na.pass)
# test <- occur.deca %>% separate(bigram, c("word1, word2"), " ")

#- - - - - - - - - - - - - - - - - - - - - - -
## Remove outliers identified with Rosner test ####

# Assumption: data without outliers are normally distributed
# We will check this before and after the removal.

## Check if raw data (with outliers) are normally distributed (Shapiro-Wilk test)
test.norm.dist <- data.frame(occur.deca[,c("word1", "word2", "deca")], p.val.raw=NA) 

for(i in 1:nrow(occur.deca)){
  try(test.norm.dist[i,"p.val.raw"] <- 
        shapiro.test(as.numeric(occur.deca[i,4:3508]))["p.value"]) #error if only 0's
}
test.norm.dist["p.val.raw"<0.05,] #none normally distributed (p always > 0.05)

## Detect outliers with Rosner test
occur.deca.clean <- occur.deca  # object where outliers are removed (replaced by NA)

for(i in 1:nrow(occur.deca)) {
  try({
  temp.row <- as.numeric(occur.deca[i,4:3509])
  outlier.detected <- rosnerTest(temp.row[!is.na(temp.row)], k=10)$all.stats #try k=3 & k=10
  
  outlier.detected.val <- outlier.detected$Value[c(outlier.detected$Outlier)]
  
  if(length(outlier.detected.val)>0){
    temp.row <- as.data.frame(temp.row) %>% replace_with_na(replace = list(temp.row = outlier.detected.val))
    temp.row <- as.numeric(temp.row$temp.row)
  }
  
  occur.deca.clean[i,4:3509] <- temp.row
  
  })
}

summary(occur.deca.clean)

names(occur.deca.clean)

#write.csv(occur.deca.clean,"CooccurDF_subcate_perDecade_noOutliers.RData") # Write the file with the distance.

## Check if data without outliers are normally ditributed
test.norm.dist$p.val <- NA

for(i in 1:nrow(occur.deca)){
  try(test.norm.dist[i,"p.val"] <- 
        shapiro.test(as.numeric(occur.deca[i,4:3508]))["p.value"]) #error if only 0's
}
test.norm.dist["p.val"<0.05]

#- - - - - - - - - - - - - - - - - - - - - - -
## Calculate average term frequency etc. for all/ per decade 
occur.total$tf <- rowSums(occur.total[,3:3507], na.rm=T) #number of occurrences over all documents

# the column called "mean" represents the term frequency averaged over all documents
occur.total$mean <- rowMeans(occur.total[,3:3507], na.rm=T)

# calculate median frequency of documents, too
occur.total$median <- matrixStats::rowMedians(as.matrix(occur.total[,3:3507]), na.rm=T)

# same for data per decade
occur.deca[,c("tf", "mean", "median")] <- 0
for(deca in decades){
  temp.start <- no.doc[no.doc$deca==deca, "cols.start"]
  temp.end <- no.doc[no.doc$deca==deca, "cols.end"]
  temp.dat <- occur.deca[occur.deca$deca==deca,]
  
  temp.tf <- temp.dat$tf + rowSums(temp.dat[,temp.start:temp.end], na.rm=T) #number of occurrences over all documents per decade
  temp.mean <- temp.dat$mean + rowMeans(temp.dat[,temp.start:temp.end], na.rm=T)
  temp.median <- temp.dat$median + matrixStats::rowMedians(as.matrix(temp.dat[,temp.start:temp.end]), na.rm=T)

  occur.deca[occur.deca$deca==deca, "tf"] <- temp.tf
  occur.deca[occur.deca$deca==deca,"mean"] <- temp.mean
  occur.deca[occur.deca$deca==deca,"median"] <- temp.median
}
  
#- - - - - - - - - - - - - - - - - - - - - - -
## calculate number of documents where terms are present 
d <- occur.total
d2 <- d[,3:3507] # remove character columns and mean column

# replace values larger than 0 (= presence) by 1
d2[d2>0] <- 1
d2$presence <- rowSums(d2, na.rm=T)  # gives number of documents where bi-gram present

# add as column to overall data frame
occur.total$df <- d2$presence

# do same for data set per decades
d <- occur.deca; d2 <- d[,4:3508] #remove also column with decade
d2[d2>0] <- 1
d2$presence <- rowSums(d2, na.rm=T)  # gives number of documents where bi-gram present
occur.deca$df <- d2$presence

rm(d); rm(d2)

#- - - - - - - - - - - - - - - - - - - - - - -
## calculate inverse document frequency (IDF)
# IDF = 1 + log2 (n / df)
# n...  number of documents in corpus
# df... number of documents where term is present
n <- ncol(occur.deca[,4:3508]); n
occur.deca$idf <- 1 + log2( (n) / occur.deca$df )

n <- ncol(occur.total[,3:3507]); n 
occur.total$idf <- 1 + log2( (n) / occur.total$df )

rm(n)

# calculate tf * df
occur.total$tf.df <- occur.total$tf * occur.total$df
occur.deca$tf.df <- occur.deca$tf * occur.deca$df

# rename first column ("word1")
colnames(occur.total)[1] <- "subcategory"
colnames(occur.deca)[1] <- "subcategory"

#- - - - - - - - - - - - - - - - - - - - - - -
# calculate logarithm for tf and df
occur.total$tf.log <- log(1+occur.total$tf)
occur.total$df.log <- log(1+occur.total$df)

occur.total$tf.df.log <- occur.total$tf.log * occur.total$df.log

occur.deca$tf.log <- log(1+occur.deca$tf)
occur.deca$df.log <- log(1+occur.deca$df)

occur.deca$tf.df.log <- occur.deca$tf.log * occur.deca$df.log

# calculate proportion of documents containing terms
occur.total$df.n <- occur.total$df / 3505
occur.deca$df.n <- occur.deca$df / (3505-rowSums(is.na(occur.deca)))

## save co-occurrences as data frames
setwd(folder.wd)
save(occur.total, file="CooccurDF_subcate_total.RData")
save(occur.deca, file="CooccurDF_subcate_perDecade.RData")

#- - - - - - - - - - - - - - - - - - - - - - -
## Create co-occurrence matrix (COM) ####
load("CooccurDF_subcate_total.RData")
load("CooccurDF_subcate_perDecade.RData")

com.mean <- tibble::tibble(word1=occur.total$subcategory, word2=occur.total$word2,
                      mean=occur.total$mean)

com.tfidf <- tibble::tibble(word1=occur.total$subcategory, word2=occur.total$word2,
                           tf.idf=(occur.total$tf*occur.total$idf))

# com.sum <- tibble::tibble(occur.total[,1], occur.total[,2],
#                            sum=rowSums(occur.total[,-(1:2)], na.rm = T))

com.mean <- reshape2::acast(com.mean, word1 ~ word2)
com.mean[is.na(com.mean)] <- 0
#com.mean <- round(com.mean, digits=3)

com.tfidf <- reshape2::acast(com.tfidf, word1 ~ word2)
com.tfidf[is.na(com.tfidf)] <- 0
#com.tfidf <- round(com.tfidf, digits=3)

## Save co-occurrence matrices
setwd(folder.wd)
save(com.mean, file=paste0("CooccurMatrix_subcate_total_mean.RData"))
save(com.tfidf, file=paste0("CooccurMatrix_subcate_total_tfidf.RData"))

#- - - - - - - - - - - - - - - - - - - - - - -
## Create COM per decade
for(deca in decades){
  occur.temp <- occur.deca[occur.deca$deca==deca,] 
  # create co-occurrence matrix (COM) 
  com.mean.deca <- tibble::tibble(word1=occur.temp$subcategory, word2=occur.temp$word2,
                             mean=occur.temp$mean)
  
  com.deca <- reshape2::acast(com.mean.deca, word1 ~ word2)
  com.deca[is.na(com.deca)] <- 0
  #com.deca <- round(com.deca, digits=3)

  # Save co-occurrence matrices per decade
  setwd(folder.wd)
  save(com.deca, file=paste0("CooccurMatrix_subcate_d", deca, "_mean.RData"))
  
  #- - - - - - - - - - - - - - - - - - - - - - -
  ## create co-occurrence matrix (COM) with tf-idf 
  com.tfidf.deca <- tibble::tibble(word1=occur.temp$subcategory, word2=occur.temp$word2,
                                  tf.idf=(occur.temp$tf*occur.temp$idf))
  
  com.deca <- reshape2::acast(com.tfidf.deca, word1 ~ word2)
  com.deca[is.na(com.deca)] <- 0
  com.deca <- round(com.deca, digits=3)
  
  # Save co-occurrence matrices per decade
  setwd(folder.wd)
  save(com.deca, file=paste0("CooccurMatrix_subcate_d", deca, "_tfidf.RData"))
  
  print(paste(deca, "ready"))
}



