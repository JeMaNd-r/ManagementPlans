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
library(data.table) #to aggregate (i.e. sum up) double entries of same bi-grams

# Check objects
years; decades
yr.deca

#- - - - - - - - - - - - - - - - - - - - - - -
## Combine all bi-gram tables ####

# Load all data frame csv files & combine them
setwd(paste0(folder.wd, "/_tdm")); getwd()

# create empty data frames to store all bigrams together/ per decade
bigram.deca <- tibble::tibble(item1="a", item2="a",deca=0)[0,]
#bigram.deca <- tibble::tibble(item1="a", item2="a", year=0,deca=0)[0,]

for(yr in years){
  load(paste0("Bigrams_", yr, ".RData"))
  #bigram.all <- full_join(bigram.all, bigrams, by="word")
  
  # add column with year and decade
  #bigram.all$year <- yr
  bigram.all$deca <- yr.deca[yr.deca$years==yr, "deca.seq"]
  
  colnames(bigram.all)[1:2] <- c("item1", "item2")
  
  # bind data frames all together
  bigram.deca <- full_join(bigram.deca, bigram.all, by=c("item1", "item2", "deca"))
  
}

colnames(bigram.deca) <- c("item1", "item2", "deca", 1:(ncol(bigram.deca)-3))

# replace NA by 0
for(col in 4:ncol(bigram.deca)){
  temp.df <- bigram.deca[,col]
  temp.df[is.na(temp.df)] <- 0
  bigram.deca[,col] <- temp.df
  rm(temp.df)
}

##... mean or sum per year/decade...?

# aggregate new-created double entries
bigram.total <- bigram.deca[,-3]
bigram.total <- as.data.table(bigram.total)

# calculate sums
numeric_cols <- which(sapply(bigram.total, is.numeric)) # which columns are numeric
bigram.total <- bigram.total[,lapply(.SD, sum), by = c("item1", "item2"), .SDcols = numeric_cols]
bigram.total <- tibble::as_tibble(bigram.total)

# same for decades
bigram.deca <- as.data.table(bigram.deca)
#bigram.deca$year <- as.character(bigram.deca$year)
bigram.deca$deca <- as.character(bigram.deca$deca)
numeric_cols <- which(sapply(bigram.deca, is.numeric)) # which columns are numeric
bigram.deca <- bigram.deca[,lapply(.SD, sum), by = c("item1", "item2", "deca"), .SDcols = numeric_cols]
bigram.deca <- tibble::as_tibble(bigram.deca)


# order data frames by number of occurrences
bigram.deca <- bigram.deca[order(bigram.deca$n, decreasing = T),]

## make one column with bi-gram
# bigram.all$bigram <- paste(bigram.all$item1, bigram.all$item2)
# bigram.deca$bigram <- paste(bigram.deca$item1, bigram.deca$item2)

head(bigram.deca)

setwd(work.wd)
#write.csv(bigram.total, "../Bigrams_all.csv", row.names = F)
#write.csv(bigram.deca, "../Bigrams_perDecade.csv", row.names = F)
bigram.total <- read.csv("../Bigrams_all.csv")
bigram.deca <- read.csv("../Bigrams_perDecade.csv")

#- - - - - - - - - - - - - - - - - - - - - - -
## Replace term in bigrams by their respective subcategories ####
keywords # terms to be replaced

for(k in 1:length(keywords)){
  bigram.total$item1 <- stringr::str_replace(bigram.total$item1, 
                                          as.character(thesauri[k, "word"]), 
                                          thesauri[k, "subcategory"])
  bigram.total$item2 <- stringr::str_replace(bigram.total$item2, 
                                          as.character(thesauri[k, "word"]), 
                                          thesauri[k, "subcategory"])
  bigram.total$bigram <- stringr::str_replace_all(bigram.total$bigram, 
                                          as.character(thesauri[k, "word"]), 
                                          thesauri[k, "subcategory"])
  
  bigram.deca$item1 <- stringr::str_replace(bigram.deca$item1, 
                                           as.character(thesauri[k, "word"]), 
                                           thesauri[k, "subcategory"])
  bigram.deca$item2 <- stringr::str_replace(bigram.deca$item2, 
                                           as.character(thesauri[k, "word"]), 
                                           thesauri[k, "subcategory"])
  bigram.deca$bigram <- stringr::str_replace_all(bigram.deca$bigram, 
                                                as.character(thesauri[k, "word"]), 
                                                thesauri[k, "subcategory"])
}

# aggregate possible double entries after replacing
bigram.total <- aggregate(n~., bigram.total, FUN=sum)
bigram.deca <- aggregate(n~., bigram.deca, FUN=sum)

bigram.total <- bigram.total[order(bigram.total$n, decreasing = T),]
bigram.deca <- bigram.deca[order(bigram.deca$n, decreasing = T),]

## Save modified table with bi-grams
#write.csv(bigram.total, "../Bigrams_all_replaced.csv", row.names = F)
#write.csv(bigram.deca, "../Bigrams_perDecade_replaced.csv", row.names = F)
bigram.total <- read.csv("../Bigrams_all_replaced.csv")
bigram.deca <- read.csv("../Bigrams_perDecade_replaced.csv")

# #- - - - - - - - - - - - - - - - - - - - - - -
# ## Create data on number of occurrences ####
# 
# # Load bigrams in which terms aren't replaced by subcategories
# setwd(work.wd)
# bigram.total <- read.csv("../Bigrams_total.csv")
# bigram.deca <- read.csv("../Bigrams_perDecade.csv")
# 
# # count number of words in both word columns of bi-gram table
# no.occur <- aggregate(n~item1, bigram.total[bigram.total$item1==bigram.total$item2,], sum)
# colnames(no.occur) <- c("word", "n")
# 
# # no.occur.deca <- aggregate(n~word, rbind(no.occur, no.occur2), sum)
# # rm(no.occur2)
#  
# # Do the same for the data per decade
# no.occur.deca <- aggregate(n~item1+deca, bigram.deca[bigram.deca$item1==bigram.deca$item2,], sum)
# colnames(no.occur.deca) <- c("word", "deca", "n")
# # no.occur.deca2 <- aggregate(n~item2+deca, bigram.deca, sum)
# # colnames(no.occur.deca2) <- c("word", "deca", "n")
# # 
# # no.occur.deca <- aggregate(n~word+deca, rbind(no.occur.deca, no.occur.deca2), sum)
# # rm(no.occur.deca2)
# 
# ## Finally, replace words by German terms
# # Make copy of tables
# no.occurE <- no.occur; no.occur.decaE <- no.occur.deca
# 
# # for loop through all terms to replace with translation
# for(k in 1:length(keywords)){
#   no.occurE$word <- stringr::str_replace_all(no.occurE$word, 
#                                                as.character(thesauri[k, "word"]), 
#                                                thesauri[k, "translation"])
#   no.occur.decaE$word <- stringr::str_replace_all(no.occur.decaE$word, 
#                                         as.character(thesauri[k, "word"]), 
#                                         thesauri[k, "translation"])
# }
# 
# # sum up double entries (note: German has many synonyms)
# no.occurE <- aggregate(n~word, no.occurE, sum)
# no.occur.decaE <- aggregate(n~word, no.occur.decaE, sum)
# 
# #- - - - - - - - - - - - - - - - - - - - - - -
# ## add categories to all terms in occurrence table ####
# categories <- unique(thesauri[, c("category", "subcategory", "translation")])
# colnames(categories)[3] <- "word"
# 
# no.occur <- merge(no.occur, categories, by="word")
# no.occur.deca <- merge(no.occur.deca, categories, by="word")
# 
# no.occurE <- merge(no.occurE, categories, by="word")
# no.occur.decaE <- merge(no.occur.decaE, categories, by="word")
# 
# #- - - - - - - - - - - - - - - - - - - - - - - 
# ## Save new data of occurrences
# setwd(work.wd); getwd()
# #write.csv(no.occur, "../Occurrence_words.csv", row.names = F)
# #write.csv(no.occur.deca, "../Occurrence_perDeca_words.csv", row.names = F)
# 
# #write.csv(no.occurE, "../Occurrence_words_EN.csv", row.names = F)
# #write.csv(no.occur.decaE, "../Occurrence_perDeca_words_EN.csv", row.names = F)
# 
# ## Save soil terms separately ####
# soil.terms <- unique(thesauri[thesauri$soil.term=="yes", "translation"])
# soil.occur <- no.occurE[no.occurE$word %in% soil.terms,]
# 
# #write.csv(soil.occur, "../Occurrence_soilterms_EN.csv", row.names = F)
#   
