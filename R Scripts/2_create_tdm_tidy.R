#- - - - - - - - - - - - - - - - -#
# Text mining of management plans #
#     author: Romy Zeiss          #
#                                 #
#  2. create tdm from clean text  #
#- - - - - - - - - - - - - - - - -#

# Set working directory
folder.wd; work.wd; git.wd
setwd(work.wd); getwd()

# Load packages
library(dplyr)     #to merge data frames
library(tm)        #for text mining
library(stringr)   #to split text into chunks
library(tidytext)  #to work with tibble and pipe (%>%)

## to parallelize code:
library(doParallel)

# Calculate the number of cores
no.cores <- detectCores()/2; no.cores

## Initiate cluster used in parLapply function
#cl <- makeCluster(no.cores); cl

# at the end: give cores free
#stopCluster(cl)

## Initiate cluster used in foreach function
registerDoParallel(no.cores)

# at the end
#stopImplicitCluster()

## Check definition for soil terms (soil.kw) and keywords
keywords 
soil.kw 

## Define years to go through in separate loop ####
# otherwise, R will likely not handle all of the files per State simultaneously
years 

#- - - - - - - - - - - - - - - - - - - - - - - 
## Extract TOKENS (word groups) centered on soil terms ####

# template to create output data frame
setwd(paste0(folder.wd, "/_tdm"))
load(paste0("Corp_Bavaria_2014.RData"))
header <- corp.tidy[0,]; header

# NOTE: NRW has 2 documents less than written down in metadata & summary csv.
#       So, instead of 497, it only has 495 management plans. 
#       Missing plans: in 2020 ...

## For loop through all years first
#foreach(yr=years, .export=c("folder.wd", "soil.kw", "folders", "all.files"), 
#         .packages = "foreach") %dopar% {

for(yr in years){
  # Set directory where files are stored
  setwd(paste0(folder.wd, "/_tdm"))
  
  ## Import of text files as data frame (tibble)
  # create output data frame
  corp <- header; corp$state <- "NA"
  
  ## for loop through all Federal States, combine them with full_join
  for(s in folders){
    #corpus <- foreach(s = folders, .export = "time", .combine=dplyr::full_join) %do% {
    
    try({  # ignores errors, i.e. if no document in specific year published
      load(paste0("Corp_", s, "_", yr, ".RData"))  
      corp.tidy$state <- s  # add state column
      corp <- add_row(corp, corp.tidy)  # merge with output data
    })
  }
  
  save(corp, file=paste0("Corp_", yr, ".RData"))
  print(paste(yr, "ready"))
}
  
# Run this code in different windows of R(Studio) to parallelize 
# for years by hand.

#- - - - - - - - - - - - - - - - - - - - - - -
## Set (and change) year: 2001-2020
yr=2016  
#- - - - - - - - - - - - - - - - - - - - - - -

setwd(paste0(folder.wd, "/_tdm"))
load(paste0("Corp_", yr, ".RData")) #load object "corp"

#- - - - - - - - - - - - - - - - - - - - - - -
## Get tokens of 11 words centered on soil keywords ####
tokens <- vector(mode="list", length=nrow(corp)) # empty vector to store resulting tokens
#tokens <- c()

for(i in 1:nrow(corp)){
  for(k in 1:length(soil.kw)){
    pattern <- soil.kw[k]
    
    # extract tokens centered on soil keyword k
    temp.token <- stringr::str_extract_all(corp[i, "text"], 
                                           paste0("(?:[^\\s]+\\s){5}", pattern, "(?:\\s[^\\s]+){5}"))
    
    # merge with all tokens (i.e. of other soil terms)
    tokens[[i]] <- c(tokens[[i]], unlist(temp.token))
    
  }
  
}

names(tokens) <- stringr::str_extract(corp$text, "(?:[^\\s]+\\s){10}")

save(tokens, file=paste0("Tokens_", yr, ".RData"))

print(paste(yr, "ready"))

#- - - - - - - - - - - - - - - - - - - - - - -
## Count occurrences of specific word BI-GRAMS per YEAR ####
keywords

## Initiate cluster used in foreach function
registerDoParallel(no.cores)

# Create object with information to each document id
id.table <- tibble::tibble(id="a", year=1, state="a", text.title="a")[0,]

# create empty vector to store words not in thesaurus
list.not.thesaurus <- c()

## For loop through all years
#foreach(yr = years, .export=c("thesauri", "keywords", "folder.wd")) %dopar% {
for(yr in years){
  # Set directory where files are stored
  setwd(paste0(folder.wd, "/_tdm"))
  
  ## Import of text files as data frame (tibble)
  load(paste0("Tokens_", yr, ".RData"))
 
  ## empty list to store resulting co-occurrence of words
  # bigrams <- vector(mode="list", length=length(tokens))
  # bigram.all <- tibble::tibble(word1="a", word2="a")[0,]
  co.occur <- vector(mode="list", length=length(tokens))
  not.in.thesaurus <- vector(mode="list", length=length(tokens))
  occur.all <- tibble::tibble(word1="a", word2="a")[0,]
  single.occ <- vector(mode="list", length=length(tokens))
  
  no.documents <- 1:length(tokens)
  
  for(i in no.documents){
    for(k2 in 1:length(keywords)){
    
      # replace terms that match one of the pre-defined patterns from thesauri
      # by the assigned subcategory
      tokens[[i]] <- stringr::str_replace_all(tokens[[i]], 
                                         as.character(thesauri[k2, "regex"]), 
                                         as.character(thesauri[k2, "subcategory"])) #replace by "word" or "translation"...
    }
  
    ## remove all words not in thesaurus (keep only those in thesauri)
    tokens[[i]] <- strsplit(unlist(tokens[[i]]), " ")  #separate words within each token
    
    # extract words not listed in thesaurus
    not.in.thesaurus[[i]] <- lapply(tokens[[i]], function(y) setdiff(y, unique(thesauri$subcategory)))
    
    # remove those words
    tokens[[i]] <- lapply(tokens[[i]], function(y) intersect(y, unique(thesauri$subcategory)))  #keep only words from thesaurus
    tokens[[i]] <- lapply(tokens[[i]], function(y) paste(y, collapse=" "))  #merge words back together
   
    # control step if tokens vector is empty
    if(length(tokens[[i]])==0){
      co.occur[[i]] <- tibble::tibble(word1=NA, word2=NA, n=1)
      not.in.thesaurus[[i]] <- c(NA)
      #bigrams[[i]] <- tibble::tibble(word1=NA, word2=NA, n=1)
    }else{
      # unlist tokens into one vector
      tokens[[i]] <- unlist(tokens[[i]])
      
      # transform to data frame (easier to handle, necessary for bi-grams)
      tokens[[i]] <- tibble::tibble(token=tokens[[i]], id=1:length(tokens[[i]]))
    
      ## Make and count occurrences ####
      # count occurrences in tokens with only word only, for later
      single.occ[[i]] <- tokens[[i]][!stringr::str_detect(tokens[[i]]$token, " "),]
      single.occ[[i]] <- tidytext::unnest_tokens(single.occ[[i]], word, token, token = "words")
      single.occ[[i]] <- dplyr::count(single.occ[[i]], word, sort=T)
      single.occ[[i]]$variable <- single.occ[[i]]$word
      
      # remove lines where token is only one word
      tokens[[i]] <- tokens[[i]][stringr::str_detect(tokens[[i]]$token, " "),]
      
      ## make and count co-occurrences -> matrix
      co.occur[[i]] <- tidytext::unnest_tokens(tokens[[i]], word, token, token = "words")
      co.occur[[i]] <- crossprod(table(co.occur[[i]]$id, co.occur[[i]]$word))
      
      # split matrix to get long format
      co.occur[[i]] <- tibble::as_tibble(co.occur[[i]])
      co.occur[[i]]$word <- colnames(co.occur[[i]])
      co.occur[[i]] <- reshape2::melt(co.occur[[i]])
      
      # ## Make and count bi-grams instead of co-occurrence pairs ####
      # bigrams[[i]] <- tidytext::unnest_tokens(tokens[[i]], word, token, token = "ngrams", n = 2)
      # bigrams[[i]] <- dplyr::count(bigrams[[i]], word, sort = TRUE)
      # 
      # # Split bi-grams into single words
      # bigrams[[i]] <- tidyr::separate(bigrams[[i]], word, c("item1", "item2"), sep=" ")
      
      ## Add single occurrences from tokens with only word (see above)
      colnames(single.occ[[i]])[2] <- "value"
      co.occur[[i]] <- aggregate(value~word+variable,
                                  dplyr::bind_rows(co.occur[[i]], single.occ[[i]]), sum)
      
      # colnames(single.occ[[i]]) <- c("item1", "n", "item2")
      # bigrams[[i]] <- aggregate(n~item1+item2, 
      #                           dplyr::bind_rows(bigrams[[i]], single.occ[[i]]), sum)
      
    } 
    
    # # assign ID for document in overall table
    # temp.s <- paste0(substr(s, start = 1, stop = 2), stringr::str_sub(s, start = -1))
    # temp.id <- paste(stringr::str_sub(yr, -2), temp.s, i, sep=".")
    temp.id <- paste(stringr::str_sub(yr, -2), i, sep=".")
    # temp.id.table <- data.frame("id"=temp.id, "state"=s, "year"=yr,
    #                             "text.title"=names(tokens)[i])
    # id.table <- rbind(id.table, temp.id.table)
    
    colnames(co.occur[[i]]) <- c("word1", "word2", temp.id)
    #colnames(bigrams[[i]]) <- c("word1", "word2", temp.id)
    
    # merge with result data
    occur.all <- dplyr::full_join(occur.all, co.occur[[i]])
    list.not.thesaurus <- unique(c(unlist(not.in.thesaurus[[i]]), list.not.thesaurus))
    #bigram.all <- dplyr::full_join(bigram.all, bigrams[[i]])
  }   
  
  ## remove lines with wrong terms (i.e. terms not in keywords)
  #occur.all <- occur.all[occur.all$word1 %in% keywords,]
  #occur.all <- occur.all[occur.all$word2 %in% keywords,]
  
  ## save tdm with all words
  save(occur.all, file=paste0("CooccurPairs_subcate_perDoc_",yr, ".RData"))
  #save(bigram.all, file=paste0("Bigrams_subcate_",yr, ".RData"))
  
  #print(paste(yr, "ready"))
}

#stopCluster(cl)
stopImplicitCluster() #stop cluster from foreach function

# save some output
list.not.thesaurus
setwd(folder.wd); write.csv(list.not.thesaurus,"Terms_not_in_thesaurus.csv", row.names = F)

id.table
setwd(folder.wd); write.csv(id.table, "Document_ID_table.csv", row.names = F)

