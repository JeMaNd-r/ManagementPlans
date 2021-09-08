#- - - - - - - - - - - - - - - - -#
# Text mining of management plans #
#     author: Romy Zeiss          #
#                                 #
#  3. co-occurrence from tokens   #
#      (code to run parallel)     #
#- - - - - - - - - - - - - - - - -#

# Set working directory
folder.wd <- "I:/eie/SoilBON/ManagementPlans"
work.wd <- ("~/Text mining MaP/R Scripts")
setwd(work.wd); getwd()

## Set folder directory to list Federal States (folders)
setwd(folder.wd); getwd() 
#folders <- list.dirs(full.names = F)  #list of directories to get State's names
folders <- c("Bavaria", "BaWue", "Berlin", "Brandenburg", "Bremen", 
             "Hamburg", "Hessen", "LowSax", "MeckPom", "NRW", "RLP", 
             "Saarland", "SaxAnh", "Saxony", "SchleHol", "Thuringia" )
folders 
# Note: Lower Saxony has do be removed if the focus is on comparing Federal States, 
# as LowSax only offered 2 management plans currently.

#- - - - - - - - - - - - - - - - - - - - - - -
## Load keywords ####
setwd(work.wd); getwd()

thesauri <- read.csv("../Thesauri.csv")
soil.kw <- as.vector(thesauri[thesauri$soil.term=="yes", "regex"]); soil.kw

thesauri <- thesauri[order(nchar(as.character(thesauri$word)), decreasing=T),]
keywords <- thesauri$word
#wordlist <- unique(thesauri$subcategory)

#- - - - - - - - - - - - - - - - - - - - - - -
# Load packages
library(dplyr)     #to merge data frames
library(tm)        #for text mining
library(stringr)   #to split text into chunks
library(tidytext)  #to work with tibble and pipe (%>%)

## Check definition for soil terms (soil.kw) and keywords
keywords 
soil.kw 

#- - - - - - - - - - - - - - - - - - - - - - -
## Count occurrences of specific word BIGRAMS per YEAR ####

## Initiate cluster used in foreach function
#registerDoParallel(no.cores)

## For loop through all years
#foreach(yr = years, .export=c("thesauri", "keywords", "folder.wd")) %dopar% {
#for(yr in years){

#- - - - - - - - - - - - - - - - - - - - - - - 
## Change YEAR here to parallelize ####
yr = 2001
#- - - - - - - - - - - - - - - - - - - - - - - 
# Set directory where files are stored
setwd(paste0(folder.wd, "/_tdm"))

## Import of text files as data frame (tibble)
# create output data frame
load(paste0("Tokens_", yr, ".RData"))  ####remove test!!!####

# empty list to store resulting co-occurrence of words
bigrams <- vector(mode="list", length=length(tokens))
bigram.all <- tibble::tibble(word1="a", word2="a")[0,]
co.occur <- vector(mode="list", length=length(tokens))
occur.all <- tibble::tibble(word1="a", word2="a")[0,]
single.occ <- vector(mode="list", length=length(tokens))

for(i in 1:length(tokens)){
  for(k2 in 1:length(keywords)){
    
    # replace terms that match one of the pre-defined patterns from thesauri
    # by the assigned subcategory
    tokens[[i]] <- stringr::str_replace_all(tokens[[i]], 
                                            as.character(thesauri[k2, "regex"]), 
                                            as.character(thesauri[k2, "word"]))
  }
  
  ## remove all words not in thesaurus (keep only those in thesauri)
  tokens[[i]] <- strsplit(unlist(tokens[[i]]), " ")  #separate words within each token
  tokens[[i]] <- lapply(tokens[[i]], function(y) intersect(y, keywords))  #keep only words from thesaurus
  tokens[[i]] <- lapply(tokens[[i]], function(y) paste(y, collapse=" "))  #merge words back together
  
  # control step if tokens vector is empty
  if(length(tokens[[i]])==0){
    co.occur[[i]] <- tibble::tibble(word1=NA, word2=NA, n=1)
    bigrams[[i]] <- tibble::tibble(word1=NA, word2=NA, n=1)
  }else{
    # unlist tokens into one vector
    tokens[[i]] <- unlist(tokens[[i]])
    
    # transform to data frame (easier to handle, necessary for bi-grams)
    tokens[[i]] <- tibble::tibble(token=tokens[[i]], id=1:length(tokens[[i]]))
    
    ## Make and count occurrences ####
    # count occurrences in tokens with only word only, for later
    single.occ[[i]] <- tokens[[i]][!stringr::str_detect(tokens[[i]]$token, " "),]
    single.occ[[i]] <- tidytext::unnest_tokens(single.occ[[i]], word, token, token = "words")
    single.occ[[i]] <- count(single.occ[[i]], word, sort=T)
    single.occ[[i]]$variable <- single.occ[[i]]$word
    
    # remove lines where token is only one word !!keep that in mind later on!!
    tokens[[i]] <- tokens[[i]][stringr::str_detect(tokens[[i]]$token, " "),]
    
    ## make and count co-occurrences -> matrix
    co.occur[[i]] <- tidytext::unnest_tokens(tokens[[i]], word, token, token = "words")
    co.occur[[i]] <- crossprod(table(co.occur[[i]]$id, co.occur[[i]]$word))
    
    # split matrix to get long format
    co.occur[[i]] <- tibble::as_tibble(co.occur[[i]])
    co.occur[[i]]$word <- colnames(co.occur[[i]])
    co.occur[[i]] <- reshape2::melt(co.occur[[i]])
    
    ## Make and count bi-grams ####
    bigrams[[i]] <- tidytext::unnest_tokens(tokens[[i]], word, token, token = "ngrams", n = 2)
    bigrams[[i]] <- dplyr::count(bigrams[[i]], word, sort = TRUE)
    
    # Split bi-grams into single words
    bigrams[[i]] <- tidyr::separate(bigrams[[i]], word, c("item1", "item2"), sep=" ")
    
    ## Add single occurrences from tokens with only word (see above)
    colnames(single.occ[[i]])[2] <- "value"
    co.occur[[i]] <- aggregate(value~word+variable, 
                               dplyr::bind_rows(co.occur[[i]], single.occ[[i]]), sum)
    
    colnames(single.occ[[i]]) <- c("item1", "n", "item2")
    bigrams[[i]] <- aggregate(n~item1+item2, 
                              dplyr::bind_rows(bigrams[[i]], single.occ[[i]]), sum)
    
  } 
  
  colnames(co.occur[[i]]) <- c("word1", "word2", i)
  colnames(bigrams[[i]]) <- c("word1", "word2", i)
  
  # merge with result data
  occur.all <- dplyr::full_join(occur.all, co.occur[[i]])
  bigram.all <- dplyr::full_join(bigram.all, bigrams[[i]])
  
  #to do... replace NA by 0
  
}   

## remove lines with wrong terms (i.e. terms not in keywords)
#occur.all <- occur.all[occur.all$word1 %in% keywords,]
#occur.all <- occur.all[occur.all$word2 %in% keywords,]

## save tdm with all words
save(occur.all, file=paste0("Bigrams_perDoc_",yr, ".RData"))
save(bigram.all, file=paste0("Bigrams_",yr, ".RData"))

print(paste(yr, "ready"))


