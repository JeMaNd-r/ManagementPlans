#- - - - - - - - - - - - - - - - -#
# Text mining of management plans #
#     author: Romy Zeiss          #
#                                 #
#  2. create tdm from clean text  #
#   (code for running parallel    #
#        to extract tokens)       #
#- - - - - - - - - - - - - - - - -#

# Set working directory
folder.wd <- "I:/eie/==PERSONAL/RZ SoilBON/RZ ManagementPlans"
work.wd <- ("~/Text mining MaP/R Scripts")
setwd(work.wd); getwd()

## Set folder directory to list Federal States (folders)
setwd(folder.wd); getwd() 
#folders <- list.dirs(full.names = F)  #list of directories to get State's names
folders <- c("Bavaria", "BaWue", "Berlin", "Brandenburg", "Bremen", 
             "Hamburg", "Hessen", "LowSax", "MeckPom", "NRW", "RLP", 
             "Saarland", "SaxAnh", "Saxony", "SchleHol", "Thuringia" )
# Note: Lower Saxony has do be removed if the focus is on comparing Federal States, 
# as LowSax only offered 2 management plans currently.

#- - - - - - - - - - - - - - - - - - - - - - -
## Load keywords ####
setwd(work.wd); getwd()

thesauri <- read.csv("../Thesauri.csv")
soil.kw <- as.vector(thesauri[thesauri$soil.term=="yes", "regex"]); soil.kw

thesauri <- thesauri[order(nchar(as.character(thesauri$word)), decreasing=T),]
keywords <- thesauri$word; keywords

#- - - - - - - - - - - - - - - - - - - - - - -
# Load packages
library(dplyr)     #to merge data frames
library(tm)        #for text mining
library(stringr)   #to split text into chunks
library(tidytext)  #to work with tibble and pipe (%>%)

## Check definition for soil terms (soil.kw) and keywords
#keywords 
#soil.kw 

#- - - - - - - - - - - - - - - - - - - - - - -
## Set (and change) year: 2001-2020
yr=2011  
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


## not needed anymore: 
# # split data frame into list with text of each document (row) as one elements
# corp2 <- split(corp$text, seq(nrow(corp)))
# 
# ## Get tokens of 11 words centered on soil keywords ####
# #tokens <- vector(mode="list", length=length(corp)) # empty vector to store resulting tokens
# tokens <- c()
# 
# ## for loop through all soil keywords
# #foreach(k = 1:length(soil.kw), .export="corp") %dopar% {
# #foreach(k = c(1, 146, 200), .export="corp", .combine=unlist) %dopar% {
# for(k in 1:length(soil.kw)){
#   pattern <- soil.kw[k]
#   
#   # extract tokens centered on soil keyword k
#   temp.token <- stringr::str_extract_all(corp, 
#                                          paste0("(?:[^\\s]+\\s){5}", pattern, "(?:\\s[^\\s]+){5}"))
#   
#   # unlist tokens per document into vector, save in list as one element
#   temp.token <- unlist(temp.token)
#   
#   # merge with all tokens (i.e. of other soil terms)
#   tokens <- c(tokens, unlist(temp.token))
# }
# 
# # save tokens per year
# setwd(folder.wd)
# save(tokens, file=paste0("tokens_", yr, ".RData"))
# 
# print(paste0(yr, " tokens ready"))
