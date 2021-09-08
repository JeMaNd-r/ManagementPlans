#- - - - - - - - - - - - - - - - -#
# Text mining of management plans #
#     author: Romy Zeiss          #
#                                 #
#   1. cleaning of thesaurus      #
#- - - - - - - - - - - - - - - - -#

## This script is similar to the cleaning of pdf files. For harmonization, 
# we will clean the word within the thesaurus the same way as the pdf texts.
# Parts of cleaning process that were not necessary for thesaurus are marked
# with an asterisk (*).

# Set working directory
folder.wd; work.wd; stopword.wd; git.wd
setwd(work.wd); getwd()

# Load packages
library(tm)        #for text mining
library(tidytext)  #to transform Corpus in tidy data

## Get package for German stemming from GitHub
#setwd(git.wd); install.packages("cistem", repos = NULL, type = "source")
library(cistem)    #for German stemming

## Set file directory
setwd(work.wd); getwd() 

#- - - - - - - - - - - - - - - - - - - - - - - 
#* ## Define stopwords to be remove below: ####
#* # Use a list of German stopwords provided by Gene Diaz & published here:
#* # https://www.npmjs.com/stopwords-de
#* list.stopwords <- read.delim(stopword.wd, header=F,  encoding = "UTF-8")
#* list.stopwords <- as.vector(list.stopwords$V1)
#* 
#* # remove single letters from stopword list
#* '%nin%' <- Negate('%in%')  #opposite of %in% (i.e. element of) -> meaning: not in
#* list.stopwords <- list.stopwords[list.stopwords %nin% letters]
#* 
#* # add specific strings to stopwords
#* list.stopwords <- c(list.stopwords, 
#*                     #"jeweil", "kung", "keit", "meist", "bzw", "men", 
#*                     #"ggf", "sowi", "ohn", "etc", 
#*                     "ii", "iii", "iv", "vi", "vii", "viii")
#* str(list.stopwords)

# Create function to replace mutated vowels (umlaute)
o <- content_transformer(function(x) gsub("ö", "o", x))
o2 <- content_transformer(function(x) gsub("\xf6", "o", x))
# the "ö" is most important because of the German term for soils ("Böden")
a <- content_transformer(function(x) gsub("ä", "a", x))
u <- content_transformer(function(x) gsub("ü", "u", x))
szet <- content_transformer(function(x) gsub("ß", "sz", x))

# in German compounds divided by "und" (and) or "oder" (or) might share a mutual 
# final noun and are therefore combined, since their meaning cannot only be defined
# on basis of their first noun:
# e.g.: Energie- und Umweltmanagement -> management belongs to both energy and environment
noun.and <- content_transformer(function(x) 
  gsub("(-[[:blank:]]und[[:blank:]])([[:upper:]])", "UND\\2", x, perl=TRUE))

noun.or <- content_transformer(function(x) 
  gsub("(-[[:blank:]]oder[[:blank:]])([[:upper:]])", "ODER\\2", x, perl=TRUE))

# Create function to remove "-" that separate words at end of lines
hyphen <- content_transformer(function(x) gsub("- ", "", x))

## Implement the word stemmer function based on CISTEM stemmer:
# package: https://github.com/FlorianSchwendinger/cistem/
cistem.stemmer <- content_transformer(function(x) {
  unlist(lapply(x, function(line) {    # unlist the corpus and lapply over the list
    paste(cistem(words(line)), collapse = " ")} )  # paste the words back together.
  )
})

#- - - - - - - - - - - - - - - - - - - - - - - 
## Cleaning process ####
getwd()
dir()  #list files in current working directory

# read csv file with thesaurus
thesauri <- read.csv("../Thesauri.csv")

#- - - - - - - - - - - - - - - - - - - - - - - 
## Clean words from thesaurus within a corpus ####

# Create a corpus (database for the text) based on the output list
corp <- VCorpus(VectorSource(thesauri$word))

corp[[4]]$content  # have a look on the text BEFORE

# reminder: code that is commented-out with * is not necessary for thesaurus
#* # Remove whitespace
#* corp <- tm_map(corp, stripWhitespace, lazy=F) #faster if lazy=TRUE

# harmonize misinterpreted characters using function defined above
#corp <- tm_map(corp, HarmonizeMisintChars)
#... ERROR: can't find encoding.table.win object...?

#* # Combine nouns separated by and/or but with similar terminus (see)
#* corp <- tm_map(corp, noun.and)
#* corp <- tm_map(corp, noun.or)

# Case conversion
corp <- tm_map(corp, content_transformer(tolower), lazy = F)

#* # remove stopwords
#* corp <- tm_map(corp, content_transformer(removeWords), 
#*                list.stopwords, lazy=F)

#* # Remove hyphen followed by (single) space 
#* corp <- tm_map(corp, hyphen, lazy=F)

#* # Remove numbers
#* corp <- tm_map(corp, removeNumbers)

# Remove punctuation
corp <- tm_map(corp, removePunctuation, ucp = F, #to remove ASCII characters
               #                 preserve_intra_word_dashes = TRUE,
               preserve_intra_word_contractions = TRUE)  #remove "-" & merge words
#corp <- tm_map(corp, removePunctuation, ucp = TRUE) #based in Unicode

# Replace mutated vowels (umlaut)
corp <- tm_map(corp, o, lazy=F)
corp <- tm_map(corp, o2, lazy=F)
corp <- tm_map(corp, a, lazy=F)
corp <- tm_map(corp, u, lazy=F)
corp <- tm_map(corp, szet, lazy=F)

# Stemming based on CISTEM stemmer (see above)
corp <- tm_map(corp, cistem.stemmer, lazy=T)

## have a look at the text AFTER cleaning
corp[[4]]$content

## Transform Corpus in tidy data (tibble) and back into table ####
corp.tidy <- tidy(corp)

thesauri.cleaned <- thesauri
thesauri.cleaned$word.cl <- corp.tidy$text

#- - - - - - - - - - - - - - - - - - - - - - -
## Save TDM ####
setwd(work.wd); getwd()
write.csv(thesauri.cleaned, file="../Thesauri_cleaned.csv", row.names = F)


