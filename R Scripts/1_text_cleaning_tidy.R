#- - - - - - - - - - - - - - - - -#
# Text mining of management plans #
#     author: Romy Zeiss          #
#                                 #
#   1. text cleaning from pdfs    #
#- - - - - - - - - - - - - - - - -#

# Set working directory
folder.wd; work.wd; stopword.wd; git.wd
setwd(work.wd); getwd()

# Load packages
#library(dplyr)     #to merge data frames
library(pdftools)  #to convert pdf in csv files
library(tm)        #for text mining
library(tidytext)  #to transform Corpus in tidy data
library(callr)     #to avoid error with memory space

## Get package for German stemming from GitHub
#setwd(git.wd); install.packages("cistem", repos = NULL, type = "source")
library(cistem)    #for German stemming

## Set folder directory to go through Federal States
setwd(folder.wd); getwd() 
folders 
# Note: Lower Saxony has do be removed if the focus is on comparing Federal States, 
# as LowSax only offered 2 management plans currently.

#- - - - - - - - - - - - - - - - - - - - - - - 
## Define years to go through in separate loop ####
# otherwise, R will likely not handle all of the files per State simultaneously
head(meta.all)
years

## Define stopwords to be remove below:
# Use a list of German stopwords provided by Gene Diaz & published here:
# https://www.npmjs.com/stopwords-de
list.stopwords <- read.delim(stopword.wd, header=F,  encoding = "UTF-8")
list.stopwords <- as.vector(list.stopwords$V1)

# remove single letters from stopword list
'%nin%' <- Negate('%in%')  #opposite of %in% (i.e. element of) -> meaning: not in
list.stopwords <- list.stopwords[list.stopwords %nin% letters]

# remove some stopwords that could be relevent for analysis
to.remove <- c("ach", "acht", "achte", "achten", "achter", "achtes", "ag",
               "mensch", "menschen", "natürlich", "ordnung", "recht", 
               "rechte", "rechten", "rechtes")
list.stopwords <- list.stopwords[list.stopwords %nin% to.remove]
rm(to.remove)

# add specific strings to stopwords
list.stopwords <- c(list.stopwords, 
                    "jeweils", "meist", "bzw",  
                    "ggf", "sowieso", "etc", 
                    "ii", "iii", "iv", "vi", "vii", "viii")
str(list.stopwords)

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
  gsub("(-[[:blank:]]und[[:blank:]])([[:upper:]])", " \\2", x, perl=TRUE))

noun.or <- content_transformer(function(x)
  gsub("(-[[:blank:]]oder[[:blank:]])([[:upper:]])", " \\2", x, perl=TRUE))

noun.bzw <- content_transformer(function(x)
  gsub("(-[[:blank:]]bzw.[[:blank:]])([[:upper:]])", " \\2", x, perl=TRUE))

## Create function to remove "-" that separate words at end of lines
hyphen <- content_transformer(function(x) gsub("- ", "", x))

## Implement the word stemmer function based on CISTEM stemmer:
# package: https://github.com/FlorianSchwendinger/cistem/
cistem.stemmer <- content_transformer(function(x) {
  unlist(lapply(x, function(line) {    # unlist the corpus and lapply over the list
    paste(cistem(words(line)), collapse = " ")} )  # paste the words back together.
  )
})

## Define function HarmonizeMisintChars(CHARACTER VECTOR)
# to correct characters that have been misinterpreted during reading of text 
# files, based on encoding table
# This part was taken from Bickel et al. 2017 in Energy, Sustainability and Society
# https://github.com/manuelbickel/semantic_sustainability_assessment 

# read character encoding list
setwd(work.wd); setwd("../"); getwd()
options(encoding = "native.enc")
encoding.table.win <- read.table("tm_win1252_misinterpretation_encoding_table_UTF8.txt",
                                 header = TRUE, sep=";", colClasses = "character",
                                 encoding="UTF-8")

# manual corrections; misinterpreted characters for space might not be read in
# correctly on some machines
encoding.table.win[121,] <- gsub("space", " ", encoding.table.win[121,])
encoding.table.win[121,4] <- c(" ")

HarmonizeMisintChars <- content_transformer(function(x) {
  
  for (i in (1:nrow(encoding.table.win))) {
    
    # unicode encoding, e.g.: U+203A
    gsub(as.character(encoding.table.win[i,1]),
         as.character(encoding.table.win[i,3]), x,  fixed = TRUE)
    
    # windows encoding, e.g.: 0x9B
    gsub(as.character(encoding.table.win[i,2]),
         as.character(encoding.table.win[i,3]), x,  fixed = TRUE)
    
    # windows misinterpreted character symbols
    gsub(as.character(encoding.table.win[i,4]),
         as.character(encoding.table.win[i,3]), x,  fixed = TRUE)
    
    # UTF-8 encoding, e.g.: %E2%80%BA
    gsub(as.character(encoding.table.win[i,5]),
         as.character(encoding.table.win[i,3]), x,  fixed = TRUE)
  }
  
  # correct some unicode characters,
  # see e.g. https://en.wikipedia.org/wiki/Typographic_ligature
  gsub("<U+FB00>", "ff",  x, fixed=TRUE)
  gsub("U+FB00", "ff",  x, fixed=TRUE)
  
  gsub("<U+FB01>", "fi",  x, fixed=TRUE)
  gsub("U+FB01", "fi",  x, fixed=TRUE)
  
  gsub("<U+00DF>", "sz",  x, fixed=TRUE)
  gsub("U+00DF", "sz",  x, fixed=TRUE)
  
  gsub("<U+FB02>", "fl",  x, fixed=TRUE)
  gsub("U+FB02", "fl",  x, fixed=TRUE)
  
  gsub("<U+AB50>", "ui",  x, fixed=TRUE)
  gsub("U+AB50", "ui",  x, fixed=TRUE)
  
  gsub("<U+FB06>", "st",  x, fixed=TRUE)
  gsub("U+FB06", "st",  x, fixed=TRUE)
  
  gsub("<U+FFFD>", "st",  x, fixed=TRUE)
  gsub("U+FFFD", "st",  x, fixed=TRUE)
  
  gsub("<U+FB06>", "st",  x, fixed=TRUE)
  gsub("U+FB06", "st",  x, fixed=TRUE)
  
  #special characters which had been misinterpreted and appear in form of a replacement character
  gsub("<U+FB06>", "",  x, fixed=TRUE)
  gsub("U+FB06", "",  x, fixed=TRUE)
  
  gsub("<U+263A>", "",  x, fixed=TRUE)
  gsub("U+263A", "",  x, fixed=TRUE)
  
  gsub("<U+F0E0>", "",  x, fixed=TRUE)
  gsub("U+F0E0", "",  x, fixed=TRUE)
  
  #return(x)
})

## to parallelize code: ####
#library(foreach)
library(doParallel)

# Calculate the number of cores
no.cores <- detectCores()/2; no.cores

## define objects to be available in all clusters
# clusterExport(cl=cl, varlist=c("cistem.stemmer", "list.stopwords", 
#                                "cistem", "words", "%nin%",
#                                "folder.wd", "s", "yr"))


## Initiate cluster used in parLapply function
#cl <- makeCluster(no.cores); cl

# at the end: give cores free
#stopCluster(cl)

# Initiate cluster used in foreach function
registerDoParallel(no.cores)

# at the end
#stopImplicitCluster()

#- - - - - - - - - - - - - - - - - - - - - - - 
## For loop through all Federal State folders ####
foreach(s=folders, .export = c("cistem", "words", "%nin%", "encoding.table.win"), 
        .packages = c("tm", "tidytext", "cistem")) %dopar% {
          
  #- - - - - - - - - - - - - - - - - - - - - - - 
  ## Transform pdf in text ####
  
  # Part of the script is based on a tutorial available via the following link:
  # https://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/
  
  # Set directory where files are stored
  setwd(paste0(folder.wd, "/", s))
  
  # select meta data for this State only
  temp.meta <- meta.all[meta.all$state==s,]
  
  ## Load/ select files
  all.files <- list.files(pattern = "pdf$")
  #all.files #contains names of pdf files
  
  temp.years <- unique(meta.all[meta.all$state==s,]$year)
  
  ## for loop through all years ####
  for(yr in temp.years){
    
    setwd(paste0(folder.wd, "/", s))
    
    # define files to be load from that state and year
    files <- as.character(temp.meta[temp.meta$year==yr, "document"])
    files <- files[files %in% all.files]  #control step to see if file is in directory
    file.path <- paste0(folder.wd, "/", s, "/", files)
    
    #- - - - - - - - - - - - - - - - - - - - - - - 
    ## Clean the pdf files within a corpus ####
    
    ## For loop over each list element (= files) for cleaning
    corp <- vector("list")
    
    for(i in 1:length(files)){
      
      ## pdf_text function to extract text from pdfs
      # using callr package to avoid full memory space, see
      # https://github.com/ropensci/pdftools/issues/64
      
      corp[[i]] <- callr::r(function(x){pdftools::pdf_text(x)}, args = list(file.path[i]))
      # returns error about unknown characters, should be fine...
      
      #length(corp) #one element per pdf document
      #lapply(corp, length) # each element is a vector that contains the text,
      # the number of vectors is equal to the number of pages
    }
    
    # Create a corpus (database for the text) based on the output list
    corp <- VCorpus(VectorSource(corp))
    
    ## Other way to load text from pdf, but memory error...
    #corp <- Corpus(URISource(files), readerControl = list(reader = readPDF, 
    #                                                       language="de", 
    #                                                       encoding = "UTF-8"))
    
    #corp[[4]]$content  # have a look on the text BEFORE
    
    # Remove footnotes (maybe)
    #to do..., see Bickel et al. 2017, script 05
    
    # Remove whitespace
    corp <- tm_map(corp, stripWhitespace, lazy=F) #faster if lazy=TRUE
    
    # harmonize misinterpreted characters using function defined above
    #corp <- tm_map(corp, HarmonizeMisintChars)
    #... ERROR: can't find encoding.table.win object...?
    
    ## Split nouns separated by and/or but with similar terminus (see)
    corp <- tm_map(corp, noun.and)
    corp <- tm_map(corp, noun.or)
    corp <- tm_map(corp, noun.bzw)
    
    # Case conversion
    corp <- tm_map(corp, content_transformer(tolower), lazy = F)
    
    # remove stopwords
    corp <- tm_map(corp, content_transformer(removeWords), 
                   list.stopwords, lazy=F)
    
    ## Remove hyphen followed by (single) space 
    corp <- tm_map(corp, hyphen, lazy=F)
    
    # Remove numbers
    corp <- tm_map(corp, removeNumbers)
    
    # Remove punctuation
    corp <- tm_map(corp, removePunctuation, ucp = F) #to remove ASCII characters
    #                 preserve_intra_word_dashes = TRUE,
    #                 preserve_intra_word_contractions = TRUE)
    #corp <- tm_map(corp, removePunctuation, ucp = TRUE) #based in Unicode
    
    # Replace mutated vowels (umlaut)
    corp <- tm_map(corp, o, lazy=F)
    corp <- tm_map(corp, o2, lazy=F)
    corp <- tm_map(corp, a, lazy=F)
    corp <- tm_map(corp, u, lazy=F)
    corp <- tm_map(corp, szet, lazy=F)
    
    ## Stemming based on CISTEM stemmer (see above)
    #corp <- tm_map(corp, cistem.stemmer, lazy=F)
    # note: no stemming because of change in strategy: search for 
    # component of words, not for that one specific word
    
    # Maybe: Replace some words, e.g. "Naturschutzgesetz" by NatSchG
    #to do...
    
    ## have a look at the text AFTER cleaning
    #corp[[4]]$content
    
    ## Transform Corpus in tidy data (tibble) ####
    corp.tidy <- tidy(corp)
    
    # Remove remaining punctuation (some ? where created during stemming)
    corp.tidy$text <- gsub(pattern = "\\W", replace = " " ,corp.tidy$text)
    
    # Remove single letters
    corp.tidy$text <- gsub("[[:blank:]][[:alpha:]][[:blank:]]", " ", corp.tidy$text)
    
    # Strip whitespace (again)
    corp.tidy$text <- stripWhitespace(corp.tidy$text)
    
    #- - - - - - - - - - - - - - - - - - - - - - -
    ## Save TDM ####
    setwd(paste0(folder.wd, "/_tdm"))
    save(corp.tidy, file=paste0("Corp_", s, "_",yr, ".RData"))
    
  }
  print(paste(s, "ready"))
  
} # end of foreach loop

stopImplicitCluster()
