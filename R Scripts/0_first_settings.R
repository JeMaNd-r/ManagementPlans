#- - - - - - - - - - - - - - - - -#
# Text mining of management plans #
#     author: Romy Zeiss          #
#                                 #
#      working directories        #
#- - - - - - - - - - - - - - - - -#

# Set working directory
folder.wd <- "I:/eie/==PERSONAL/RZ SoilBON/RZ ManagementPlans"
work.wd <- ("I:/eie/==PERSONAL/RZ SoilBON/RZ ManagementPlans/Text mining MaP/R Scripts")
stopword.wd <- ("~/GitHub/stopwords-de/stopwords-de.txt")
git.wd <- ("~/GitHub/")
figu.wd <- ("I:/eie/==PERSONAL/RZ SoilBON/RZ ManagementPlans/Text mining MaP/Figures")
setwd(work.wd); getwd()

## working directories to merge pdfs
# for Bavaria
input.wd <- "I:/eie/SoilBON/ManagementPlans/Bavaria/to convert"
output.wd <- "I:/eie/SoilBON/ManagementPlans/Bavaria"

# for Rhineland-Palatinate
input.wd <- "I:/eie/SoilBON/ManagementPlans/RLP/to convert"
output.wd <- "I:/eie/SoilBON/ManagementPlans/RLP"

#- - - - - - - - - - - - - - - - - - - - - - -
## Set some objects frequently used in following scripts ####
#- - - - - - - - - - - - - - - - - - - - - - -
## Set folder directory to list Federal States (folders)
setwd(folder.wd); getwd() 
#folders <- list.dirs(full.names = F)  #list of directories to get State's names
folders <- c("Bavaria", "BaWue", "Berlin", "Brandenburg", "Bremen", 
             "Hamburg", "Hessen", "LowSax", "MeckPom", "NRW", "RLP", 
             "Saarland", "SaxAnh", "Saxony", "SchleHol", "Thuringia" )
#folders 
# Note: Lower Saxony has do be removed if the focus is on comparing Federal States, 
# as LowSax only offered 2 management plans currently.

#- - - - - - - - - - - - - - - - - - - - - - -
## Define years to go through in separate loop
# otherwise, R will likely not handle all of the files per State simultaneously
# Note: can run only after creating meta data file (metadata script)

# first, load meta data (publishing year of management plans)
setwd(work.wd)
meta.all <- read.csv("../Document_Year_all_filled.csv")

# extract years 
years <- unique(meta.all$year)
years <- years[order(years)]; years

## Define decades (or half-decades)
decades <- c(2010, 2015, 2020); decades
deca.seq <- rep(decades, c(10,5,5)); deca.seq

# merge years and decades
decades; deca.seq; years
yr.deca <- as.data.frame(cbind(years, deca.seq)); yr.deca

#- - - - - - - - - - - - - - - - - - - - - - -
## Load keywords ####
setwd(work.wd); getwd()

thesauri <- read.csv("../Thesauri.csv")
soil.kw <- as.vector(thesauri[thesauri$soil.term=="yes", "regex"])
#soil.kw

thesauri <- thesauri[order(nchar(as.character(thesauri$word)), decreasing=T),]
keywords <- thesauri$word
#keywords
#wordlist <- unique(thesauri$subcategory)

# get categories to all terms, and order subcategories by category
categories <- unique(thesauri[, c("category", "subcategory")])
categories <- categories[order(categories$subcategory),]
categories <- categories[order(categories$category),]
#setwd(work.wd); write.csv(categories, file="../Categories.csv", row.names=F)

categories <- read.csv("../Categories.csv"); head(categories)

#- - - - - - - - - - - - - - - - - - - - - - -
## Get number of plans overall and per decade ####
# load meta data file with documents and their publication year
setwd(folder.wd)
meta.all <- read.csv("Document_Year_all_filled.csv")

# count number of documents per year
temp.d <- as.data.frame(table(meta.all$year))

# asign decade to years and sum up number of documents per decade
temp.d$deca <- yr.deca$deca.seq
temp.d <- aggregate(Freq~deca, temp.d, sum)

# create data frame with number of documents per decade
no.doc <- data.frame(deca=decades, n=temp.d$Freq); no.doc

# remove temporal tabels
rm(temp.d); rm(meta.all)

#- - - - - - - - - - - - - - - - - - - - - - -
## necessary packages ####
# Check if all packages are installed.
# They will be loaded again in the respective script.
library(tidyverse)

# Merge some files
library(staplr)   # to merge pdf files
library(stringr)  # for str_detect function

# Extract metadata
library(tabulizer) #to extract metadata from pdf files

# Extract and clean text from pdf
library(pdftools)  #to convert pdf in csv files
library(tm)        #for text mining
library(tidytext)  #to transform Corpus in tidy data
library(callr)     #to avoid error with memory space

# Get package for German stemming from GitHub
#setwd(git.wd); install.packages("cistem", repos = NULL, type = "source")
library(cistem)    #for German stemming

# Create term-document matrix and tokens
library(dplyr)     #to merge data frames
library(tm)        #for text mining
library(stringr)   #to split text into chunks
library(tidytext)  #to work with tibble and pipe (%>%)

# Megre co-occurrences and bigrams
library(data.table) #to aggregate (i.e. sum up) double entries of same bi-grams
library(EnvStats)   #to detect outliers
library(naniar)     #to replace outliers with NAs

## to parallelize code
library(doParallel)

# Plot policy timeline
library(lubridate)  #to transform date columns in one column

# Plot word networks, barplots etc.
library(influential) #for word network
library(igraph)      #for word network
library(reshape2)
library(ggraph)      #for word network
library(wordcloud)   #make word clouds
library(psych)       #calculate confidence intervals
library(ggpubr)      #combine multiple plots in one figure
  
# Plot Germany with Federal States
library(sp)

