#- - - - - - - - - - - - - - - - -#
# Text mining of management plans #
#     author: Romy Zeiss          #
#                                 #
#     2. define key words         #
#- - - - - - - - - - - - - - - - -#

# Assign working directories
folder.wd; work.wd; figu.wd
setwd(work.wd); getwd()

# Load packages
library(stringr)   #for str_detect & str_extract functions
library(dplyr)     #to manage data frames, e.g. add_row()

#- - - - - - - - - - - - - - - - - - - - - - - 
## Run preparation script ####
#source("1c_create_tdm_manualCleaning.R")

setwd(folder.wd)

#- - - - - - - - - - - - - - - - - - - - - - -
## Extract keywords from all documents

# List files with cleaned text, stored in data frames per year and State
setwd(paste0(folder.wd, "/_tdm")); getwd()

# create empty vector to store keywords from all years
keywords <- c()

## For loop through all years ####
for(yr in years){
  
  files <- list.files(pattern = paste0(yr,".RData$"))
  #files #contains names of pdf files
  
  # Load files into one data frame
  # first, create output data frame
  load(paste0("Corp_Bavaria_2019.RData"))
  corp <- corp.tidy[0,]  #take headers only
  
  # for loop through all files (i.e. Federal States)
  for(i in files){
    try({  # ignores errors, i.e. if no document in specific year published
      load(i)
      corp <- add_row(corp, corp.tidy)  # merge with output data
    })
  }
  head(corp)
  
  #- - - - - - - - - - - - - - - - - - - - - - -
  ## Define keywords based on words occurring in management plans ####
  
  # Get all words containing "boden" ("Boden" = soil) from cleaned text
  #y <- unlist(str_extract_all(corp$text, "bod[A-Za-z]+")) #at beginning of word
  y <- unlist(str_extract_all(corp$text, "[A-Za-z]*boden[A-Za-z]*")) #in-between & at beginning of words 
  keywords <- unique(c(keywords, y))
  
  print(paste0(yr, " ready"))
}

# remove non-relevant terms
keywords <- keywords[str_detect(keywords, "http")==F]
keywords <- keywords[str_detect(keywords, "html")==F]
keywords <- keywords[str_detect(keywords, "www")==F]

# remove proper names
#keywords <- keywords[str_detect(keywords, "bodd")==F] # note: "Bodden" means briny bodies of water often forming lagoons
keywords <- keywords[str_detect(keywords, "bodensee")==F] #Bodensee = Lake Constance in southern Germany

# remove German city names provided in https://www.namen-liste.de/orte-b/
keywords <- keywords[str_detect(keywords, "bodenheim")==F]
keywords <- keywords[str_detect(keywords, "bodenkirchen")==F]
keywords <- keywords[str_detect(keywords, "bodenwerder")==F]
keywords <- keywords[str_detect(keywords, "bodenwohr")==F]  #Bodenwöhr
keywords <- keywords[str_detect(keywords, "bodenwerder")==F]

# remove German street names provided in https://www.namen-liste.de/strassen-b/
keywords <- keywords[str_detect(keywords, "bodenbach")==F]
keywords <- keywords[str_detect(keywords, "bodenberg")==F]
#keywords <- keywords[str_detect(keywords, "bodener")==F]      #Bodener street, not included but words like "bodenerhaltung"
#keywords <- keywords[str_detect(keywords, "bodenweg")==F]     #not included, checked by ==T
#keywords <- keywords[str_detect(keywords, "bodenstrasze")==F] #not included

#keywords <- keywords[!is.na(keywords)]  #remove NAs if necessary
keywords <- keywords[order(keywords)]
keywords #gives respective keywords, ~4359 terms

#- - - - - - - - - - - - - - - - - - - - - - -
## Save keywords ####
setwd(work.wd)
#write.csv(keywords, "../List_keywords_raw.csv", row.names = F)
keywords <- read.csv("../List_keywords_raw.csv")
keywords <- keywords$x; keywords

# Then, we manually defined categories for these soil keywords, and translated them.
# In addition, thesauri with more keywords such as drivers of soil or threats will
# be defined and clustered into categories.
# We will load the file containing all this information & use it in the following.

#- - - - - - - - - - - - - - - - - - - - - - -
## Load thesauri (keywords with their categories) ####
setwd(work.wd)
thesauri <- read.csv("../Thesauri.csv")
head(thesauri)

unique(thesauri$category)

## Plot thesaurus categories as mind map ####
setwd(figu.wd)
require(visNetwork, quietly = TRUE)
categories <- unique(thesauri[, c("category", "subcategory")])

nodes <- data.frame(id = unique(c(categories$category, categories$subcategory)),
                    label = unique(c(categories$category, categories$subcategory)),
                    shape = "box")
edges <- data.frame(from = categories$category, to = categories$subcategory)
network <- visNetwork(nodes, edges, width = "100%")
visSave(network, file = "Thesauri_mindmap_categories.html")

# now each single category with subcategories & terms
t <- 0
setwd(figu.wd)

for(i in unique(thesauri$category)){
  temp.thesauri <- thesauri[thesauri$category==i,]
  temp.color <- t+1  #doesn't work...
  nodes <- data.frame(id = unique(c(temp.thesauri$word, temp.thesauri$subcategory, i)),
                      label = unique(c(temp.thesauri$word, temp.thesauri$subcategory, i)),
                      shape = "box", color = temp.color)
  edges <- data.frame(from = c(temp.thesauri$word, unique(temp.thesauri$subcategory)), 
                      to = c(temp.thesauri$subcategory, rep(i, length(unique(temp.thesauri$subcategory)))))
  
  temp.network <- visNetwork(nodes, edges, width = "100%") #%>%
  #visGroups(groupname = "A", color = "darkblue")
  visSave(temp.network, file = paste0("Thesauri_mindmap_", i, ".html"))
}

