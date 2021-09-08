#- - - - - - - - - - - - - - - - -#
# Text mining of management plans #
#     author: Romy Zeiss          #
#                                 #
#      0. get meta data           #
#- - - - - - - - - - - - - - - - -#

# Assign working directories
work.wd; folder.wd 
setwd(work.wd); getwd()

# Load packages
library(dplyr)  #to merge data frames at the end

## Install tabulizer package - if neccessary
#install.packages("rJava")
#library(rJava) 
#install.packages("devtools")

#devtools::install_github("ropensci/tabulizer", args="--no-multiarch")
#devtools::install_github("ropensci/tabulizerjars", args="--no-multiarch") 

library(tabulizer) #to extract metadata from pdf files

#- - - - - - - - - - - - - - - - - - - - - - - 
## For loop through all Federal State folders ####
# We want to sort all documents by year instead of Federal State. 
# Therefore, we will extract the meta data to get the year of creation.
setwd(folder.wd); getwd() 
folders

# Now, it'll take ~5 minutes per Federal State (16 in total)...
for(s in folders){
  # Assign working directory (folder of Federal State)
  file.wd <- paste0(folder.wd, "/", s)
  
  #- - - - - - - - - - - - - - - - - - - - - - - 
  ## Get metadata of pdf files (e.g. date) ####
  setwd(file.wd)
  print(getwd()) # to see progress
  
  ## load/ select files
  files <- list.files(pattern = "pdf$")
  #files #contains names of pdf files
  
  # for loop to extract year and name for each file
  meta.data <- data.frame(document=files, date=NA, title=NA, state=NA)
  for(i in 1:length(files)){
    try({   # try() ignores errors & goes on with the next pdf file
      meta.temp <- extract_metadata(files[i])
      meta.data[i,"date"] <- meta.temp$created  #gives a string with exact time & date of creation
      #meta.data[i,"title"] <- meta.temp$title  #gives the title, sometimes more detailed then file name
      meta.data[i, "state"] <- s
    })
  } #reminder: errors are ignored by try(), typically caused by no data for date 
  #head(meta.data)
  
  # extract the last four characters of the date string (i.e. the year)
  meta.data$year <- stringr::str_sub(meta.data$date, - 4, - 1) 
  head(meta.data[,c("document","year", "state")])
  
  #- - - - - - - - - - - - - - - - - - - - - - - 
  ## Save year when files are published ####
  setwd(folder.wd)
  
  ## Detect and save missing years 
  if(nrow(meta.data[which(is.na(meta.data$date)),])!=0) 
    write.csv(meta.data[which(is.na(meta.data$date)),], 
              paste0("Document_Year_", s, "_missing.csv"), row.names = F)
  # fill out file manually at the end...
  
  #head(meta.data[!is.na(meta.data$year),c("document", "year", "state")])
  write.csv(meta.data[!is.na(meta.data$year),c("document", "year", "state")], 
            paste0("Document_Year_", s, ".csv"), row.names = F)
  
}

#- - - - - - - - - - - - - - - - - - - - - - - 
## Combine meta data files of all States ####
setwd(folder.wd)
meta.all <- data.frame()
meta.missing <- data.frame()

for(s in folders){
  m.temp <- read.csv(paste0("Document_Year_", s, ".csv"))
  meta.all <- bind_rows(meta.all, m.temp)
  
  # and for files with the missing data
  try({
    mm.temp <- read.csv(paste0("Document_Year_", s, "_missing.csv"))
    mm.temp$state <- rep(s, nrow(mm.temp))
    meta.missing <- bind_rows(meta.missing, mm.temp)
  })
}

head(meta.all)
head(meta.missing)

## Save meta data file of all documents
setwd(folder.wd)
#write.csv(meta.all, "Document_Year_all.csv", row.names=F)
#write.csv(meta.missing, "Document_Year_all_missing.csv", row.names=F)

#- - - - - - - - - - - - - - - - - - - - - - - 
## Combine manually checked meta data with others ####
setwd(folder.wd)
meta.all <- read.csv("Document_Year_all.csv")
meta.miss.fill <- read.csv("Document_Year_all_missing_filled.csv")

meta.complete <- bind_rows(meta.all, meta.miss.fill)
head(meta.complete)

## Save meta data file of really all documents
setwd(work.wd)
#write.csv(meta.complete, "../Document_Year_all_filled.csv", row.names=F)
