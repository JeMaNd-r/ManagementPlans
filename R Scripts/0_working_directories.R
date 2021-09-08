#- - - - - - - - - - - - - - - - -#
# Text mining of management plans #
#     author: Romy Zeiss          #
#                                 #
#      working directories        #
#- - - - - - - - - - - - - - - - -#

# Set working directory
folder.wd <- "I:/eie/==PERSONAL/RZ SoilBON/RZ ManagementPlans"
work.wd <- ("~/Text mining MaP/R Scripts")
stopword.wd <- ("~/GitHub/stopwords-de/stopwords-de.txt")
git.wd <- ("~/GitHub/")
figu.wd <- ("~/Text mining MaP/Figures")
setwd(work.wd); getwd()

## working directories to merge pdfs
# for Bavaria
input.wd <- "I:/eie/SoilBON/ManagementPlans/Bavaria/to convert"
output.wd <- "I:/eie/SoilBON/ManagementPlans/Bavaria"

# for Rhineland-Palatinate (RLP)
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
folders 
# Note: Lower Saxony has do be removed if the focus is on comparing Federal States, 
# as LowSax only offered 2 management plans currently.

#- - - - - - - - - - - - - - - - - - - - - - -
## Define years to go through in separate loop
# otherwise, R will likely not handle all of the files per State simultaneously
# Note: can run only after creating meta data file (metadata script)

# first, load meta data (publishing year of management plans)
setwd(folder.wd)
meta.all <- read.csv("Document_Year_all_filled.csv")

# extract years 
years <- unique(meta.all$year)
#years <- 2016:2020
years <- years[order(years)]; years

## Define decades (or half-decades)
decades <- c(2010, 2015, 2020); decades
#decades <- 2020
deca.seq <- rep(decades, c(10,5,5)); deca.seq
#deca.seq <- rep(decades, 5)

# merge years and decades
decades; deca.seq; years
yr.deca <- as.data.frame(cbind(years, deca.seq)); yr.deca
rm(meta.all)

#- - - - - - - - - - - - - - - - - - - - - - -
## Load keywords ####
setwd(work.wd); getwd()

thesauri <- read.csv("../Thesauri.csv")
soil.kw <- as.vector(thesauri[thesauri$soil.term=="yes", "regex"]); soil.kw

thesauri <- thesauri[order(nchar(as.character(thesauri$word)), decreasing=T),]
keywords <- thesauri$word; keywords
#wordlist <- unique(thesauri$subcategory)

# get categories to all terms, and order subcategories by category
categories <- unique(thesauri[, c("category", "subcategory")])
categories <- categories[order(categories$subcategory),]
categories <- categories[order(categories$category),]

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
#...



