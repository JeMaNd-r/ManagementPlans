#- - - - - - - - - - - - - - - - -#
# Text mining of management plans #
#     author: Romy Zeiss          #
#                                 #
#    0. merge some pdf files      #
#- - - - - - - - - - - - - - - - -#

# For Bavaria, most management plans were split into actions and background information.
# Because this was not the case for all of the Bavarian/RLP plans, we decided to merge
# splitted plans into one pdf file for harmonization.

# Check working directories
work.wd; folder.wd
setwd(work.wd); getwd()

## Load packages
library(staplr)  #to merge pdf files
library(stringr) #for str_detect function

#- - - - - - - - - - - - - - - - - - - - - 
## Merge pdf files of same protected area ####

# Load files
setwd(input.wd)
files <- list.files(pattern = "pdf$") #per decade
files #contains the names of pdf files

# Define pattern to combine files for the same protected area
file.names <- substr(files, 1, 12)  #for Bavarian files
#file.names <- substr(files, 1, 14) #for RLP
file.names

length(files); length(unique(file.names))

# We have to make sure to remove plans that are not split from the input folder 
# (i.e. "_ft_mt_" instead of "ft"/"mt" only, or no double area number)
file.tab <- as.matrix(table(file.names))
head(file.tab)

nodoubles <- file.tab[file.tab!=2,]
nodoubles

# Now, MANUALLY check the respective files, because either their names aren't
# correctly (-> rename them) or they do not have a second part (-> remove those 
# that have no second file from input to output directory).
# (Note: Sometimes, only one of the 2 parts of the management plans was available, 
#        either the action plan (mt) or the background information (fg).
#        However, such plans will be included in analyses since these represent
#        the available information for these protected areas, too.)

# If is says *integer(0)*, you've done everything right and can proceed.

#- - - - - - - - - - - - - - - - - - - - - 
## for loop through all areas ####
file.names <- unique(file.names)

for(i in 1:length(file.names)){
  # Find the two files for same protected area
  temp.files <- files[str_detect(files, str_c(file.names[i], collapse=""))]
  
  # Assign name for output file
  temp.output.name <- paste0(output.wd, "/", file.names[i], "2files_merged.pdf")
  
  # Merge the two pdf files  
  staple_pdf(input_directory = NULL, input_files = temp.files,
             output_filepath = temp.output.name, 
             overwrite = TRUE)
  
  print(paste("Done:", i, "of", length(file.names)))
}

#- - - - - - - - - - - - - - - - - - - - -
## Finally, please check if there are not double files ####

# Load files
setwd(output.wd)
files <- list.files(pattern = "pdf$") #per decade
files #contains the names of pdf files

# Define pattern to combine files for the same protected area
file.names <- substr(files, 1, 12)    #for Bavaria
# file.names <- substr(files, 1, 14)  #for RLP
file.names

length(files); length(unique(file.names))

# We have to make sure to remove plans that are not split from the input folder 
# (i.e. "_ft_mt_" instead of "ft"/"mt" only, or no double area number)
file.tab <- as.matrix(table(file.names))
head(file.tab)

nodoubles <- file.tab[file.tab!=1,]
nodoubles

# Similarly, if is says *integer(0)*, you've done everything right and are done.

