#- - - - - - - - - - - - - - - - -#
# Text mining of management plans #
#     author: Romy Zeiss          #
#                                 #
#     4. plot Germany & plans     #
#- - - - - - - - - - - - - - - - -#

# check working directories
work.wd; figu.wd

library(sp)

### load the German  geo map polygons
setwd(work.wd)
map.g <- readRDS("../DEU_adm1.rds")  

# German shape file: http://biogeo.ucdavis.edu/data/gadm2.8/rds/DEU_adm1.rds

# sample "clicks" data with German state coded as ID_1
sample <- data.frame( 
  ID_1 =    c( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16L), 
  clicks =  c(555, 209, 21, 295, 10, 14, 613, 207, 2, 497, 106, 124, 63, 270, 341, 180L)) 

# Merge sample data with geo map data
final <- merge(x =map.g@data, y = sample, by = "ID_1", all.y = TRUE)
map.g@data <- data.frame(my.data@data, 
                           sample[match(map.g@data[,"ID_1"], 
                                        sample[,"ID_1"]),])

# German language hick-ups need to be resolved
enamessp <- gsub("?", "ue", map.g@data$NAME_1)
map.g@data$NAME_1 <- enamessp

# print out states and clicks (sorted high to low) for verification
final[ order(final$clicks),c("ID_1","NAME_1","HASC_1","clicks") ]

# insert the newly created clicksvariable into the spatial data frame
map.g$clicks <- final$clicks
print(sample[ order(sample$clicks), ])

# clrs <- c("#FFFFE5", "#FFFFE5", "#FFFFE5", "#FFFFE5", # lower than 100
#           "#FFF7BC", # 100-200
#           "#FEE391", "#FEE391", "#FEE391", "#FEE391", #200-300
#           "#FEC44F", "#FEC44F", "#FEC44F", "#FEC44F", #300-400
#           "#FE9929", "#EC7014", "#CC4C02", "#8C2D04") # >400

clrs <- RColorBrewer::brewer.pal(n = 8, name = "Greens")

#setwd(figu.wd); pdf("")
spplot(map.g, zcol = "clicks", main = "Plans per State", 
       at = c(0, 30, 100, 200, 300, 400, 500, 620), col.regions=clrs)
#... work on colors...



