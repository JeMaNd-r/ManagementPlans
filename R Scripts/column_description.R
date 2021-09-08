#given a dataframe, this function returns another dataframe that provides general information on the data provided

col_descr <- function(df){
  descr <- data.frame(Column_names=names(df))
  #datatype
  descr$Datatype <- sapply(df, typeof)
  descr$Dataclass <- sapply(df, class)
  
  #if numeric, gives the range. if factor or character, number of levels (or unique values)
  descr$Range <- NA
  for (i in 1:nrow(descr)) {
    if (descr$Dataclass[i]=="numeric" | descr$Dataclass[i]=="integer") {
      descr$Range[i] <- paste0(signif(range(df[i], na.rm=TRUE), digits = 4), collapse = " - ")
    } else if (descr$Dataclass[i]=="factor" | descr$Dataclass[i]=="character") {
      descr$Range[i] <- paste(length(unique(df[[i]])), "levels")
    } else {descr$Range[i] <- NA}
  }
  descr$Perc_complete <- round(colSums(!is.na(df))/nrow(df)*100, 1)
  return(descr)
}



# example -----------------------------------------------------------------

# col_descr(iris)
# View(col_descr(iris))
