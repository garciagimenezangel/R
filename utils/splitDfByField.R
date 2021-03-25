library(rlist)
library(dplyr)
# The goal is to divide a df so that unique values of a given field don't have more rows than certain limit.
# For example, field='sampling_year', divide df into several df's, so that the number of rows for any 'sampling_year' in every df is < 1000.

splitDfByYear = function(df, field, limit) {

  # For each year, create a list of subsets of df, each with number of rows below the limit. 
  listOfLists = list()
  uniqueFieldVals = unique(df[,field])
  for (fieldVal in uniqueFieldVals) {
    listByYear = list()
    dfField = df[df[,field] == fieldVal,] 
    n = nrow(dfField)
    while (n > limit) {
      dfSel = dfField[1:limit,]                                     # select a number of rows equal to 'limit'
      listByYear = list.append(listByYear, dfSel)                   # add to the list
      dfField = dfField[!row.names(dfField) %in% row.names(dfSel),] # removed saved elements from dfField
      n = nrow(dfField)
    }
    listByYear = list.append(listByYear, dfField)
    listOfLists = list.append(listOfLists, listByYear)
  }
  
  # Bind the i'th element of each list. As a result, we'll get dataframes where each unique value of the selected field has multiplicity lower than the selected limit
  maxInd = max(do.call(cbind, lapply(listOfLists, function(l){ length(l) })))
  indSlice = 1
  listSlices = list()
  while(indSlice<=maxInd) {
    dfSlice = do.call(rbind, lapply(listOfLists, function(l, i){ 
      m = length(l)
      if (i<=m) return(l[[i]]) 
    }, i=indSlice))
    listSlices = list.append(listSlices, dfSlice)
    indSlice = indSlice + 1
  }

# Sanity check: dfTest should be equal to df
# dfTest = do.call(rbind, lapply(listSlices, function(l){ return(l) } ))
  
  return(listSlices)
}