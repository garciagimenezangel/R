library(geosphere)
library(dplyr)
library(rlist)
rm(list=ls())
getClosestDays = function(data) {
  values = data$dl
  mean   = mean(values)
  doys   = data$doy
  refDoy = c()
  for(i in seq(2,length(values))) {
    if( ((values[i-1] - mean >= 0) & (values[i] - mean <= 0)) | ((values[i-1] - mean <= 0) & (values[i] - mean >= 0)) ) {
      refDoy = c(refDoy, doys[i-1])
    } 
  }
  if (length(refDoy) == 1) {
    uniqueDoy = T
  } else {
    uniqueDoy = F
    refDoy = refDoy[1]
  }
  return(tibble::tibble(refDoy = refDoy, uniqueDoy=uniqueDoy))
}


testLatitudes = c(-75,-70,-65,-60,-45,-30,-15,-10,-5,0,5,10,15,30,45,60,65,70,75)
df_closestDoy = data.frame(month=seq(1,12))
for (lat in testLatitudes) {
  df_daylength_lat        = expand.grid(lat,seq(1,365))
  names(df_daylength_lat) = c("lat","doy")
  df_daylength_lat$dl     = daylength(lat, 1:365)
  df_daylength_lat$month  = rep(c(1,2,3,4,5,6,7,8,9,10,11,12),c(31,28,31,30,31,30,31,31,30,31,30,31))
  closestDOY = df_daylength_lat %>% group_by(month) %>% do(data.frame(getClosestDays(.)))
  names(closestDOY) = c("month", paste0("refDoy_",lat), paste0("uniqueRefDoy_",lat))
  df_closestDoy = merge(df_closestDoy, closestDOY)
}

# Get reference days:
refDoys = df_closestDoy$refDoy_45
df_error = data.frame(month=seq(1,12))
for (lat in testLatitudes) {
  columnMeanActual             = paste0("mean_actual_lat",lat)
  columnMeanRefDoys            = paste0("mean_refdoys_lat",lat)
  columnDiff                   = paste0("diff_lat",lat)
  df_error[,columnMeanActual]  = NA
  df_error[,columnMeanRefDoys] = NA
  df_daylength_lat             = expand.grid(lat,seq(1,365))
  names(df_daylength_lat)      = c("lat","doy")
  df_daylength_lat$dl          = daylength(lat, 1:365)
  df_daylength_lat$month       = rep(c(1,2,3,4,5,6,7,8,9,10,11,12),c(31,28,31,30,31,30,31,31,30,31,30,31))
  for (month in seq(1,12)) {
    refDoy1                                              = refDoys[month]
    refDoy2                                              = refDoys[month]+1
    dlRefDoys                                            = daylength(lat, c(refDoy1, refDoy2))
    dlRefMean                                            = mean(dlRefDoys)
    dlActualMean                                         = mean(df_daylength_lat[df_daylength_lat$month == month, ]$dl)
    dlDiff                                               = abs(dlActualMean-dlRefMean)
    df_error[df_error$month == month, columnMeanActual]  = dlActualMean
    df_error[df_error$month == month, columnMeanRefDoys] = dlRefMean
    df_error[df_error$month == month, columnDiff]        = dlDiff
  }
}
df_error = df_error[,order(colnames(df_error))]
write.csv(df_error, file="diffDaylengthActualRefDoys.csv", row.names = F)
