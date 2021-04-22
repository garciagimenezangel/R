library(geosphere)
library(dplyr)
library(rlist)

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
