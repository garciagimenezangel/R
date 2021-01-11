for(i in seq(2001,2019)) {
  nSeg = nrow(df_data[df_data$YEA == i,])
  print(paste("Year:",i,"n:",nSeg))
}

for(i in seq(2001,2019)) {
  dataYear = df_data[df_data$YEA == i,]
  area = sum(dataYear$segArea)
  print(paste("Year:",i,"Area:",area))
}