setwd("~/Documents/DATA/FAOSTAT")
df = read.csv(file = "", header = TRUE)

df$cropsMonfreda = as.character(df$cropsMonfreda)
df$CropFAOSTAT = as.character(df$CropFAOSTAT)
for (crop in df$CropFAOSTAT) {
  if (crop %in% df$cropsMonfreda) {
    print(paste0("FAOSTAT: ",crop, " is in Monfreda"))
  }
}