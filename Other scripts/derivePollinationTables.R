rm(list=ls())
abundanceFlowersTable = "C:/Users/angel.gimenez/Google Drive/PROJECTS/OBSERV/Lookup Tables/Abundance_flowers_location.csv"
dfAbsFlowers = read.csv(file = abundanceFlowersTable, header = TRUE)

# CORINE level 3
clean = dfAbsFlowers[dfAbsFlowers$CORINE.Level.3 != "?",c("Number","CORINE.Level.3")]
clean = clean[complete.cases(clean),]
clean$Number = as.numeric(clean$Number)
clean$CORINE.Level.3 = as.character(clean$CORINE.Level.3)
# Mean
mean.clean <- aggregate(x = clean, 
              by = list(clean$CORINE.Level.3), 
              FUN = mean)
names(mean.clean) <- c("CORINE.Level.3","mean")
mean.clean = mean.clean[,c("CORINE.Level.3","mean")]
# Std
sd.clean <- aggregate(x = clean, 
                      by = list(clean$CORINE.Level.3), 
                      FUN = sd)
names(sd.clean) <- c("CORINE.Level.3","sd")
sd.clean = sd.clean[,c("CORINE.Level.3","sd")]
merged = merge(mean.clean, sd.clean)
# Coefficient
merged$coeff = merged$mean/max(merged$mean)
write.csv(merged, file = "C:/Users/angel.gimenez/Google Drive/PROJECTS/OBSERV/Lookup Tables/Abundance_flowers_MeanStd_level3.csv", row.names = FALSE)


# CORINE level 2
clean = dfAbsFlowers[dfAbsFlowers$CORINE.Level.2 != "?",c("Number","CORINE.Level.2")]
clean = clean[complete.cases(clean),]
clean$Number = as.numeric(clean$Number)
clean$CORINE.Level.2 = as.character(clean$CORINE.Level.2)

# Mean
mean.clean <- aggregate(x = clean, 
                        by = list(clean$CORINE.Level.2), 
                        FUN = mean)
names(mean.clean) <- c("CORINE.Level.2","mean")
mean.clean = mean.clean[,c("CORINE.Level.2","mean")]
# Std
sd.clean <- aggregate(x = clean, 
                      by = list(clean$CORINE.Level.2), 
                      FUN = sd)
names(sd.clean) <- c("CORINE.Level.2","sd")
sd.clean = sd.clean[,c("CORINE.Level.2","sd")]
merged = merge(mean.clean, sd.clean)
# Coefficient
merged$coeff = merged$mean/max(merged$mean)
write.csv(merged, file = "C:/Users/angel.gimenez/Google Drive/PROJECTS/OBSERV/Lookup Tables/Abundance_flowers_MeanStd_level2.csv", row.names = FALSE)

