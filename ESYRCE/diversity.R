library(reshape2)
###########
setwd("C:/Users/angel/git/R/ESYRCE/")

# Organize categories
source("./categories.R")

# Functions
source("./functions.R")

# Read datasets
dataFolder   = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/"
dataFile     = paste0(dataFolder, "geo_metrics_climate_20-12-18.csv")
df_data      = read.csv(dataFile, header=T)
df_data$region   = abbreviate(df_data$region)
df_data$province = abbreviate(df_data$province)

# alpha diversity (#crops/hectare)
agriColnames = paste0("prop_",agriLand)
baseCols     = c("D1_HUS","D2_NUM","YEA","segArea","segAreaNoWater")
d            = df_data[,c(baseCols,agriColnames)] 
d$alpha      = apply(d[,agriColnames], 1, function(x) sum(x>0)) / d$segAreaNoWater
hist(d$alpha) 
summary(d$alpha)
#subset places with no crop data?
d2 <- subset(d, alpha > 0)
summary(d2$alpha)
hist(d2$alpha) #mÃ¡s 9 crops.
Alpha <- mean(d2$alpha) #0.13

#The temporal turnover in species richness between years
#was calculated for each site (BetaTemporalSite) as:
#the total number of crops found within that site (over the entire study period)
#minus the mean number of crops per year for that site.
#Overall BetaTemporal was calculated as the mean BetaTemporalSite.

#Pool years HACER POR HECTAREA NO POR SEGMENTO
d3 = d2
d3$id <- paste(d3$D1_HUS, d3$D2_NUM, sep = "_")
head(d3)
d3_melted <- melt(d3[,c("YEA",agriColnames,"alpha","id")], id.vars = c("id", "YEA"), variable.name = "crop")
head(d3_melted)
Site <- dcast(d3_melted, id ~ crop, fun.aggregate = sum, na.rm = TRUE, value.var = "value")
head(Site)
# Estimate area segments 
df_segArea    = df_data[,c("D1_HUS","D2_NUM","YEA","segAreaNoWater")]
df_segArea$id = paste(df_segArea$D1_HUS, df_segArea$D2_NUM, sep = "_")
df_segArea    = df_segArea %>% group_by(id) %>% summarise(segArea=mean(segAreaNoWater, na.rm = TRUE))
# Merge Site and df_segArea
SiteArea = merge(Site, df_segArea)
# Get #crops per site
SiteArea$CropxSite <- apply(SiteArea[,agriColnames], 1, function(x) sum(x>0)) / SiteArea$segArea
hist(SiteArea$CropxSite) #up to 1.5 crops over years.
summary(SiteArea$CropxSite) #0.16 over years

#paste mean alpha into Site dataframe.
colnames(d2)
dtemp <- d2[,c("D1_HUS", "D2_NUM", "YEA", "alpha")]
dtemp$id <- paste(dtemp$D1_HUS, dtemp$D2_NUM, sep = "_")
dtemp_melted <- melt(dtemp[,3:ncol(dtemp)], id.vars = c("id", "YEA"), variable.name = "variable")
dtemp2 <- dcast(dtemp_melted, id ~ variable, fun.aggregate = mean, na.rm = TRUE, value.var = "value")
head(dtemp2)
colnames(dtemp2)[2] <- "MeanAlpha"
Site2 <- merge(SiteArea, dtemp2) 
nrow(Site2) == nrow(Site) #Ok

#Calculate BetaTemporalSite
Site2$BetaTemporalSite <- Site2$CropxSite - Site2$MeanAlpha
BetaTemporal <- mean(Site2$BetaTemporalSite)

# Save data  
ids = str_split(Site2$id,"_")
df_ids <- data.frame(matrix(unlist(ids), nrow=length(ids), ncol = 2, byrow=T))
colnames(df_ids) <- c("D1_HUS", "D2_NUM")
Site2$D1_HUS = df_ids$D1_HUS
Site2$D2_NUM = df_ids$D2_NUM
df_diversity = Site2 %>% select(c("D1_HUS", "D2_NUM","alpha","CropxSite","segArea","MeanAlpha","BetaTemporalSite"))

#Spatial turnover (BetaSpatial) was calculated as the total number of crops
#over the entire study period (131) minus the mean number of crops
#per site (over the entire study period).
totalNumCrops = length(which(colSums(df_data[,agriColnames] ) > 0))

# ¿NO DEBERÍAMOS DIVIDIR NÚMERO TOTAL ENTRE ÁREA TOTAL?
BetaSpatial = totalNumCrops - mean(df_diversity$CropxSite)


#Como sabemos si la beta se reduce entre aÃ±os? 
#Pool sites
Year <- dcast(d3_melted, YEA ~ crop, fun.aggregate = sum, na.rm = TRUE, value.var = "value")
head(Year)
Year$CropxYear <- apply(Year[,2:ncol(Year)], 1, function(x) length(which(x > 0)))
summary(Year$CropxYear) #55 over sites!
hist(Year$CropxYear) #up to 56 crops per year over sites
scatter.smooth(Year$CropxYear ~ Year$YEA) #el numero de cultivos incrementa por aÃ±o.

#We can calculate mean betadiv (e.g. Sorensen matrix across sites) per year.
head(d3)
d3[is.na(d3)] <- 0
library(vegan)
dist <- vegdist(x = d3[,4:(ncol(d3)-1)], method = "bray")
#add year column 
dim(d3[4:(ncol(d3)-1)])
str(dist) #25592
rm(d3_melted, d, dtemp_melted, d2, dtemp, dtemp2, Site, Site2, Year)
head(dist)
#Sys.setenv("R_MAX_VSIZE" = 9e9)
#usethis::edit_r_environ("project") #modified manually


#Para cada site
#dist <- vegdist(x = d3[1:1000,4:(ncol(d3)-1)], method = "bray")
#str(dist) #1000
a <- adonis(dist ~ d3$YEA, strata = d3$id) #with 1000 is doable, but crashes anyway.
#subset per year and calc mean?
#meandist <- rowMeans(dist) #but with full matrix!! Now traiangle only.
#mean_year <- tapply(dist, year, mean)

#Look at the 'bigmemory' package, big.matrix function: https://www.rdocumentation.org/packages/bigmemory/versions/4.5.36/topics/big.matrix
#In general, for HPC in R, mostly through Rcpp, https://cran.r-project.org/web/views/HighPerformanceComputing.html
#Shared memory, memory-mapped files and all that stuff


#Notes: We can try also Baselga style metrics separating beta
#We can try also changes in Eveness per time.
#Think on sperating annual / permanent crops Unify crop names referring to the same crop.
#Do spatian Beta per CCAA / Ecoregions?

#I was planning to do a PERMANOVA, but maybe I can create a distance matrix per year, calculate mean distance and regress this with yearâ€¦

