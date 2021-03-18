#ESYRCE:

#Calcular no solo alpha diversidad de cultivos, 
  #pero también beta y gamma (por año)… Usamos tylianakys partitioning method? 


#Notes: 
#SHOULD WE Unify crop names referring to the same crop.

#Read data----

library(reshape2)
#d <- read.csv("clean_data/cropAreaByComAut.csv") 
d <- read.csv("clean_data/cropAreaByProvince.csv") #Can be done per province
head(d)
colnames(d)[2] <- "region"
unique(d$region) #ctl? CHECK ANGEL!

#Calculate alpha diversity of crops.----
#Following Tylianakis et al 2005 Ecology
#Alpha diversity was defined as the mean number of crops per region per year
#Make binary
colnames(d)
d0 <- ifelse(d[,4:134] > 0, 1, 0)
head(d0)
d <- cbind(d[,1:2], d0)
head(d)
#Calculate alpha per region and year
d$alpha <- apply(d0, 1, function(x) sum(x))
hist(d$alpha) #10 to 120 crops per region and year.
summary(d$alpha)
Alpha <- mean(d$alpha) #57 per ccaa #43 per province

#The temporal turnover in species richness between years
#was calculated for each site (BetaTemporalSite) as:
#the total number of crops found within that site (over the entire study period)
#minus the mean number of crops per year for that site.
#Overall BetaTemporal was calculated as the mean BetaTemporalSite.
#Pool years
d3 <- d
head(d3)
d3_melted <- melt(d3[,c(1:ncol(d3)-1)], id.vars = c("region", "YEA"), variable.name = "crop")
head(d3_melted)
#crops per region pooled
Region <- dcast(d3_melted, region ~ crop, fun.aggregate = sum, na.rm = TRUE, value.var = "value")
head(Region)
Region$CropxRegion <- apply(Region[,2:ncol(Region)], 1, function(x) length(which(x > 0)))
hist(Region$CropxRegion) #up to 140 crops over years.
summary(Region$CropxRegion) 
#paste mean alpha into Region dataframe (mean number of crops per year for that site)
colnames(d)
dtemp <- d[,c(1,2,134)]
head(dtemp)
dtemp2 <- dcast(dtemp, region ~ YEA, fun.aggregate = mean, na.rm = TRUE, value.var = "alpha")
head(dtemp2)
str(dtemp2)
dtemp2$meanCropxYear <- rowMeans(dtemp2[,2:20], na.rm = TRUE)
Region2 <- merge(Region, dtemp2) 
nrow(Region2) == nrow(Region) #Ok
#Calculate BetaTemporalSite
Region2$BetaTemporalSite <- Region2$CropxRegion - Region2$meanCropxYear
#plot(Region2$BetaTemporalSite ~ Region2$region) #we can see which regions have more temporal betadiversity here
#ANGEL; CAN YOU DO A NICE MAP PER PROVINCE?
BetaTemporal <- mean(Region2$BetaTemporalSite)

#Spatial turnover (BetaSpatial) was calculated as the total number of crops
#over the entire study period minus the mean number of crops
#per site (over the entire study period).
head(d)
BetaSpatial = ncol(d)-3 - mean(Region2$CropxRegion)

#Therefore, the overall diversity of a habitat type can be described as 
#gamma = alpha + BetaTemporal + BetaSpacial 
#As alpha and BetaTemporalSite were replicated across sites, 
#they were analyzed as a proportion of gamma
Alpha + BetaTemporal + BetaSpatial #Overall 131 possible crops, which is what I expect gamma to be.

#questions:
#1) which is the temporal and spatial diversity?
Alpha + BetaTemporal + BetaSpatial
#2) has betadiversity across regions (spatial) changed over time?
#For this we repeat steps above, but pooling over years, and not over regions
Yearly <- dcast(d3_melted, YEA ~ crop, fun.aggregate = sum, na.rm = TRUE, value.var = "value")
head(Yearly)
Yearly$CropxYear <- apply(Yearly[,2:ncol(Yearly)], 1, function(x) length(which(x > 0)))
hist(Yearly$CropxYear) #up to 140 crops over years.
summary(Yearly$CropxYear) 
plot(Yearly$YEA, Yearly$CropxYear)
#paste mean alpha into Region dataframe (mean number of crops per year for that site)
head(dtemp)
dtemp3 <- dcast(dtemp, YEA ~ region, fun.aggregate = mean, na.rm = TRUE, value.var = "alpha")
head(dtemp3)
dtemp3$meanCropxRegion <- rowMeans(dtemp3[,2:16], na.rm = TRUE)
Yearly2 <- merge(Yearly, dtemp3) 
nrow(Yearly2) == nrow(Yearly) #Ok
#Calculate BetaSiteTemporal
head(Yearly2)
Yearly2$BetaSiteTemporal <- Yearly2$CropxYear - Yearly2$meanCropxRegion
plot(Yearly2$BetaSiteTemporal ~ Yearly2$YEA, las = 2, xlab = "Year", ylab = "Spatial turnover") #beta spatial increase with time (but asimptotes)
plot(Yearly2$CropxYear ~ Yearly2$YEA, las = 2, xlab = "Year", ylab = "Crops per year") #el numero de cultivos incrementa por año.


##########
## PLOTS
##########
library(dplyr)
library(sf)
library(ggplot2)
library(rasterVis)
library(wesanderson)
# Regions
# fileShp = "C:/Users/angel.gimenez/DATA/Administrative areas/ESP_adm/ESP_adm2.shp"
fileShp = "C:/Users/angel/DATA/Administrative areas/ESP_adm/ESP_adm2.shp"
polys <- st_read(fileShp)
# Crop to peninsula and Baleares
box = c(xmin=-9.271338, xmax=4.308773, ymin=36.04188, ymax=43.76652)
polys = st_crop(polys, st_bbox(box)) 
polys$region = abbreviate(polys$NAME_1) # abbreviate names
polys$province = abbreviate(polys$NAME_2) # abbreviate names
# Settings
name = paste0("Temporal \nBeta \nDiversity")
df_beta = Region2[,c("region","BetaTemporalSite")]
colnames(df_beta) = c("province","BetaTemporalSite")
sf_beta = merge(polys,df_beta)
pal <- wes_palette("Zissou1", 100, type = "continuous")
# Plot
fBeta = ggplot(sf_beta) +
    geom_sf(data = sf_beta, aes(fill = BetaTemporalSite))+
    scale_fill_gradientn(colours = pal, name=name)
# Save
figuresFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/figures/"
pngFile=paste0(figuresFolder,"TemporalBetaDiversity.png")
fBeta+ggsave(pngFile, width = 8, height = 8, dpi = 250, units = "in", device='png')


