library(dplyr)
library(gstat) 
###########
setwd("C:/Users/angel/git/R/ESYRCE/")

# Organize categories
source("./categories.R")

# Functions
source("./functions.R")

dataFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/Analysis/2020-12/"
GEEFolder  = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/GEE/ZonasNaturales/"

# Read datasets
dataFile     = paste0(dataFolder, "geo_metrics_20-12-18.csv")
df_data      = read.csv(dataFile, header=T)
modelFile    = paste0(dataFolder, "geo_model_20-12-18.csv")
df_pollModel = read.csv(modelFile, header=T)

###############################
# Calculate slopes by category
###############################
###############
# Proportion of seminatural area
columns = paste0("prop_",seminatural)
useCols = c("D1_HUS", "D2_NUM", "province", "region", "YEA","longitude", "latitude", columns)
df_slopeSeminatural = df_data[,useCols] %>% group_by(D1_HUS, D2_NUM, province, region) %>%  do(data.frame(calculateSlope(., columns)))
df_slopeSeminatural = df_slopeSeminatural[!is.na(df_slopeSeminatural$slope),]
df_slopeSeminatural$region = abbreviate(df_slopeSeminatural$region)
df_slopeSeminatural$province = abbreviate(df_slopeSeminatural$province)
# Histogram
ggplot(df_slopeSeminatural, aes(x=slope)) + geom_histogram(breaks=c(-0.2,-0.1,-0.01,-0.001,0.001,0.01,0.1,0.2)) + coord_cartesian(xlim=c(-0.2,0.2)) + facet_wrap(~region, nrow=2)
# Values by region
ggplot(df_slopeSeminatural, aes(region, slope)) + geom_point(aes(colour = factor(region))) + geom_hline(yintercept=0, linetype="dashed", color = "black")
boxplot(df_slopeSeminatural$slope ~ df_slopeSeminatural$region, range=100, ylim = c(-0.01, 0.01))
boxplot(df_slopeSeminatural$slope ~ df_slopeSeminatural$province, range=100, ylim = c(-0.01, 0.01))
aggregate(df_slopeSeminatural[,"slope"], by=list(df_slopeSeminatural$region), FUN=mean1000)
aggregate(df_slopeSeminatural[,"slope"], by=list(df_slopeSeminatural$region), FUN=median1000)

###############
# Proportion of cereal grain
columns = paste0("prop_",cerealGrain)
useCols = c("D1_HUS", "D2_NUM", "province", "region", "YEA","longitude", "latitude", columns)
df_slopeCereal = df_data[,useCols] %>% group_by(D1_HUS, D2_NUM, province, region) %>%  do(data.frame(calculateSlope(., columns)))
df_slopeCereal = df_slopeCereal[!is.na(df_slopeCereal$slope),]
df_slopeCereal$region = abbreviate(df_slopeCereal$region)
df_slopeCereal$province = abbreviate(df_slopeCereal$province)
# Histogram
ggplot(df_slopeCereal, aes(x=slope)) + geom_histogram(breaks=c(-0.2,-0.1,-0.01,-0.001,0.001,0.01,0.1,0.2)) + coord_cartesian(xlim=c(-0.2,0.2)) + facet_wrap(~region, nrow=2)
# Values by region
ggplot(df_slopeCereal, aes(region, slope)) + geom_point(aes(colour = factor(region))) + geom_hline(yintercept=0, linetype="dashed", color = "black")
boxplot(df_slopeCereal$slope ~ df_slopeCereal$region, range=100, ylim = c(-0.01, 0.01))
boxplot(df_slopeCereal$slope ~ df_slopeCereal$province, range=100, ylim = c(-0.01, 0.01))

###############
# Proportion of fruit trees (citric and no citric)
columns = paste0("prop_",fruitTree)
useCols = c("D1_HUS", "D2_NUM", "province", "region", "YEA","longitude", "latitude", columns)
df_fruitTree = df_data[,useCols] %>% group_by(D1_HUS, D2_NUM, province, region) %>%  do(data.frame(calculateSlope(., columns)))
df_fruitTree = df_fruitTree[!is.na(df_fruitTree$slope),]
df_fruitTree$region = abbreviate(df_fruitTree$region)
df_fruitTree$province = abbreviate(df_fruitTree$province)
# Histogram
ggplot(df_fruitTree, aes(x=slope)) + geom_histogram(breaks=c(-0.2,-0.1,-0.01,-0.001,0.001,0.01,0.1,0.2)) + coord_cartesian(xlim=c(-0.2,0.2)) + facet_wrap(~region, nrow=2)
# Values by region
ggplot(df_fruitTree, aes(region, slope)) + geom_point(aes(colour = factor(region))) + geom_hline(yintercept=0, linetype="dashed", color = "black")
boxplot(df_fruitTree$slope ~ df_fruitTree$region, range=100, ylim = c(-0.01, 0.01))
boxplot(df_fruitTree$slope ~ df_fruitTree$province, range=100, ylim = c(-0.01, 0.01))

###############
# Proportion of other (mainly abandoned/fallow)
columns = paste0("prop_",other)
useCols = c("D1_HUS", "D2_NUM", "province", "region", "YEA","longitude", "latitude", columns)
df_other = df_data[,useCols] %>% group_by(D1_HUS, D2_NUM, province, region) %>%  do(data.frame(calculateSlope(., columns)))
df_other = df_other[!is.na(df_other$slope),]
df_other$region = abbreviate(df_other$region)
df_other$province = abbreviate(df_other$province)
# Histogram
ggplot(df_other, aes(x=slope)) + geom_histogram(breaks=c(-0.2,-0.1,-0.01,-0.001,0.001,0.01,0.1,0.2)) + coord_cartesian(xlim=c(-0.2,0.2)) + facet_wrap(~region, nrow=2)
# Values by region
ggplot(df_other, aes(region, slope)) + geom_point(aes(colour = factor(region))) + geom_hline(yintercept=0, linetype="dashed", color = "black")
boxplot(df_other$slope ~ df_other$region, range=100, ylim = c(-0.01, 0.01))
boxplot(df_other$slope ~ df_other$province, range=100, ylim = c(-0.01, 0.01))

###############
# Proportion of notAgri (artificial?)
columns = paste0("prop_",notAgri)
useCols = c("D1_HUS", "D2_NUM", "province", "region", "YEA","longitude", "latitude", columns)
df_notAgri = df_data[,useCols] %>% group_by(D1_HUS, D2_NUM, province, region) %>%  do(data.frame(calculateSlope(., columns)))
df_notAgri = df_other[!is.na(df_notAgri$slope),]
df_notAgri$region = abbreviate(df_notAgri$region)
df_notAgri$province = abbreviate(df_notAgri$province)
# Histogram
ggplot(df_notAgri, aes(x=slope)) + geom_histogram(breaks=c(-0.2,-0.1,-0.01,-0.001,0.001,0.01,0.1,0.2)) + coord_cartesian(xlim=c(-0.2,0.2)) + facet_wrap(~region, nrow=2)
# Values by region
ggplot(df_notAgri, aes(region, slope)) + geom_point(aes(colour = factor(region))) + geom_hline(yintercept=0, linetype="dashed", color = "black")
boxplot(df_notAgri$slope ~ df_notAgri$region, range=100, ylim = c(-0.01, 0.01))
boxplot(df_notAgri$slope ~ df_notAgri$province, range=100, ylim = c(-0.01, 0.01))

###############
# Proportion of agricultural land
columns = paste0("prop_",agriLand)
useCols = c("D1_HUS", "D2_NUM", "province", "region", "YEA","longitude", "latitude", columns)
df_agriLand = df_data[,useCols] %>% group_by(D1_HUS, D2_NUM, province, region) %>%  do(data.frame(calculateSlope(., columns)))
df_agriLand = df_agriLand[!is.na(df_agriLand$slope),]
df_agriLand$region = abbreviate(df_agriLand$region)
df_agriLand$province = abbreviate(df_agriLand$province)
# Histogram
ggplot(df_agriLand, aes(x=slope)) + geom_histogram(breaks=c(-0.2,-0.1,-0.01,-0.001,0.001,0.01,0.1,0.2)) + coord_cartesian(xlim=c(-0.2,0.2)) + facet_wrap(~region, nrow=2)
# Values by region
ggplot(df_agriLand, aes(region, slope)) + geom_point(aes(colour = factor(region))) + geom_hline(yintercept=0, linetype="dashed", color = "black")
boxplot(df_agriLand$slope ~ df_agriLand$region, range=100, ylim = c(-0.01, 0.01))
boxplot(df_agriLand$slope ~ df_agriLand$province, range=100, ylim = c(-0.01, 0.01))

##############
# Demand
columns = demand
useCols = c("D1_HUS", "D2_NUM", "province", "region", "YEA","longitude", "latitude", columns)
df_slopeDemand = df_data[,useCols] %>% group_by(D1_HUS, D2_NUM, province, region) %>%  do(data.frame(calculateSlope(., columns)))
df_slopeDemand = df_slopeDemand[!is.na(df_slopeDemand$slope),]
df_slopeDemand$region   = abbreviate(df_slopeDemand$region)
df_slopeDemand$province = abbreviate(df_slopeDemand$province)
# Histogram
ggplot(df_slopeDemand, aes(x=slope)) +
  geom_histogram(breaks=c(-0.2,-0.1,-0.01,-0.001,0.001,0.01,0.1,0.2)) +
  coord_cartesian(xlim=c(-0.2,0.2)) +
  facet_wrap(~region, nrow=2)
# Values by region
ggplot(df_slopeDemand, aes(region, slope)) + geom_point(aes(colour = factor(region))) + geom_hline(yintercept=0, linetype="dashed", color = "black")
boxplot(df_slopeDemand$slope ~ df_slopeDemand$region, range=100, ylim = c(-0.005, 0.005))
boxplot(df_slopeDemand$slope ~ df_slopeDemand$province, range=100, ylim = c(-0.005, 0.005))

##############
# Yield 
columns = paste0("yield_",agriLand)
indNotAllNA = rowSums(!is.na(df_data[,columns])) > 0
df_data[indNotAllNA,columns]  = df_data[indNotAllNA,columns] %>% mutate_all(~replace(., is.na(.), 0)) # The NA values from crops that are not present in the segments must be replace by 0's
df_slopeYield = df_data[,useCols] %>% group_by(D1_HUS, D2_NUM, province, region) %>%  do(data.frame(calculateSlope(., columns)))
df_slopeYield = df_slopeDemand[!is.na(df_slopeYield$slope),]
df_slopeYield$region   = abbreviate(df_slopeYield$region)
df_slopeYield$province = abbreviate(df_slopeYield$province)
# Histogram
ggplot(df_slopeYield, aes(x=slope)) +
  geom_histogram(breaks=c(-0.2,-0.1,-0.01,-0.001,0.001,0.01,0.1,0.2)) +
  coord_cartesian(xlim=c(-0.2,0.2)) +
  facet_wrap(~region, nrow=2)
# Values by region
ggplot(df_slopeYield, aes(region, slope)) + geom_point(aes(colour = factor(region))) + geom_hline(yintercept=0, linetype="dashed", color = "black")
boxplot(df_slopeYield$slope ~ df_slopeYield$region, range=100, ylim = c(-0.01, 0.01))
boxplot(df_slopeYield$slope ~ df_slopeYield$province, range=100, ylim = c(-0.01, 0.01))

##############
# Pollination potential
columns = c("ZonasNaturales_man0_mod0")
df_slopesPoll = df_pollModel %>% group_by(D1_HUS, D2_NUM, province, region) %>%  do(data.frame(calculateSlope(., columns)))
df_slopesPoll = df_slopesPoll[!is.na(df_slopesPoll$slope),]
df_slopesPoll$region   = abbreviate(df_slopesPoll$region)
df_slopesPoll$province = abbreviate(df_slopesPoll$province)
# Values by region
boxplot(df_slopesPoll$slope ~ df_slopesPoll$region, range=100, ylim = c(-0.01, 0.01))
ggplot(df_slopesPoll, aes(region, slope)) + geom_point(aes(colour = factor(region))) + geom_hline(yintercept=0, linetype="dashed", color = "black")


# Field size
columns = c("avgFieldSizeDiss")
df_slopeFieldSize = df_data %>% group_by(D1_HUS, D2_NUM, province, region) %>%  do(data.frame(calculateSlope(., columns)))

#...

############################
# Annual evolution 
############################
# Demand
pl_dem<-ggplot(df_data, aes(YEA, demand)) +
  geom_point(aes(colour = factor(region))) +
  geom_smooth(method="lm", se=TRUE, aes(color=region))

# Pollination potential
pl_poll<-ggplot(df_pollModel, aes(YEA, ZonasNaturales_man0_mod0)) +
  geom_point(aes(colour = factor(region))) +
  geom_smooth(method="lm", se=TRUE, aes(color=region))

