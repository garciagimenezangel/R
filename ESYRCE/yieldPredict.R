library(dplyr)
library(gstat) 
library(raster)
###########
setwd("C:/Users/angel/git/R/ESYRCE/")

# Organize categories
source("./categories.R")

# Functions
source("./functions.R")

dataFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/Analysis/2020-12/"
GEEFolder  = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/GEE/ZonasNaturales/"

# Read datasets
dataFile     = paste0(dataFolder, "geo_metrics_climate_20-12-18.csv")
df_data      = read.csv(dataFile, header=T)
modelFile    = paste0(dataFolder, "geo_model_20-12-18.csv")
df_pollModel = read.csv(modelFile, header=T)


###############
# Explore data
###############
# One plot for each province/region
crop     = "rapeseed"
yieldCol = paste0("yield_",crop)
useCols  = c("D1_HUS", "D2_NUM", "province", "region", "YEA", yieldCol)
indYield = !is.na(df_data[,yieldCol]) & df_data[,yieldCol] != 0
df_yield = df_data[indYield, useCols]
listGroupVals = df_yield %>% group_by(D1_HUS, D2_NUM, province, region) %>% group_split(.)
stPlot = paste0("ggplot(NULL, aes(YEA,",yieldCol,"))")
for (i in seq(1,length(listGroupVals))){ if(nrow(listGroupVals[[i]])>1) { stPlot = paste(stPlot, "+ geom_line(data = listGroupVals[[",as.character(i),"]])") } }
stPlot = paste(stPlot, "+ facet_wrap(~province, nrow=2)", "+ ylim(0, 10000)")
eval(parse(text=stPlot))

# Plot everything together by region? Difficult to see anything
pl_crop<-ggplot(df_yield, aes(YEA, yield_rapeseed)) +
  geom_point(aes(colour = factor(region)))+ 
  geom_smooth(method="lm", se=TRUE, aes(color=region)) + 
  ylim(0, 10000)
pl_crop

# By year
boxplot(df_yield[,yieldCol] ~ df_yield$YEA, range=100, ylim=c(-100, 10000))

#########
# Models
#########
# Model (Ainhoa): Production ~ Climatic (including interanual variability, total_prec + min_temp + max_temp) * 
#Landscape configuration (avgFieldSize + FALTAN: edge density, average size natural area) * 
#Landscape composition (heterogeneity + seminatural) * 
#time (* management) + cuadraticos [medidas repetidas] 

# Select useful columns from df_data
stackCols = paste0("yield_",agriLand)
commCols  = c("D1_HUS", "D2_NUM", "province", "region", "YEA")
metrics   = c("avgFieldSizeDiss", "avgSeminatSizeDiss", "heterogeneity", "edgeDenSeminat", "edgeDensity")
useCols   = c(commCols, metrics, stackCols)
df_yield  = df_data[, useCols]

# Create new columns with new parameters
df_yield$seminatural = rowSums(df_data[,paste0("prop_",seminatural)])

# Add columns to use as parameters
rowSums(data[,columns])

# Stack data to use crops as random factors
stackCols = paste0("yield_",agriLand)
commCols  = c("D1_HUS", "D2_NUM", "province", "region", "YEA")
metrics   = c("avgFieldSizeDiss", "avgSeminatSizeDiss", "heterogeneity", "edgeDenSeminat", "edgeDensity")
useCols   = c(commCols, metrics, stackCols)
df_yield  = df_data[, useCols]
df_stack  = data.frame(df_yield[,c(commCols, metrics)], stack(df_yield[,columns]))
df_stack  = na.omit(df_stack) #remove all NA observations for yield
df_stack$site<-paste(df_stack$D1_HUS, df_stack$D2_NUM) #unique identifier for each site combination of HUS and NUM

df_rape = subset(df_stack, df_stack$ind=="yield_rapeseed")
df_rape_norm = df_rape
df_rape_norm[,c(metrics,"values")] = lapply(df_rape[,c(metrics,"values")], function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE)/1))

library(lme4)
m1<-lme(values~(heterogeneity + YEA + edgeDensity), 
        random = ~ 1 | site,
        correlation = corCAR1(form = ~ YEA | site), data=df_rape_norm) #include site as random and correlation structure for year
plot(m1)
qqnorm(resid(m1))
qqline(resid(m1))

