library(dplyr)
library(rlist)
library(ggplot2)
library(reshape2)

# Objetivo: sacar una tabla de probabilidades de cada posible transición entre clases.
# Método 1: usar landcover extraído en 9 puntos de control (lccp1, lccp2...) de cada segmento, y recoger todas las transiciones existentes de año a año
# Método 2: de un año a otro, extraer pérdidas y ganancias de cada landcover, y distribuirlas equitativamente. Es decir, si por ejemplo maíz supone un 10% de todas las ganancias de cobertura ese año, supondríamos que el 10% de las pérdida de cada landcover que pierde cobertura, ha ido a parar a cobertura de maíz.

setwd("C:/Users/angel/git/R/ESYRCE/")
# setwd("C:/Users/angel.gimenez/git/R/ESYRCE/")
source("./categories.R")
source("./functions.R")
dataFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/"
# dataFolder = "C:/Users/angel.gimenez/Google Drive/PROJECTS/OBSERV/ESYRCE/"
dataFileMetrics       = paste0(dataFolder, "metrics_v2021-02.csv")
dataFileControlPoints = paste0(dataFolder, "landCoverChange/landCoverTransitions.csv")

# Método 1: usar landcover extraído en 9 puntos de control (lccp1, lccp2...) de cada segmento, y recoger todas las transiciones existentes de año a año
csvSaved = TRUE
if (csvSaved)  df_LCtransitions = read.csv(paste0(dataFolder,"landCoverChange/landCoverChange_method1.csv"), row.names = 1)
if (!csvSaved) df_LCtransitions = getLandCoverTransitionsFromControlPoints(dataFile = dataFileControlPoints)

# Método 2: de un año a otro, extraer pérdidas y ganancias de cada landcover, y distribuirlas equitativamente. Es decir, si por ejemplo maíz supone un 10% de todas las ganancias de cobertura ese año, supondríamos que el 10% de las pérdida de cada landcover que pierde cobertura, ha ido a parar a cobertura de maíz.
# if already calculated: 
csvSaved = TRUE
if (csvSaved)  df_LCtransitions = read.csv(paste0(dataFolder,"landCoverChange/landCoverChange_method2.csv"), row.names = 1)
if (!csvSaved) df_LCtransitions = getLandCoverTransitionsFromProportion(dataFile = dataFileMetrics)

################
# SPLIT BY YEAR
################
csvSaved = TRUE
initYears = seq(2001,2018)
list_LCtrans_byYear = list()
for (i in seq(1,length(initYears))) {
  initYear = initYears[i]
  endYear  = initYear+1
  if (csvSaved)  list_LCtrans_byYear[[i]] = read.csv(paste0(dataFolder,"landCoverChange/landCoverChange_method2_",initYear,"-",endYear,".csv"), row.names = 1)
  if (!csvSaved) list_LCtrans_byYear[[i]] = getLandCoverTransitionsFromProportion(dataFile = dataFileMetrics, timeInterval = c(initYear,endYear))
}
for (i in seq(1,length(initYears))) {
  initYear = initYears[i]
  endYear  = initYear+1
  fileName = paste0("landCoverChange/landCoverChange_method2_",initYear,"-",endYear,".csv")
  rownames(list_LCtrans_byYear[[i]]) = colnames(list_LCtrans_byYear[[i]])  
  write.csv(list_LCtrans_byYear[[i]], file=paste0(dataFolder,fileName), row.names=TRUE)
}

############################
# NORMALIZED TRANSITIONS
############################
# Normalized transitions can be read, at each row, as the probability of transition from the row lc type, to the columns land cover types -> destination probability
# If df_LCtransitions is transposed, the normalization yields the probability of transition to the row lc type, from the columns land cover types -> origin probability
df_LCdestin = getLCdestinProb(df_LCtransitions)
df_LCorigin = getLCoriginProb(df_LCtransitions)

# by year
list_LCdestin_prob_byYear = lapply(list_LCtrans_byYear, getLCdestinProb)

############################
# BY GROUP OF LANDCOVER
############################
# groups = list(c("cerealGrain",cerealGrain),
#               c("legumeGrain",legumeGrain),
#               c("tuber",tuber),
#               c("industrial",industrial),
#               c("fodder",fodder),
#               c("vegetable",vegetable),
#               c("orchard",orchard),
#               c("ornamental",ornamental),
#               c("citric",citric),
#               c("fruitNoCitric",fruitNoCitric),
#               c("vineyard",vineyard),
#               c("oliveTrees",oliveTrees),
#               c("nursery",nursery),
#               c("forested",c(forested,otherWoodyCrop)),
#               c("pasture",pasture),
#               c("fallow",other),
#               c("improductive",improductive),
#               c("notAgri",notAgri))
groups = list(c("Active Cropland",agriActive),
              c("Fallow", lowActivity),
              c("Forested",c(forested,otherWoodyCrop)),
              c("Pasture",pasture),
              c("Abandoned",abandAgri),
              c("Unproductive",improductive),
              c("Artificial",notAgri))
groups = list(c("Agri Land",agriLand),
              c("Seminatural",seminatural),
              c("Abandoned",abandAgri),
              c("Unproductive",improductive),
              c("Artificial",notAgri))
# groups = list(c("Poll-dep",pollDependent),
#               c("Poll-indep",pollNotDepent),
#               c("Unkn-dep",pollUnknown),
#               c("Fallow", lowActivity),
#               c("Forested",c(forested,otherWoodyCrop)),
#               c("Pasture",pasture),
#               c("Abandoned",abandAgri),
#               c("Unproductive",improductive),
#               c("Artificial",notAgri))
# Sanity check->Land cover types not in groups:
grpElts = unlist(lapply(groups, function(x) x[2:length(x)]))
paste0("Land cover types not considered:",landcovertypes[!(landcovertypes %in% grpElts)])

# If method 1, add water and other:
# groups = list.append(groups,c("water",c("water")),c("other",c("other"))) 

# ORIGIN
df_LCorigin_gr = getLCoriginProbByGroup(df_LCtransitions)
# DESTINATION
df_LCdestin_gr = getLCdestinProbByGroup(df_LCtransitions)
# by year
list_LCdestin_gr_byYear = lapply(list_LCtrans_byYear, getLCdestinProbByGroup)

############
# PLOTS
############
figFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/figures/"
removeNoTransition = FALSE
abbreviateNames = FALSE

# Evolution by year
title = "Active Cropland to Fallow transition"
df_LCdestin_gr_byYear     = list.rbind(list_LCdestin_gr_byYear)
target                    = "Active Cropland"
plotTargets               = c("Active Cropland","Fallow")
selRows                   = grepl( target, rownames(df_LCdestin_gr_byYear), fixed = TRUE)
df_LCdestin_Target_byYear = as.data.frame(t(df_LCdestin_gr_byYear[selRows, ]))
colnames(df_LCdestin_Target_byYear) = initYears+1 
df_melt <- melt(as.matrix(df_LCdestin_Target_byYear))
colnames(df_melt) = c("final","year","value")
if (removeNoTransition) df_melt = df_melt[ df_melt$final != target , ]
df_melt = df_melt [df_melt$final %in% plotTargets, ]  
df_melt %>%
  mutate(bin = value > 0.5) %>%
ggplot(aes(year, value, group=factor(final))) + 
  geom_line(aes(color=factor(final))) +
  geom_smooth(method='lm', formula= y~x)+labs(colour="Land cover") +
  facet_grid(bin ~ ., scale='free_y') +
  theme(strip.text.y = element_blank())+    
  ylab("Probability") +
  xlab("Year") +
  ggtitle(title)
pngFile = paste0(figFolder,'landcover/',title,".png")
ggsave(pngFile)

# Origin
target = "Seminatural"
title = "Seminatural Land origin"
df_plot = df_LCorigin_gr_norm
df_melt = melt(as.matrix(df_plot))
colnames(df_melt) = c("origin","final","value")
if (removeNoTransition) df_melt = df_melt[ df_melt$origin != df_melt$final , ]
if (abbreviateNames) {
  df_melt$origin = abbreviate(df_melt$origin)
  df_melt$final  = abbreviate(df_melt$final)  
}
df_melt_sel = df_melt[df_melt$origin == target,]
ggplot(data=df_melt_sel, aes(x=final, y=value*100)) +
    ylab("Percentage") +
    xlab("Land cover category") +
    geom_bar(stat="identity") + 
    ggtitle(title)
# Save in a png file
pngFile = paste0(figFolder,title,".png")
ggsave(pngFile)

# Destination agri land
target = "Agri Land"
title = "Agricultural Land destination"
df_plot = df_LCdestin_gr_norm
df_melt = melt(as.matrix(df_plot))
colnames(df_melt) = c("origin","final","value")
if (removeNoTransition) df_melt = df_melt[ df_melt$origin != df_melt$final , ]
if (abbreviateNames) {
  df_melt$origin = abbreviate(df_melt$origin)
  df_melt$final  = abbreviate(df_melt$final)  
}
df_melt_sel = df_melt[df_melt$origin == target,]
ggplot(data=df_melt_sel, aes(x=final, y=value*100)) +
    ylab("Percentage") +
    xlab("Land cover category") +
    geom_bar(stat="identity") + 
    ggtitle(title)
# Save in a png file
pngFile = paste0(figFolder,title,".png")
ggsave(pngFile)

# Origin poll-dep crops
target = "Poll-dep"
title = "Poll-dep crops origin"
df_plot = df_LCorigin_gr_norm
df_melt = melt(as.matrix(df_plot))
colnames(df_melt) = c("origin","final","value")
if (removeNoTransition) df_melt = df_melt[ df_melt$origin != df_melt$final , ]
if (abbreviateNames) {
  df_melt$origin = abbreviate(df_melt$origin)
  df_melt$final  = abbreviate(df_melt$final)  
}
df_melt_sel = df_melt[df_melt$origin == target,]
ggplot(data=df_melt_sel, aes(x=final, y=value)) +
    geom_bar(stat="identity") + 
    ggtitle(title) + ylim(0,0.25)
# Save in a png file
pngFile = paste0(figFolder,title,".png")
ggsave(pngFile)

# Origin poll-indep crops
target = "Poll-indep"
title = "Poll-indep crops origin"
df_plot = df_LCorigin_gr_norm
df_melt = melt(as.matrix(df_plot))
colnames(df_melt) = c("origin","final","value")
if (removeNoTransition) df_melt = df_melt[ df_melt$origin != df_melt$final , ]
if (abbreviateNames) {
  df_melt$origin = abbreviate(df_melt$origin)
  df_melt$final  = abbreviate(df_melt$final)  
}
df_melt_sel = df_melt[df_melt$origin == target,]
ggplot(data=df_melt_sel, aes(x=final, y=value)) +
    geom_bar(stat="identity") + 
    ggtitle(title) + ylim(0,0.25)
# Save in a png file
pngFile = paste0(figFolder,title,".png")
ggsave(pngFile)

# All groups
# (p1<-ggplot(data=df_melt, aes(x=final, y=value)) +
#   geom_bar(stat="identity")) + 
#   ggtitle("Transitions") +
#   facet_wrap(~origin)

##################
# ANALYSE REGIONS 
##################
df_data = df_data[df_data$region == "Galicia",] # By region/province?
df_lcsums = data.frame(landcover = landcovertypes, totalArea=colSums(df_data[,prop_landcovertypes]),row.names = NULL)
df_lcsums_groups = data.frame(matrix(0, ncol = 1, nrow = 0))
colnames(df_lcsums_groups) = c("totalArea")
for(group in groups) {
  name    = group[1]
  lcTypes = group[2:length(group)]
  df_subset = df_lcsums[df_lcsums$landcover %in% lcTypes, ] 
  df_lcsums_groups[name, "totalArea"] = sum(df_subset$totalArea)
}
df_lcsums_groups$landcover = abbreviate(rownames(df_lcsums_groups))
(pie<- ggplot(df_lcsums_groups, aes(x="", y=totalArea, fill=landcover))+
  geom_bar(width = 1, stat = "identity", color="white") +
    coord_polar("y", start=0))
(bp<-ggplot(data=df_lcsums_groups, aes(x=landcover, y=totalArea)) +
  geom_bar(stat="identity"))



# Save data
write.csv(df_LCtransitions_groups, file=paste0(dataFolder,"landCoverChange/landCoverChangeGroupsGalicia_method2.csv"))
write.csv(df_LCtransitions_groups_norm, file=paste0(dataFolder,"landCoverChange/landCoverChangeGroupsNormGalicia_method2.csv"))
