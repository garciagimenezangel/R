library(dplyr)
library(rlist)
library(ggplot2)
library(reshape2)

setwd("C:/Users/angel/git/ESYRCE/")
source("./scripts/categories.R")
source("./scripts/functions.R")
dataFolder = "./clean_data/"

# df_LCtransitions = getLandCoverTransitionsFromControlPoints("G:/My Drive/PROJECTS/OBSERV/ESYRCE/metrics_v2021-02-25_FILLED.csv") # Method 1 (that file doesn't have the necessary lccpX columns, we would need to compute them again)
# df_LCtransitions = getLandCoverTransitionsFromProportion("G:/My Drive/PROJECTS/OBSERV/ESYRCE/metrics_v2021-02-25_FILLED.csv")    # Method 2
df_LCtransitions = read.csv(paste0(dataFolder,"landCoverChange_method2.csv"), row.names = 1)

############################
# NORMALIZED TRANSITIONS
############################
# Normalized transitions can be read, at each row, as the probability of transition from the row lc type, to the columns land cover types -> destination probability
# If df_LCtransitions is transposed, the normalization yields the probability of transition to the row lc type, from the columns land cover types -> origin probability
df_LCdestin = getLCdestinProb(df_LCtransitions)
df_LCorigin = getLCoriginProb(df_LCtransitions)

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
# groups = list(c("Active Cropland",agriActive),
#               c("Fallow", lowActivity),
#               c("Forested",c(forested,otherWoodyCrop)),
#               c("Pasture",pasture),
#               c("Abandoned",abandAgri),
#               c("Unproductive",improductive),
#               c("Artificial",notAgri))
# groups = list(c("Herbaceous Crop",c(cerealGrain, legumeGrain, tuber, industrial, fodder, vegetable, orchard, 
#                                     ornamental, nursery)),
#               c("Fallow", lowActivity),
#               c("Forested",c(forested,otherWoodyCrop)),
#               c("Pasture",pasture),
#               c("Abandoned",abandAgri),
#               c("Unproductive",improductive),
#               c("Artificial",notAgri))
# groups = list(c("Agri Land",agriLand),
#               c("Seminatural",seminatural),
#               c("Abandoned",abandAgri),
#               c("Unproductive",improductive),
#               c("Artificial",notAgri))
# groups = list(c("Poll-dep",pollDependent),
#               c("Poll-indep",pollNotDepent),
#               c("Unkn-dep",pollUnknown),
#               c("Fallow", lowActivity),
#               c("Forested",c(forested,otherWoodyCrop)),
#               c("Pasture",pasture),
#               c("Abandoned",abandAgri),
#               c("Unproductive",improductive),
#               c("Artificial",notAgri))
groups = list(c("Poll-dep",pollDependent),
              c("Poll-indep",pollNotDepent),
              c("Unkn-dep",pollUnknown[!(pollUnknown %in% seminatural2)]),
              c("Seminatural",seminatural2),
              c("Fallow", c("fallow")),#lowActivity),
              c("Abandoned",abandAgri),
              c("Unproductive",improductive),
              c("Artificial",notAgri))
# Sanity check->Land cover types not in groups:
grpElts = unlist(lapply(groups, function(x) x[2:length(x)]))
paste0("Land cover types not considered:",landcovertypes[!(landcovertypes %in% grpElts)])

# ORIGIN
df_LCorigin_gr = getLCoriginProbByGroup(df_LCtransitions)
# DESTINATION
df_LCdestin_gr = getLCdestinProbByGroup(df_LCtransitions)

############
# PLOTS
############
figFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/figures/"
removeNoTransition = T
abbreviateNames = FALSE

# Origin
target = "Seminatural"
title = "Seminatural Land origin"
df_plot = df_LCorigin_gr
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
df_plot = df_LCdestin_gr
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

# Destination fallow
target = "Fallow"
title = "Fallow destination"
df_plot = df_LCdestin_gr
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
ggsave(pngFile, width = 8, height = 8, dpi = 250, units = "in", device='png')

# Destination poll-dep crops
target = "Poll-dep"
title = "Poll-dep crops destination"
df_plot = df_LCdestin_gr
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
ggsave(pngFile, width = 8, height = 8, dpi = 250, units = "in", device='png')

# Destination poll-indep crops
target = "Poll-indep"
title = "Poll-indep crops destination"
df_plot = df_LCdestin_gr
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
ggsave(pngFile, width = 8, height = 8, dpi = 250, units = "in", device='png')

# Destination seminatural
target = "Seminatural"
title = "Seminatural habitat destination"
df_plot = df_LCdestin_gr
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
ggsave(pngFile, width = 8, height = 8, dpi = 250, units = "in", device='png')

# Origin poll-indep crops
target = "Poll-indep"
title = "Poll-indep crops origin"
df_plot = df_LCorigin_gr
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

# Origin poll-dep crops
target = "Poll-dep"
title = "Poll-dep crops origin"
df_plot = df_LCorigin_gr
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
ggsave(pngFile, width = 8, height = 8, dpi = 250, units = "in", device='png')

