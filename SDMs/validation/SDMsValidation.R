#ESYRCE
#Validar londsdorf global y por crop
library(lme4)
library(MuMIn)
library(nlme)
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggeffects)
library(sjPlot)
library(stargazer)
library(glmmTMB)  
library(visreg)

# Model data repo
model_repo = "C:/Users/angel/git/OBserv_models/"
data_repo  = "C:/Users/angel/git/OBServData/"
# model_repo = "C:/Users/angel.gimenez/git/OBserv_models/"
# data_repo  = "C:/Users/angel.gimenez/git/OBServData/"

#Read data----
modelData = read.csv(paste0(model_repo, "data/model_data.csv"), header = TRUE) %>% dplyr::select(-c("latitude","longitude"))
fieldData = read.csv(paste0(data_repo, "Final_Data/CropPol_field_level_data.csv"), header = TRUE) %>% dplyr::select(-c("latitude","longitude"))
df_data   = merge(fieldData, modelData)

# Filter to get complete data at the columns that we are interested in
fieldCols = c("study_id", "site_id", "sampling_year", "crop", "country", "abundance","ab_honeybee","ab_bombus" ,                        
              "ab_wildbees"             ,"ab_syrphids"        ,"ab_humbleflies"        ,
              "ab_other_flies"          ,"ab_beetles"         ,"ab_lepidoptera"        ,
              "ab_nonbee_hymenoptera"   ,"ab_others"          ,"total_sampled_area"    ,
              "total_sampled_time"      ,"sampling_visitation","visitation_rate_units" ,
              "visitation_rate"         ,"visit_honeybee"     ,"visit_bombus"          ,
              "visit_wildbees"          ,"visit_syrphids"     ,"visit_humbleflies"     ,
              "visit_other_flies"       ,"visit_beetles"      ,"visit_lepidoptera"     ,
              "visit_nonbee_hymenoptera","visit_others",
              "observed_pollinator_richness", "other_pollinator_richness")
modelCols = c("SDM.combined.v1", "SDM.combined.v2")
df_data   = df_data %>% dplyr::select(c(fieldCols,modelCols)) # select columns

#############################
# SET DATA (EUROPE OR SPAIN) 
#############################
# SPAIN
df_spain = subset(df_data, country == "Spain")
dat = df_spain
dat = subset(dat, !is.na(SDM.combined.v2))

# EUROPE
dat = df_data
dat = subset(dat, !is.na(SDM.combined.v2))

# Standardize units somehow
sumCols                = c("ab_bombus","ab_wildbees","ab_syrphids","ab_humbleflies", "ab_nonbee_hymenoptera", "ab_lepidoptera") 
dat                    = subset(dat, !is.na(ab_wildbees)) # at least abundance of wildbees must be measured 
dat                    = subset(dat, !is.na(total_sampled_time)) # total sampled time needed 
dat[,sumCols]          = dat[,sumCols] %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))
dat$ab_wild_comparable = rowSums(dat[,sumCols]) / dat$total_sampled_time
dat                    = dat %>% group_by(study_id) %>% mutate(model = scale(ab_wild_comparable))

#############################
# EXPLORE 
#############################
boxplot(dat$ab_wild_comparable ~ dat$crop)
hist(dat$ab_wild_comparable)
(ggplot(dat, aes(ab_wild_comparable)) + 
        geom_histogram(bins=20)+ 
        facet_wrap(~ study_id))
(ggplot(aes(Lonsdorf.ZonasNaturales_man0_mod0, ab_wild_comparable), data = dat) + 
        geom_point() + 
        facet_wrap(~ study_id) + 
        xlab("model") + 
        ylab("ab_wild_comparable")) # See values by study_id
(ggplot(aes(Lonsdorf.INVEST.CORINE_man0_mod0, ab_wild_comparable), data = dat) + 
        geom_point() + 
        facet_wrap(~ study_id) + 
        xlab("model") + 
        ylab("ab_wild_comparable")) # See values by study_id



###############################
# MODELS
###############################
# SDM.combined.v2
model <- glmer.nb(observed_pollinator_richness ~ SDM.combined.v2 + (1|study_id), data = dat, na.action = na.omit)
model <- glmmTMB(ab_wild_comparable ~ SDM.combined.v2 + (1|study_id), family = Gamma(link='log'), dat[dat$ab_wild_comparable>0,])
performance::r2(model)

# Lonsdorf.INVEST.CORINE_man0_mod0 or Lonsdorf.INVEST.CORINE_man0_mod1
# model <- glmer.nb(ab_wild_comparable ~ Lonsdorf.INVEST.CORINE_man0_mod0 + (1|study_id), data = dat, na.action = na.omit)
m2 <- glmmTMB(ab_wild_comparable ~ Lonsdorf.INVEST.CORINE_man0_mod0 + (1|study_id), family = Gamma(link='log'), dat[dat$ab_wild_comparable>0,])
r22 = performance::r2(m2)

# Z abundance
#model <- lme(Zabundance ~ Lonsdorf.INVEST.CORINE_man0_mod1, random = ~1|study_id, data = dat, na.action = na.omit)


##############################
# PLOTS
##############################
term = "SDM.combined.v2" # write predictor used here
# Using visreg
(plot <- visreg(model, term, gg = TRUE))

dat=dat[dat$SDM.combined.v2>0, ]
dat$ot
ggplot(dat, aes(SDM.combined.v2, ab_wild_comparable, na.rm = TRUE))+
    geom_point(size=2.5, shape=21,color = "black", aes(fill=factor(crop)))+
    geom_smooth(method="lm", size=1, se=TRUE, color = "black", alpha=0.4, na.rm = T)+
    facet_wrap(~crop) 

ggplot(dat, aes(SDM.combined.v2, observed_pollinator_richness, na.rm = TRUE))+
    geom_point(size=2.5, shape=21,color = "black", aes(fill=factor(crop)))+
    geom_smooth(method="lm", size=1, se=TRUE, color = "black", alpha=0.4, na.rm = T)+
    facet_wrap(~crop) 

crop="Allium porrum"
dcrop = dat[(dat$crop == crop) & !is.na(dat$observed_pollinator_richness),]
ggplot(dcrop, aes(SDM.combined.v2, observed_pollinator_richness, na.rm = TRUE))+
    geom_point(size=2.5, shape=21,color = "black", aes(fill=factor(crop)))+
    geom_smooth(method="lm", size=1, se=TRUE, color = "black", alpha=0.4, na.rm = T)
cor(dcrop$observed_pollinator_richness, dcrop$SDM.combined.v2)

# term = "SDM.combined.v1" # write predictor used here
# # Using visreg
# p <- visreg(model, term, gg = TRUE, plot = FALSE)
# p$colour = dat[dat$observed_pollinator_richness>0,]$crop
# Crop = p1$colour
# Crop = stringr::str_replace(Crop, "Fragaria x ananassa", "strawberry")
# Crop = stringr::str_replace(Crop, "Helianthus annuus", "sunflower")
# Crop = stringr::str_replace(Crop, "Malus domestica", "apple")
# Crop = stringr::str_replace(Crop, "Prunus dulcis", "almond")
# f1 <- ggplot()+
#     geom_point(data = p1$res, aes(Lonsdorf.ZonasNaturales_man0_mod0, visregRes, colour=Crop))+
#     geom_line(data = p1$fit, aes(Lonsdorf.ZonasNaturales_man0_mod0, visregFit))+
#     geom_ribbon(data = p1$fit, aes(x = Lonsdorf.ZonasNaturales_man0_mod0, ymin = visregLwr, ymax = visregUpr), 
#                 fill = "lightgrey", alpha = 0.5)+
#     xlab("Pollinators score (Spain)") +
#     ylab("f(Observed abundance)")+
#     theme_classic()+
#     theme(legend.text = element_text(size = 12, face = "italic"), legend.title = element_text(size=15, face = "bold"))
