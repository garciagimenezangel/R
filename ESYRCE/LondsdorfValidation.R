#ESYRCE
#Validar londsdorf global y por crop
library(lme4)
library(MuMIn)
library(nlme)
library(ggplot2)
library(dplyr)
library(ggeffects)
library(sjPlot)
library(stargazer)

# Model data repo
model_repo = "C:/Users/angel/git/OBserv_models/"
data_repo  = "C:/Users/angel/git/OBServData/"

#Read data----
modelData = read.csv(paste0(model_repo, "data/model_data.csv"), header = TRUE) %>% select(-c("latitude","longitude"))
fieldData = read.csv(paste0(data_repo, "Final_Data/CropPol_field_level_data.csv"), header = TRUE) %>% select(-c("latitude","longitude"))
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
              "visit_nonbee_hymenoptera","visit_others")
modelCols = c("Lonsdorf.INVEST.CORINE_man0_mod0"  , "Lonsdorf.INVEST.CORINE_man0_mod1",    "Lonsdorf.INVEST.CORINE_man1_mod0",    "Lonsdorf.INVEST.CORINE_man1_mod1",
              "Lonsdorf.ZonasNaturales_man0_mod0" , "Lonsdorf.ZonasNaturales_man0_mod1",   "Lonsdorf.ZonasNaturales_man1_mod0",  "Lonsdorf.ZonasNaturales_man1_mod1")
df_data   = df_data %>% select(c(fieldCols,modelCols)) # select columns

#Explore
boxplot(df_data$Zabundance ~ df_data$crop)
hist(df_data$Zabundance)
boxplot(df_data$ab_wild_comparable ~ df_data$crop)
hist(df_data$ab_wild_comparable)
(split_plot <- ggplot(aes(Lonsdorf.ZonasNaturales_man0_mod0, ab_wild_comparable), data = dat) + 
    geom_point() + 
    facet_wrap(~ study_id) + 
    xlab("model") + 
    ylab("ab_wild_comparable")) # See values by study_id
(split_plot <- ggplot(aes(Lonsdorf.INVEST.CORINE_man0_mod0, ab_wild_comparable), data = dat) + 
    geom_point() + 
    facet_wrap(~ study_id) + 
    xlab("model") + 
    ylab("ab_wild_comparable")) # See values by study_id

#############################
# SET DATA (EUROPE OR SPAIN) 
#############################
# SPAIN
df_spain = subset(df_data, country == "Spain")
dat = df_spain
dat = subset(dat, !is.na(Lonsdorf.ZonasNaturales_man0_mod0))

# EUROPE
dat = df_data
dat = subset(dat, !is.na(Lonsdorf.ZonasNaturales_man0_mod0))

# Standardize units somehow
sumCols                = c("ab_bombus","ab_wildbees","ab_syrphids","ab_humbleflies", "ab_nonbee_hymenoptera", "ab_lepidoptera") 
dat                    = subset(dat, !is.na(ab_wildbees)) # at least abundance of wildbees must be measured 
dat                    = subset(dat, !is.na(total_sampled_time)) # total sampled time needed 
dat[,sumCols]          = dat[,sumCols] %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))
dat$ab_wild_comparable = rowSums(dat[,sumCols]) / dat$total_sampled_time
dat                    = dat %>% group_by(study_id) %>% mutate(Zabundance = scale(ab_wild_comparable))

###############################
# MODELS
###############################
# Lonsdorf.ZonasNaturales mod0
model <- glmer.nb(ab_wild_comparable ~ Lonsdorf.ZonasNaturales_man0_mod0 + (1|study_id), data = dat, na.action = na.omit)
r.squaredGLMM(model)
# qqnorm(resid(model))
# qqline(resid(model))

# Lonsdorf.ZonasNaturales mod1
model <- glmer.nb(ab_wild_comparable ~ Lonsdorf.ZonasNaturales_man0_mod1 + (1|study_id), data = dat, na.action = na.omit)
r.squaredGLMM(model)

# Lonsdorf.INVEST.CORINE mod0
model <- glmer.nb(ab_wild_comparable ~ Lonsdorf.INVEST.CORINE_man0_mod0 + (1|study_id), data = dat, na.action = na.omit)
r.squaredGLMM(model)

#  Lonsdorf.INVEST.CORINE mod1
model <- glmer.nb(ab_wild_comparable ~ Lonsdorf.INVEST.CORINE_man0_mod1 + (1|study_id), data = dat, na.action = na.omit)
r.squaredGLMM(model)

# Z abundance
model <- lme(Zabundance ~ Lonsdorf.INVEST.CORINE_man0_mod1, random = ~1|study_id, data = dat, na.action = na.omit)


##############################
# PLOTS
##############################
term = "Lonsdorf.INVEST.CORINE_man0_mod1" # write predictor used here, e.g. "Lonsdorf.INVEST.CORINE_man0_mod1"
# Predicted values
pred.mm <- ggpredict(model, terms = term)  # this gives overall predictions for the model
(ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = dat,                      # adding the raw data (scaled values)
               aes(x = Lonsdorf.INVEST.CORINE_man0_mod1, y = ab_wild_comparable, colour = crop)) + 
    xlab("Predicted") +      
    ylab("Observed abundance") + theme_bw()  + theme (legend.position= "right") +
    theme(text = element_text(size = 18), axis.text=element_text(size=rel(1)), axis.title=element_text(size=rel(1.3), face="bold")) + 
    scale_fill_discrete(name="Crop") +  
    theme(legend.text = element_text(size = 14, face = "italic"), legend.title = element_text(size=15, face = "bold"))
)

# Linear fit
scatter.smooth(dat$ab_wild_comparable ~ dat$Lonsdorf.ZonasNaturales_man0_mod0)
(p2<-ggplot(dat, aes(Lonsdorf.INVEST.CORINE_man0_mod0, ab_wild_comparable)) +
    geom_point(size=2.5, shape=21,color = "black", aes(fill=factor(crop))) +
    geom_smooth(method="lm", size=1, se=TRUE, color = "black", alpha=0.4) +
    xlab("Predicted") +      
    ylab("Observed abundance") + theme_bw()  + theme (legend.position= "right") +
    theme(text = element_text(size = 18), axis.text=element_text(size=rel(1)), axis.title=element_text(size=rel(1.3), face="bold")) + 
    scale_fill_discrete(name="Crop") +  
    theme(legend.text = element_text(size = 14, face = "italic"), legend.title = element_text(size=15, face = "bold")))

##############################
# TABLES
##############################
tab_model(model)
stargazer(model, type = "text",
          +           digits = 3,
          +           star.cutoffs = c(0.05, 0.01, 0.001),
          +           digit.separator = "")