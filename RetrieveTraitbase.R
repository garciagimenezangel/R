library(dplyr)

# working directory
work_dir = "C:/Users/angel.gimenez/Documents/Projects/OBServ/R/";
setwd(work_dir)
data_file = "../Data/Traitbase/Apoidea/data.csv"
tdata = read.csv(file=data_file,header=T)

summary(tdata %>% select(m_plant_genus))
summary(tdata %>% select(m_floral_specialization))
summary(tdata %>% select(m_plant_species))
summary(tdata %>% select(m_it))
summary(tdata %>% select(m_nest_site))
summary(tdata %>% select(m_nest_location))
summary(tdata %>% select(m_nest_construction))
