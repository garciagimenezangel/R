library(dplyr)

df = read.csv(file= "C:/Users/angel.gimenez/Google Drive/GEE outputs/visit_index.csv", header=T);

sandias2013 = df[df$YEA == 2013,]
sandias2014 = df[df$YEA == 2014,]
sandias2015 = df[df$YEA == 2015,]
sandias2016 = df[df$YEA == 2016,]

par(mfrow = c(2, 2))  

lin_reg2013 = lm(sandias2013$D9_RTO~sandias2013$remapped_sum, data.frame(sandias2013$remapped_sum, sandias2013$D9_RTO));
lin_reg2014 = lm(sandias2014$D9_RTO~sandias2014$remapped_sum, data.frame(sandias2014$remapped_sum, sandias2014$D9_RTO));
lin_reg2015 = lm(sandias2015$D9_RTO~sandias2015$remapped_sum, data.frame(sandias2015$remapped_sum, sandias2015$D9_RTO));
lin_reg2016 = lm(sandias2016$D9_RTO~sandias2016$remapped_sum, data.frame(sandias2016$remapped_sum, sandias2016$D9_RTO));

plot(sandias2013$remapped_sum, sandias2013$D9_RTO);
abline(lin_reg2013);

plot(sandias2014$remapped_sum, sandias2014$D9_RTO);
abline(lin_reg2014);

plot(sandias2015$remapped_sum, sandias2015$D9_RTO);
abline(lin_reg2015);

plot(sandias2016$remapped_sum, sandias2016$D9_RTO);
abline(lin_reg2016);

summ2013 = summary(lin_reg2013);
summ2014 = summary(lin_reg2014);
summ2015 = summary(lin_reg2015);
summ2016 = summary(lin_reg2016);

summ2013$r.squared
summ2014$r.squared
summ2015$r.squared
summ2016$r.squared

