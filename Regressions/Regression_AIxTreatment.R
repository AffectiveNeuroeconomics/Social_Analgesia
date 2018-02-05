# Regression results the effects of learning (anterior insula learning signal, right panel a), 
# the effect of social context (treatment type, right panel b), and the interaction between 
# social context and learning (right panel c), on the individual pre- vs. post-treatment 
# differences in pain ratings (= delta pain rating).

# Clear all
rm(list = ls(all = TRUE))

# load packages
require(rms)
require(car)

setwd('D:\\OneDrive\\Research\\GroupHelpAndPainRelief\\Github\\Regressions\\')
dat <- read.table('reg_data.csv', header = T, sep = ",")
names(dat)

lm1 <- lm(diff_pre_post ~ insula_signal + impression_ratings + age , x=T, y=T, data=dat)
summary(lm1)

lm2 <- lm(diff_pre_post ~ group + impression_ratings + age , x=T, y=T, data=dat)
summary(lm2)

lm.full <- lm(diff_pre_post ~ insula_signal*group + impression_ratings + age , x=T, y=T, data=dat)
summary(lm.full)

# ANOVA results
anova_T1 <- anova(lm.full)
anova_T3 <- Anova(lm.full, type=3)

anova_T3