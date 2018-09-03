rm(list = ls(all = TRUE))

library("ez") # for inferential analysis using ANOVA
require("sjstats")

setwd('D:\\OneDrive\\Research\\GroupHelpAndPainRelief\\Github\\anova\\')
Dat <- read.table('impression_ratings.csv',sep = ",", header=T)
names(Dat)

Dat$subject_f <- factor(Dat$Subject)

fit <- aov(Impress ~  Rated_group * group_f  + Error(subject_f/(Rated_group))+(group_f),
           data=Dat)
summary(fit)
cohens_f(fit)

ez_anova <- ezANOVA(Dat, Impress, subject_f, within = .(Rated_group),
                    between = group_f, type = 3)

print(ez_anova)