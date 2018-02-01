### ******************************** ###
### Moderated Mediation using Lavaan ###
### ******************************** ###

# This script runs a moderated mediation model with a grouping variable using Lavaan
# data file: mediation_data.csv

# Clear all
rm(list = ls(all = TRUE))

library('lavaan')

# ******************************** #
# Read in data
# ******************************** #

setwd('D:\\OneDrive\\Research\\GroupHelpAndPainRelief\\Github\\')
dat <- read.table('mediation_data.csv', header = T, sep = ",")
names(dat)

# ******************************** #
## core mediation model           ##
# ******************************** #

model.simp <- '
                # combined outcome model: Y ~ c*X; Mediator model 2: Y ~ b*M
                # ------------------------
                diff_pre_post ~ c(c1,c2)*impression_ratings + age

                # mediator models 1: M ~ a*X; 
                # --------------------------------------
                learning_signal ~ a(a1,a2)*impression_ratings + age 
                diff_pre_post ~ b(b1,b2)*learning_signal #+ age

                indirect1 := a1*b1
                indirect2 := a2*b2

                direct1   := c1
                direct2   := c2

                total1    := c1 + (a1*b1)
                total2    := c2 + (a2*b2) 

'
# G_on_a1a2 := a1 - a2 G_on_b1b2 := b1 - b2 G_on_ind := indirect1 - indirect2 G_on_dir := direct1 - direct2 G_on_tot := total1 - total2

fit <- sem(model.simp, data = dat, group = "group",  test = "bootstrap")
summary(fit)


# ********************************************* #
## comparison models with specific constraints ##
# ********************************************* #

### Constraint 1 ###
model.simp1 <- '
                # combined outcome model: Y ~ c*X; Mediator model 2: Y ~ b*M
                # ------------------------
                diff_pre_post ~ c(c1,c2)*impression_ratings + age #d(v3,v3)*age

                # mediator models 1: M ~ a*X; 
                # --------------------------------------
                learning_signal ~ a(a1,a2)*impression_ratings + age #d(v3,v3)*age
                diff_pre_post ~ b(b1,b2)*learning_signal #+ age

                indirect1 := a1*b1
                indirect2 := a2*b2

                direct1   := c1
                direct2   := c2

                total1    := c1 + (a1*b1)
                total2    := c2 + (a2*b2) 

#constraint
indirect1 == indirect2
'

fit1 <- sem(model.simp1, data = dat, group = "group")
summary(fit1)


### Constraint 2 ###
model.simp2 <- '
                # combined outcome model: Y ~ c*X; Mediator model 2: Y ~ b*M
                # ------------------------
                diff_pre_post ~ c(c1,c2)*impression_ratings + age #d(v3,v3)*age

                # mediator models 1: M ~ a*X; 
                # --------------------------------------
                learning_signal ~ a(a1,a2)*impression_ratings + age #d(v3,v3)*age
                diff_pre_post ~ b(b1,b2)*learning_signal #+ age

                indirect1 := a1*b1
                indirect2 := a2*b2

                direct1   := c1
                direct2   := c2

                total1    := c1 + (a1*b1)
                total2    := c2 + (a2*b2) 

#constraint
direct1 == direct2
'

fit2 <- sem(model.simp2, data = dat, group = "group")
summary(fit2)


### Constraint 3 ###
model.simp3 <- '
                # combined outcome model: Y ~ c*X; Mediator model 2: Y ~ b*M
                # ------------------------
                diff_pre_post ~ c(c1,c2)*impression_ratings + age #d(v3,v3)*age

                # mediator models 1: M ~ a*X; 
                # --------------------------------------
                learning_signal ~ a(a1,a2)*impression_ratings + age #d(v3,v3)*age
                diff_pre_post ~ b(b1,b2)*learning_signal #+ age

                indirect1 := a1*b1
                indirect2 := a2*b2

                direct1   := c1
                direct2   := c2

                total1    := c1 + (a1*b1)
                total2    := c2 + (a2*b2) 

#constraint
total1 == total2
'

fit3 <- sem(model.simp3, data = dat, group = "group")
summary(fit3)


### Constraint 3 ###
model.simp4 <- '
                # combined outcome model: Y ~ c*X; Mediator model 2: Y ~ b*M
                # ------------------------
                diff_pre_post ~ c(c1,c2)*impression_ratings + age #d(v3,v3)*age

                # mediator models 1: M ~ a*X; 
                # --------------------------------------
                learning_signal ~ a(a1,a2)*impression_ratings + age #d(v3,v3)*age
                diff_pre_post ~ b(b1,b2)*learning_signal #+ age

                indirect1 := a1*b1
                indirect2 := a2*b2

                direct1   := c1
                direct2   := c2

                total1    := c1 + (a1*b1)
                total2    := c2 + (a2*b2) 

#constraint
a1 == a2
'

fit4 <- sem(model.simp4, data = dat, group = "group")
summary(fit4)


### Constraint 3 ###
model.simp5 <- '
                # combined outcome model: Y ~ c*X; Mediator model 2: Y ~ b*M
                # ------------------------
                diff_pre_post ~ c(c1,c2)*impression_ratings + age #d(v3,v3)*age

                # mediator models 1: M ~ a*X; 
                # --------------------------------------
                learning_signal ~ a(a1,a2)*impression_ratings + age #d(v3,v3)*age
                diff_pre_post ~ b(b1,b2)*learning_signal #+ age

                indirect1 := a1*b1
                indirect2 := a2*b2

                direct1   := c1
                direct2   := c2

                total1    := c1 + (a1*b1)
                total2    := c2 + (a2*b2) 

#constraint
b1 == b2
'

fit5 <- sem(model.simp, data = dat, group = "group")
summary(fit5)


# These anovas test the influence of constraining aspects of the model on model fit.
# They allow for tests of the influence of group on the contrained parameters. These model comparisons form the basis of the 
# inferences on group influences .

anova(fit,fit1,test="Chisq")
anova(fit,fit2,test="Chisq")
anova(fit,fit3,test="Chisq")

anova(fit,fit4,test="Chisq")
anova(fit,fit5,test="Chisq")