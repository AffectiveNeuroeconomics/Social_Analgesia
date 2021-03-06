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
##  Test absence of group          ##
# ******************************** #

model.nogroup <- '
                # combined outcome model: Y ~ c*X; Mediator model 2: Y ~ b*M
# ------------------------
diff_pre_post ~ c*impression_ratings + age

# mediator models 1: M ~ a*X; 
# --------------------------------------
learning_signal ~ a*impression_ratings + age 
diff_pre_post ~ b*learning_signal #(age omitted because duplicate model element)

indirect := a*b
direct   := c
total    := c + (a*b)
'

fit.ng <- sem(model.nogroup, data = dat, test = "bollen.stine")
summary(fit.ng, standardized = T, fit.measures = T, rsq = T)


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

fit <- sem(model.simp, data = dat, group = "group", test = "bollen.stine")
summary(fit, standardized = T, fit.measures = T, rsq = T)


# ********************************************* #
## comparison models with specific constraints ##
# ********************************************* #

### Constraint 1: indirect paths do not differ across groups ###
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
summary(fit1, standardized = T, fit.measures = T, rsq = T)


### Constraint 2: direct paths do not differ across groups ###
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
summary(fit2, standardized = T, fit.measures = T, rsq = T)

# These anovas test the influence of constraints on model fit.
# These model comparisons form the basis of the inferences on group influences, as the enable
# testing the influence of group on the constrained parameters. 

anova(fit,fit1,test="Chisq")
anova(fit,fit2,test="Chisq")
