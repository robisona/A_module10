# A_module10.R
# Group A assignment for Module 10
# Drew Robison, Connor Breton, Korik Vargas
# Git Repo: A_module10

# Create and save an R script in RStudio with the file name GroupLetter_module10.R.  You will submit this
# file as this week’s assignment, so be sure to follow good coding practices throughout.  Include your 
# answers to the following questions as labeled comments in the script.Set your working directory and read 
# in the frangula.csv data file.  [NOTE: this file is the same dataset used in class]

# Set working directory:
setwd("C:/Users/Connor/Documents/word_files/graduate_courses/r/module_10")

# Read in 'fangula' data
buck <- read.table("frangula.csv", sep=",", header=T)

# Load "ggplot2" into current R work session
library(ggplot2)


# (1)	We completed a model comparison for the plant height data in class, but did not have  the full set of all
# possible covariate combinations (one, two, and three predictor models).  Using the predictors basal.diam,
# treatment.sp, and bioassay.sp, construct the remaining linear models with height as the response (no 
# interactions terms) and find the best fit model among all models (including those constructed in class). 
# Interpret the results of your best fit model and be sure to check model assumptions.

m1.basal <- lm(height ~ basal.diam, data = buck)
m1.treat <- lm(height ~ treatment.sp, data = buck)
m1.bioas <- lm(height ~ bioassay.sp, data = buck)
m2.bas.tre <- lm(height ~ basal.diam + factor(treatment.sp), data = buck)
m2.bas.bio <- lm(height ~ basal.diam + factor(bioassay.sp), data = buck)
m2.tre.bio <- lm(height ~ treatment.sp + factor(bioassay.sp), data = buck)
m3.bas.tre.bio <- lm(height ~ basal.diam + factor(treatment.sp) + factor(bioassay.sp), data = buck)


# Determine AIC value for each model
aic = AIC(m1.basal, m1.treat, m1.bioas, m2.bas.tre, m2.bas.bio, m2.tre.bio, m3.bas.tre.bio)
aic   
# AIC values range from 902.7335 to 1048.6183


# Find the best fit model (model with lowest AIC value)
AICm = min(aic$AIC) 
AICm  
# Lowest AIC value is 902.7335, therefore the best fit model is m3.bas.tre.bio. This model includes 
# "basal.diam", "treatment.sp", and "bioassay.sp" as predictor variables


# Calculate delta AIC values for each model to determine the difference between each model and the best 
# fit model
aic$delta = aic$AIC - AICm
aic
# The largest difference was between model "m3.bas.tre.bio" (best fit) and model "m1.treat" 
# (delta value ~ 145.88). The smallest difference was between model "m3.bas.tre.bio" and model
# "m2.bas.bio" (delta value ~ 3.71).


summary(m3.bas.tre.bio) # Returns summary of "m3.bas.tre.bio" (our best model)
# Unadjusted R-squared = 0.6577
# Adjusted R-squared = 0.6478
# (Intercept) (P<.001), basal.diam (P<.001), factor(treatment.sp)Frangula (P<.05), 
# factor(bioassay.sp)Spirea (P<.001), and factor(bioassay.sp)Viburnum (P<.001)

# Plant height increases 3.9306 units per unit increase in basal diameter.
# All else equal, plants in Frangula litter are 2.1465 units taller than plants in Cornus litter.
# All else equal, plants associated with Spirea bioassay are 7.22066 units shorter than plants
# associated with Alnus bioassay.
# All else equal, plants associated with Viburnum bioassay are 16.1097 units shorter than plants
# associated with Alnus bioassay.


# Check model assumptions and interpret best fit model

# Check distribution of residual values
par(mfrow = c(1,1))
hist(m3.bas.tre.bio$residuals)
# Distribution of residual values is apparoximately normal.


# Check four main plots useful for model investigation/nterpretation
par(mfrow = c(2,2))
plot(m3.bas.tre.bio)
# Fitted line for "Residuals vs Fitted" and "Scale-Location" figures are curved but ROUGHLY horizontal. For the
# "Normal Q-Q" plot, the majority of values are fitted appropriately to the normal distribution quantile line, 
# though values tend to stray from the line towards each end. Several potential outliers are flaggd in the
# "Residuals vs Leverage" plot.


# Check collinearity of variables
ggplot(data = buck, aes(x = bioassay.sp, y = basal.diam)) +
  geom_boxplot() 
# No systematic differences between basal diameters of different types of plants found

ggplot(data = buck, aes(x = treatment.sp, y = basal.diam)) +
  geom_boxplot() 
# No systematic differences between basal diameters of plants in different treatments found


###################################################################################################################


# (2)	Using model diagnostics for your best fit model in Question 1, identify potential outliers in your 
# best fit model. Explore the effect of these observations on your results by removing potential outliers 
# from your analysis and comparing the output to the best fit model that includes all observations.  Do 
# any of the potential outliers qualitatively affect the model results?  In other words, would you come to
# a different conclusion about the effect of the predictors on height if you removed the outliers?

# Determine potential outliers with Cook's Distance
par(mfrow = c(1,1))
plot(m3.bas.tre.bio, which =4)
# Potential outliers include observations (rows) 13, 38, and 39.


# Remove potential outliers (13, 38, 39) from data set
buck.out <- buck[-c(13,38,39), ]


# Check summary of original best fit model (includes all data points)
m3.bas.tre.bio <- lm(height ~ basal.diam + factor(treatment.sp) + factor(bioassay.sp), data = buck)
summary(m3.bas.tre.bio)


# Check summary of new best fit model (previously identified outliers removed)
new.m3.bas.tre.bio <- lm(height ~ basal.diam + factor(treatment.sp) + factor(bioassay.sp), data = buck.out)
summary(new.m3.bas.tre.bio)


# Though model "new.m3.bas.tre.bio" (outliers removed) has higher unadjusted and adjusted R-squared values 
# than model "m3.bas.tre.bio" (original best fit), and the exact estimates of some predictor variables have 
# changed, I would not draw different conclusions about the effect of each predictor variable on height. 
# One notable change, however, is that "factor(treatment.sp)Frangula", which previously had a P value of 
# less than .05, now has a P value of less than .01. This change does not change my conclusion that treatment 
# (in this case litter type) has a significant effect on plant height.


###################################################################################################################


# (3)	We might expect that the number of flowers and fruit produced would have similar drivers.  Use model 
# comparison to find the best fit model to explain the number of fruit and the number of flowers produced. 
# Restrict your covariate set to basal diameter and treatment organ and do not include interaction terms.  
# Do the same covariates best explain both flower and fruit production?  What best explains flower 
# production?  What best explains fruit production? Do your models explain fruit and flower prediction well?  


m.flower.bd <- glm(flower.num ~ basal.diam, data = buck, family = "poisson")
# General linear model (GLM) for number of flowers and basal diameter
m.flower.trmnt <- glm(flower.num ~ treatment.sp, data = buck, family = "poisson")
# GLM for number of flowers and treatment

aic.flower = AIC(m.flower.bd, m.flower.trmnt)
aic.flower
# AIC determination of best fit model -- Here the best fit is "m.flower.bd" with an AIC value of 456.3056


m.fruit.bd <- glm(fruit.num ~ basal.diam, data = buck, family = "poisson")
# GLM for number of fruits and basal diameter
m.fruit.trmnt <- glm(fruit.num ~ treatment.sp, data = buck, family = "poisson")
# GLM for number of fruits and treatment

aic.fruit = AIC(m.fruit.bd, m.fruit.trmnt)
aic.fruit
# AIC determination of best fit model -- Here the best fit is "m.fruit.treatment" with an AIC value of 67.68091


# Determine model fit by checking P values (the lower the P value, the worse the fit)

print(p.chisq <- pchisq(m.flower.bd$deviance, m.flower.bd$df.residual, lower.tail=FALSE))
# P value of 1.0353 * 10^-31

print(p.chisq <- pchisq(m.flower.trmnt$deviance, m.flower.trmnt$df.residual, lower.tail=FALSE))
# P value of 6.454955 * 10^-66

print(p.chisq <- pchisq(m.fruit.bd$deviance, m.fruit.bd$df.residual, lower.tail=FALSE))
# P value of 1

print(p.chisq <- pchisq(m.fruit.trmnt$deviance, m.fruit.trmnt$df.residual, lower.tail=FALSE))
# P value of 1

# Based on the P values found above, it appears that only the two fruit models were of decent fit to our 
# collected data. Flower models were considered very poor fits to collected data due to extremely low P values.


###################################################################################################################

  
# In 1-2 sentences, identify the contribution of each group member to the assignment.  Upload your .R file 
# to submit your assignment in myCourses, one per group.
