# A_module10.R
# Group A assignment for Module 10
# Drew Robison, Connor Breton, Korik Vargas
# Git Repo: A_module10

##################################################################################################################

# Create and save an R script in RStudio with the file name GroupLetter_module10.R.  You will submit this
# file as this week’s assignment, so be sure to follow good coding practices throughout.  Include your 
# answers to the following questions as labeled comments in the script.Set your working directory and read 
# in the frangula.csv data file.  [NOTE: this file is the same dataset used in class]

# Set working directory:
setwd("C:/Users/Connor/Documents/word_files/graduate_courses/r/module_10")

# Read in 'fangula' data
Data <- read.table("frangula.csv", sep=",", header=T)

# Load "ggplot2" into current R work session
library(ggplot2)


##################################################################################################################


# (1)	We completed a model comparison for the plant height data in class, but did not have  the full set of all
# possible covariate combinations (one, two, and three predictor models).  Using the predictors basal.diam,
# treatment.sp, and bioassay.sp, construct the remaining linear models with height as the response (no 
# interactions terms) and find the best fit model among all models (including those constructed in class). 
# Interpret the results of your best fit model and be sure to check model assumptions.


# Run 7 regression models for each explanatory variables (n = 3), each combination of 2 explanatory variables
# (n = 3), and one that includes all three explanatory variables (n = 1). For the categorical variables
# (treatment.sp and bioassay.sp), must use 'factor'.
m1.basal <- lm(height ~ basal.diam, data = Data)
m2.treat <- lm(height ~ factor(treatment.sp), data = Data)
m3.bioas <- lm(height ~ factor(bioassay.sp), data = Data)
m4.bas.tre <- lm(height ~ basal.diam + factor(treatment.sp), data = Data)
m5.bas.bio <- lm(height ~ basal.diam + factor(bioassay.sp), data = Data)
m6.tre.bio <- lm(height ~ factor(treatment.sp) + factor(bioassay.sp), data = Data)
m7.bas.tre.bio <- lm(height ~ basal.diam + factor(treatment.sp) + factor(bioassay.sp), data = Data)


# Use the AIC to determine which model provides the best fit. First find the AIC of the seven models, then 
# determine the index of the minimum AIC, which indicates the best model fit while taking into account the
# number of explanatory variables.
aic = AIC(m1.basal, m2.treat, m3.bioas, m4.bas.tre, m5.bas.bio, m6.tre.bio, m7.bas.tre.bio)
best.m.num <- which.min(aic$AIC)
best.m.num
min.AIC <- min(aic$AIC)
min.AIC
# The best fit model is the one which includes all three explanatory variables (m7.bas.tre.bio). The AIC value
# for this model is 902.7335.


# Evaluate the results of our best fit model.
# Rename model for ease of use
m7 <- m7.bas.tre.bio


# Summarize model output and examine calculated coefficients
summary(m7)
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

# Overall regression equation follows, with coefficients rounded to 3 significant digits:
# height = 17.1 + 3.93*basal.dim + 2.14(if treatment.sp = Frangula) - 7.23(if bioassay.sp = Spirea) - 
# 16.1(if bioassay.sp = Viburnum)


# Check model assumptions and interpret best fit model

# Check distribution of residual values
par(mfrow = c(1,1))
hist(m7$residuals)
# Distribution of residual values is apparoximately normal.


# Check four main plots useful for model investigation/nterpretation
par(mfrow = c(2,2))
plot(m7)
# Fitted line for "Residuals vs Fitted" and "Scale-Location" figures are curved but ROUGHLY horizontal (e.g. point
# distributions are fairly even across fitted lines). For the "Normal Q-Q" plot, the majority of values are fitted 
# appropriately to the normal distribution quantile line, though values tend to stray from the line towards each 
# end. Several potential outliers are flaggd in the "Residuals vs Leverage" plot.


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
plot(m7, which =4)
# Potential outliers include observations (rows) 13, 38, and 39.


# Remove potential outliers (13, 38, 39) from data set
Data.out <- Data[-c(13,38,39), ]


# Check summary of original best fit model (includes all data points)
summary(m7)


# Check summary of new best fit model (previously identified outliers removed)
new.m7 <- lm(height ~ basal.diam + factor(treatment.sp) + factor(bioassay.sp), data = Data.out)
summary(new.m7)


# Though model "new.m3.bas.tre.bio" (outliers removed) has higher unadjusted and adjusted R-squared values 
# than model "m3.bas.tre.bio" (original best fit), and the exact estimates of some predictor variables have 
# changed, I would not draw different conclusions about the effect of each predictor variable on height. 
# One notable change, however, is that "factor(treatment.sp)Frangula", which previously had a P value of 
# less than .05, now has a P value of less than .01. This change does not change my conclusion that treatment 
# (in this case litter type) has a significant effect on plant height.

#########################
# Drew, how did you get it to give you new AIC values? I am getting an error due to a different number of
# fitted observations
#########################

### I have the same error and for the same reason ###

# Compare model fits using AIC
aic.2 <- AIC(m7, new.m7)
best.m.num <- which.min(aic.2$AIC)
best.m.num
# The model fit is imporved by removing outliers. The AIC improves from approximately 903 to 826.


# Summarize model output and examine calculated coefficients
new.m7.output <- summary(new.m7)
new.m7.output$coefficients
# The regression equation follows, with coefficients rounded to 3 significant digits:
# height = 19.9 + 3.35*basal.dim + 1.96(if treatment.sp = Frangula) - 8.81(if bioassay.sp = Spirea) - 
# 17.1(if bioassay.sp = Viburnum)
# All coefficients are significant with p < 0.001 for all except the treatment.sp factor, in which p = 0.01.
# However, the magnitude of coefficients is not very largely different from those of the linear model with
# outliers still in place.

# Examine r2 fit
new.m7.output$r.squared
# The r2 improves to 0.74 (from 0.66)


###################################################################################################################


# (3)	We might expect that the number of flowers and fruit produced would have similar drivers.  Use model 
# comparison to find the best fit model to explain the number of fruit and the number of flowers produced. 
# Restrict your covariate set to basal diameter and treatment organ and do not include interaction terms.  
# Do the same covariates best explain both flower and fruit production?  What best explains flower 
# production?  What best explains fruit production? Do your models explain fruit and flower prediction well?  

# Since the flower and fruit is count data, we can use a Poisson distribution. So we can run a model for
# flower numbers with basal diamter, treatment organism, or both as explanatory variables, remembering that
# treatment organism is a categorical variable. We save the summary of each model for use later in comparing
# models.

#### I didn't use poisson distribution and obviously the results are different. The best model for flowers is different, 
#### but for fruit production is the same
#### My question is: for count data should we use always Poisson distribution? How did you know about that?

m1p.flo.bas <- glm(flower.num ~ basal.diam, data = Data, family = "poisson")
m1p.sum <- summary(m1p.flo.bas)

m2p.flo.tre <- glm(flower.num ~ factor(treatment.organ), data = Data, family = "poisson")
m2p.sum <- summary(m2p.flo.tre)

m3p.flo.bas.tre <- glm(flower.num ~ basal.diam + factor(treatment.organ), data = Data, family = "poisson")
m3p.sum <- summary(m3p.flo.bas.tre)

# We can view the AIc values for each model fit to determine the best model for our data.
mp.flo.AIC <- c(m1p.sum$aic, m2p.sum$aic, m3p.sum$aic)
mp.flo.AIC
# The best model for the number of flowers combines both basal diameter and treatment organism (AIC = 444).

# Apply a chi-squared test to the deviance residuals to get an estimate 'goodness of fit'
print(p.chisq <- pchisq(m3p.flo.bas.tre$deviance, m3p.flo.bas.tre$df.residual, lower.tail=FALSE))
# Because this results in a value << 0, we can say the model has a poor fit


########## Fruit Data ##########


# The same process can be used on fruit data.
m4p.fru.bas <- glm(fruit.num ~ basal.diam, data = Data, family = "poisson")
m4p.sum <- summary(m4p.fru.bas)

m5p.fru.tre <- glm(fruit.num ~ factor(treatment.organ), data = Data, family = "poisson")
m5p.sum <- summary(m5p.fru.tre)

m6p.fru.bas.tre <- glm(fruit.num ~ basal.diam + factor(treatment.organ), data = Data, family = "poisson")
m6p.sum <- summary(m6p.fru.bas.tre)

mp.fru.AIC <- c(m4p.sum$aic, m5p.sum$aic, m6p.sum$aic)
mp.fru.AIC
# The best model for the number of flowers uses only the treatment organism (AIC = 67.7)

# Apply a chi-squared test to the deviance residuals to get an estimate 'goodness of fit'
print(p.chisq <- pchisq(m5p.fru.tre$deviance, m5p.fru.tre$df.residual, lower.tail=FALSE))
# Because this results in a value = 1, we can say the model has a good fit. However, if we inspect the data
# further, we can see that there is fruit on only one sample. 
# If we sum all the rows with values greater than 0, it will return the number of samples with fruit
sum(Data$fruit.num > 0)
# We get only one sample. So we should be wary of developing a model with one positive result in 144 samples.
# We can view this overwhelming leverage of a single point in the following plot:
plot(m5p.fru.tre, which = 4)


###################################################################################################################


# In 1-2 sentences, identify the contribution of each group member to the assignment.  Upload your .R file 
# to submit your assignment in myCourses, one per group.
