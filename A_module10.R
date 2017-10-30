# A_module10.R
# Group A assignment for Module 10
# Drew Robison, Connor Breton, Korik Vargas
# Git Repo: A_module10

# Create and save an R script in RStudio with the file name GroupLetter_module10.R.  You will submit this
# file as this week’s assignment, so be sure to follow good coding practices throughout.  Include your 
# answers to the following questions as labeled comments in the script.Set your working directory and read 
# in the frangula.csv data file.  [NOTE: this file is the same dataset used in class]

# Set working directory:
setwd("C:\\Users\\Drew\\Documents\\UNH\\Courses\\NR 995 - R\\Modules\\10")

# Read in 'fangula' data
Data = read.table("frangula.csv", sep=",", header=T)



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


aic = AIC(m1.basal, m1.treat, m1.bioas) #, m2.bas.tre, m2.bas.bio, m2.tre.bio, m3.bas.tre.bio)
aic 






# (2)	Using model diagnostics for your best fit model in Question 1, identify potential outliers in your 
# best fit model. Explore the effect of these observations on your results by removing potential outliers 
# from your analysis and comparing the output to the best fit model that includes all observations.  Do 
# any of the potential outliers qualitatively affect the model results?  In other words, would you come to
# a different conclusion about the effect of the predictors on height if you removed the outliers?










# (3)	We might expect that the number of flowers and fruit produced would have similar drivers.  Use model 
# comparison to find the best fit model to explain the number of fruit and the number of flowers produced. 
# Restrict your covariate set to basal diameter and treatment organ and do not include interaction terms.  
# Do the same covariates best explain both flower and fruit production?  What best explains flower 
# production?  What best explains fruit production? Do your models explain fruit and flower prediction well?  
  









  
# In 1-2 sentences, identify the contribution of each group member to the assignment.  Upload your .R file 
# to submit your assignment in myCourses, one per group.
