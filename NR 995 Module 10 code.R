## NR 995 Module 10: linear regression and GLM (Poisson regression)
## Jenica Allen, instructor
## Last modified: 26 October 2017

library(ggplot2)

setwd("C:\\Users\\Localadmin\\Box Sync\\Teaching\\NR 995 Data Manip Model in R\\Fall 2017\\Module 10\\data")

############################################################
## load and explore data
############################################################

buck = read.table("frangula.csv", sep=",", header=T)

head(buck)
str(buck)
summary(buck)
dim(buck)

#######################################################
## simple linear regression
######################################################

## H1: taller plants have larger basal diamters
## Ho: plant height is not affected by basal diameter

## check for linear (or non-linear) relationships visually (linear regression assumption)
plot(height ~ basal.diam, data = buck, xlab="Basal diameter (mm)", ylab = "Height (cm)")

## assess distribution of predictor and response data
hist(buck$basal.diam)
hist(buck$height)

## linear regression with continuous variables (predictor and response)
## use formula notation boxplots
m.1 <- lm(height ~ basal.diam, data = buck)
summary(m.1)

## walk through interpretation (slope, intercept, p-values, R^2)

## accessing output components


m.1$fitted.values

out <- summary(m.1)
head(out)

out$coefficients
out$r.squared


## check normality of residuals-- histogram
m.1$residuals
hist(m.1$residuals)

## check normality of residuals-- normal Q-Q plot
## points should fall on line 
plot(m.1, which = 2)

## check homogeneity of residuals
## fitted vs resids want even scatter across x values
plot(m.1, which = 1)

## the good news is that linear regression is fairly robust to violations of residual assumptions
## how much deviation from assumption is too much is as much art as science (see Zuur et al. 2010)

## how about leverage (i.e., outliers)?
## observations with high Cook's distance are suspect high leverage points
## high leverage means that the point has a larger effect than other points on the model fit
## high leverage points should be investigated, but not automatically discarded (see Zuur et al. 2010)
plot(m.1, which = 4)

########################################################
## Plotting with Regression
#########################################################

## plotting
plot(height ~ basal.diam, data = buck, xlab="Basal diameter (mm)", ylab = "Height (cm)")
abline(lm(height ~ basal.diam, data = buck))

## add equation and stats to plot

# create object with coefs
coefs = out$coefficients 
coefs

# create object that contains the equation
eq <- paste("height = ", coefs[2, 1], "*basal diameter + ", coefs[1, 1], sep="")

# add the equation to the plot
text(1, 42, eq, cex = 0.5, pos = 4)

## alternative version of the equation, rounded coefficients
eq.1 <- paste("height = ", round(coefs[2, 1], digits = 3), "*basal diameter + ", round(coefs[1, 1], digits = 3), sep="")
text(1, 42, eq.1, cex = 0.5, pos = 4)

# create object with R-squared value
r.sq <- out$r.squared  

# add R-squared value to plot
text(1, 39, bquote(R^2 == .(round(r.sq, 3))), cex = 0.5, pos = 4)

#########################################################
## Linear regression with categorical predictors
#########################################################

## categorical predictor
m.2 <- lm(height ~ factor(treatment.sp), data = buck)
summary(m.2)

## categorical predictors can be used, but less intuitive interpretation
## interpretation of coefficients is RELATIVE to a baseline group (Cornus in this case)
## height is 2.378 cm more with frangula as the treatment species than cornus, BUT note that the coefficient is not statistically significant
## so we conclude that the treatment species has no effect on height

## check normality of residuals
hist(m.2$residuals)

## get all regression diagnostic plots together
## scale-location plot is a version of the residual vs fitted, with the residuals standardized
## leverage plot looks different when called this way-- puts leverage on the x and lables high leverage points on plot
## the labeled points can sometimes be hard to see in this visualization, can always plot Cook's on it's own
par(mfrow = c(2,2))
plot(m.2)

## remember that we have a categorical predictor, so the fitted valued are going to be similar for those groups
## plots are not super helpful in this case

#########################################################
## Multiple Linear Regression
#######################################################

## run model and interpret
m.3 <- lm(height ~ basal.diam + factor(treatment.sp), data = buck)
summary(m.3)

## check normailty of resuduals
par(mfrow = c(1,1))
hist(m.3$residuals)

## get all regression diagnostic plots together
## looks reasonable, a few possible high leverage points to investigate
par(mfrow = c(2,2))
plot(m.3)

## note that we should be thinking about collinearity of predictors
## here we have 1 continuous and one categorical predictor, so difficult to assess
## can can look at boxplots of the continuous predictor grouped by the categorical one
## we would not want to see that the basal diameter was always much larger for one treatment.sp 
ggplot(data = buck, aes(x = treatment.sp, y = basal.diam)) +
  geom_boxplot() 

########################################################
## CODE PRACTICE
#######################################################

## create a multiple linear regression that includes basal diameter, treatment species, and bioassay species
m.4 <- lm(height ~ basal.diam + factor(treatment.sp) + factor(bioassay.sp), data = buck)
summary(m.4) 

## check normailty of resuduals
par(mfrow = c(1,1))
hist(m.4$residuals)

## get all regression diagnostic plots together
## looks reasonable, a few possible high leverage points to investigate
par(mfrow = c(2,2))
plot(m.4)

## consider collinearity of new variable
ggplot(data = buck, aes(x = bioassay.sp, y = basal.diam)) +
geom_boxplot() 


###########################################################
## Covariate Selection (AIC)
#########################################################

### combine AIC values from our 4 candidate models
aic = AIC(m.1, m.2, m.3, m.4)
aic

## find the minimum AIC
## this is the best fit model
AICm = min(aic$AIC) 

## calculate delta AIC-- how much larger are all models than the best fit model?
aic$delta = aic$AIC - AICm
aic

## models with delta AIC < 10 worthy of more consideration
## modeils with delta AIC < 2 are considered equivalent in fit-- use parsimony to select (fewest parameters)
## often one clear best model (as in here)

######################################################
## GLM: Poisson Regression
######################################################

## Poisson distribution useful for count data
## the 

## examine the relationship between flower number and basal diameter
## do larger plants have more flowers?

m.pois <- glm(flower.num ~ basal.diam, data = buck, family = "poisson")

## look at output
## significance of coefficients
## AIC value (only meaningful for comparison to another model)
summary(m.pois)

## look at coefficient point estimates
m.pois$coefficients

# exponentiate intercept to get expected mean number of flowers for a tiny plant (when x = 0)
# very few! (0.0006)
exp(m.pois$coefficients[1])

# exponentiate basal diameter coefficient to interpret
## for every unit (mm) increase in basal diameter, the number of flowers goes up by 5.87
exp(m.pois$coefficients[2])

par(mfrow = c(2,2))
plot(m.pois)

## one tricky part of Poisson models (and GLMs in general)
## is that there aren't great options for quantifying "goodness of fit"
## akin to an R-squared value

## you will sometimes see the suggestion to apply a chi-squared test
## to the deviance residuals (which are similar in concept to residuals in general linear regression)
## but there are assumptions required (e.g., large sample size)
## that often do not hold for ecological data sets
## the null hypothesis is that the model is appropriate for the data
## so small p-values indicate poor fit
print(p.chisq <- pchisq(m.pois$deviance, m.pois$df.residual, lower.tail=FALSE))


## but note that we have many zeroes in this dataset
## calls for a zero-inflated poisson model
## see the package pscl for functions to fit zero-inflated Poisson models
plot(buck$basal.diam, buck$flower.num)



