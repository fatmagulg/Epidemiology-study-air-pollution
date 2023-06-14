### ### ### ### ### ### ### ###
##### Epidemiology study #####
### ### ### ### ### ### ### ###

# Aim: estimate effect of exposure to air pollution on health
# What is the effect of air pollution on the risk of respiratory hospitalisation in Scotland?
# Data: data on air pollution, health, and confounders in 2015-2016 for each intermediate
# zone in Scotland

### Set up ###
dat <- read.csv("Air pollution and health.csv")
head(dat)
dim(dat)


##### SMR #####
# Standard Mortality Ratio (SMR) = Observed number of deaths / Expected number of deaths
# The number of observed hospitalisations in an IZ depends on the number of people living in that zone
# as well as population demographics (age, sex structure)
# Therefoe we must calculate SMR to get a standardised 
# Calculate SMR and add as a column to the data frame
dat$SMR <- dat$YhospResp / dat$EhospResp




##### Correlation matrix#####
# Produce correlation matrix of all variables that can be included in the model
# Using log(SMR) bc Poisson regression with log link it being used
M <- cor(cbind(log(dat$SMR), dat[, 5 :12]))
M

# Plot it
library(corrplot)
corrplot(M, method = 'circle', type = "full" 
         #,col = colorRampPalette(c("darkblue", "blue", "white", "pink", "red"))(10)
         )
# Education, income, and employment appear highly correlated with each other.
# Housing appears moderately correlated with education, income, and housing. 
# This makes intuitive sense; we would expect people who are educated to have higher income and hence
# afford better quality of housing. These all are essentially a measure of income so only include one
# in the model to avoid complications related to multicollinearity
# pm10 and no2 appear highly correlated. Again this is surely because they are both measures of air 
# pollution so we can consider retaining only retain one



##### Fit model #####
# We will use a Poisson regression model because our response is count type data
# We include an offset term to allow for variations in population size between the different IZ's. The offset 
# is on the log scale to match the log link we are using for the Poisson GLM

# Model with both no2 and pm10
mod1 <- glm(YhospResp ~ offset(log(EhospResp)) + income + crime + access + no2 + pm10, family = "poisson", data = dat)
summary(mod1)

# Model with only no2
mod2 <- glm(YhospResp ~ offset(log(EhospResp)) + income + crime + access + no2, family = "poisson", data = dat)
summary(mod2)

library(broom)
glance(mod1)
glance(mod2)
# Both no2 and pm10 appear significant in mod1. The AIC of mod1 (both pollutant variables) is lower
# suggesting mod1 is better

#Compute the relative change in risk for:
#a one unit increase in no2;
#one standard deviation increase in no2.
#Why might we prefer using the standard deviation?





# Producse histograms of the SMR and log(SMR)
hist(mod1$fitted.values / dat$EhospResp, main = "Histogram of SMR", xlab = "SMR")
hist(log(mod1$fitted.values / dat$EhospResp), main = "Histogram of log(SMR)", xlab = "log(SMR)")



# Predicting risk
linPred <- mod1$coeff[1] + 8*mod1$coeff[2] + 350*mod1$coeff[3] + 10*mod1$coeff[4] + 5*mod1$coeff[5]
unname(linPred)

# Risk is just the exponential of the linear predictor
risk <- exp(linPred)
unname(risk)

SE <- summary(mod1)$coefficients[1,2] +
  8 * summary(mod1)$coefficients[2,2] +
  350 * summary(mod1)$coefficients[3,2] +
  10 * summary(mod1)$coefficients[4,2] + 
  5 * summary(mod1)$coefficients[5,2]

unname(SE)
lower <- exp(linPred - 1.96 * SE)
upper <- exp(linPred + 1.96 * SE)
c(unname(lower), unname(upper))

# If a person had income score 8, crime score 350, etc we would expect them to have risk of being admitted to
# hospital for a respiratory disease in the CI



summary(dat$no2)

riskUpperQuart <- exp(mod1$coeff[1] + 8 * mod1$coeff[2] + 350 * mod1$coeff[3] + 10 * mod1$coeff[4] + 
                        14.044 * mod1$coeff[5])

unname(riskUpperQuart)

riskMax <- exp(mod1$coeff[1] + 8 * mod1$coeff[2] + 350 * mod1$coeff[3] + 10 * mod1$coeff[4] + 
                 38.291 * mod1$coeff[5])

unname(riskMax)

summary(dat)

