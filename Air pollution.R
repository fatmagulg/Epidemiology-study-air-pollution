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
# calculate SMR and add as a column to the dataframe
dat$SMR <- dat$YhospResp / dat$EhospResp




##### Correlation matrix#####
# Produce correlation matrix of all variables that can be included in the model
# Using log(SMR) bc Poisson regression with log link it being used
M <- cor(cbind(log(dat$SMR), dat[, 5 :12]))


# Plot it
install.packages("corrplot")
library(corrplot)
corrplot(M, method = 'shade', type = "full" 
         #,col = colorRampPalette(c("darkblue", "blue", "white", "pink", "red"))(10)
         )




##### Fit model #####
mod1 <- glm(YhospResp ~ offset(log(EhospResp)) + income + crime + access + no2, family = "poisson", data = dat)
summary(mod1)



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

