
### ---------------------------------------------------------- ###
### --- Springboard, Foundations of Data Science
### --- Chris Hamblin: Capstone Project
### --- CH_Models.R; inspect the data set, model, and evaluate
### ---------------------------------------------------------- ###

### --- clear all/load
rm(list = ls())
library(dplyr)
library(ggplot2)
library(ordinal) # - for Ordinal Logistic Regression
library(aod) # - wald.test() for Logistic Regression
library(caret) # - varImp() for variable importance in Logistic Regression
library(boot) # - cv.glm() for leave-one-out Cross Validation for Logistic Regression

# - working directory
wDir <- 
  "/Users/e375086/Documents/Data Science/Springboard/Capstone/project"
setwd(wDir)

# - load: data set
dataSet <- read.csv('All_Data.csv',
                    header = T,
                    check.names = F,
                    row.names = 1,
                    stringsAsFactors = F)

### --- inspect the data set

str(dataSet)

# - clean up: one flight has NAs
dataSet <- dataSet[complete.cases(dataSet), ]
dim(dataSet)

# - correct some feature names
colnames(dataSet) <- gsub("Leg", "Lag", colnames(dataSet), fixed = T)

# - how many pilots?
nPilots <- length(unique(dataSet$pilot_id))
nPilots

# - how many flights per pilot?
nPilotFlights <- table(dataSet$pilot_id)
nPilotFlights

# - how many runways?
nRunways <- length(unique(dataSet$Rnwy))
nRunways
table(dataSet$Rnwy)

# - how many airports?
nAirports <- length(unique(dataSet$Arpt))
nAirports

### --- look for outliers in continuous variables

# - Dist2Tresh has one HUGE outlier:
boxplot(dataSet$Dist2Thresh, main="Distance to Threshold", ylab="Distance (ft.)")
w <- which(dataSet$Dist2Thresh<0)
w


### --- NOTE C.H.: have 
dataSet <- dataSet[-w, ] # - eliminate from data

# - Dist2CL
boxplot(dataSet$Dist2CL, main="Distance to Center Line",ylab="Distance") # - Ok, we can live with these...

# - Dist2CL_LR
boxplot(dataSet$Dist2CL_LR) # - Ok, we can live with these...

# - Vert_Accel
boxplot(dataSet$Vert_Accel, main = "Vertical Acceleration", ylab="Acceleration (G)") # - Ok, we can live with these...

### ---------------------------------------------------------- ###
### --- PREPARE DATA
### ---------------------------------------------------------- ###

### --- Predictors (Independent Variables)

# - feature standard deviations:
# - only from stationary series
featureSDs <- dataSet %>% 
  select(starts_with('diffVar')) %>% 
  sqrt() # NOTE: variances to standard deviations to comply with previous studies

# - lag to zero:
# - how much time (secs) until autocorrelation <= 0?
featureLags <- dataSet %>% 
  select(starts_with('Lag'))

# - slopes:
# - the slope of regression line: Acf ~ Lag
featureSlopes <- dataSet %>%
  select(starts_with('RegCoeff'))

# - categorical/integer predictors:
featureCat <- dataSet %>% 
  select(pilot_id, Rnwy, Stable_approach, axis_count)

featureCat$pilot_id <- factor(featureCat$pilot_id) # 307 - arbitrary baseline pilot_id

featureCat$Rnwy <- factor(featureCat$Rnwy) # 19L - arbitrary baseline runway 

featureCat$Stable_approach <- featureCat$Stable_approach %>% 
  recode(No = 0, Yes = 1)

featureCat$Stable_approach <- factor(featureCat$Stable_approach) # 0 == unstable -> baseline

### --- Criteria (Dependent Variables)

# - categorical criteria
criteriaCat <- dataSet %>% 
  select(precision, TDQual, Bounce, Uncoord)

criteriaCat$precision <- criteriaCat$precision %>% 
  recode(`out of bounds` = 0, good = 1, optimal = 2)
summary(criteriaCat$precision)

criteriaCat$precision <- factor(criteriaCat$precision) # 0 == `out of bounds` -> baseline

criteriaCat$TDQual <- criteriaCat$TDQual %>% 
  recode(hard = 0, rough = 1, good = 2, excellent = 3)
summary(criteriaCat$TDQual)

criteriaCat$TDQual <- factor(criteriaCat$TDQual) # 0 == hard -> baseline

criteriaCat$Bounce <- criteriaCat$Bounce %>% 
  recode(` ` = 1, `Bounced Landing!` = 0)

criteriaCat$Bounce <- factor(criteriaCat$Bounce) # 0 == bounced landing -> baseline
# DV Uncoord - time difference between left and right WOW sensors 
# (0 = coordinated landing, >1 = uncoordinated landing)
summary(criteriaCat$Bounce)

criteriaCat$Uncoord <- criteriaCat$Uncoord %>% 
  recode(`0` = 4, `1` = 3, `2` = 2, `3` = 1, `4` = 0) %>%
  factor() # 0 == very uncoordinated -> baseline
summary(criteriaCat$Uncoord)

# continuous criteria
criteriaCont <- dataSet %>%
  select(Dist2Thresh, Dist2CL, Dist2CL_LR, Vert_Accel)

###------------------------------------------------------------###
###Descriptive Statistics
###------------------------------------------------------------###

summary(dataSet$Dist2Thresh)
summary(dataSet$Dist2CL)
table(dataSet$precision)

table(dataSet$TDQual)

table(dataSet$Bounce)

table(dataSet$Stable_approach)

### ---------------------------------------------------------- ###
### --- MODELS
### ---------------------------------------------------------- ###

### ---------------------------------------------------------- ###
### --- Model 1A:
### --- DV: Dist2Thresh
### --- Predictors: ACF lags to zero + all categorical/integer
### --- Model: Multiple Linear Regression
### ---------------------------------------------------------- ###

# - select variables --> modelSet
modelSet <- cbind(featureCat,
                  featureLags,
                  Dist2Thresh = criteriaCont$Dist2Thresh)

# - model
mFit <- lm(Dist2Thresh ~ ., 
           data = modelSet)

# - inspect model
summary(mFit)

# - diagnostics: Heteroskedasticity
dgnFrame <- data.frame(
  Residuals = mFit$residuals,
  Predicted = mFit$fitted.values)
ggplot(dgnFrame, aes(x = Predicted, y = Residuals)) +
  geom_smooth(method = 'lm', size = .25) + 
  geom_point(size = 1.2, color = "black") +
  geom_point(size = 1, color = "white") + 
  ggtitle('Inspect Heteroskedasticity') +
  theme(plot.title = element_text(size = 10))

# - qqplot of standardized residuals
qqnorm(scale(mFit$residuals, center = T, scale = T))
qqline(scale(mFit$residuals, center = T, scale = T), col = "red")

# this will explained later (c.f. Model 1D)
pressModel_1A <- sum(rstandard(mFit, type="pred")^2)

### ---------------------------------------------------------- ###
### --- Model 1B:
### --- DV: Dist2Thresh
### --- Predictors: ACF ~ lags Regression Slope + all categorical/integer
### --- Model: Multiple Linear Regression
### ---------------------------------------------------------- ###

# - select variables --> modelSet
modelSet <- cbind(featureCat,
                  featureSlopes,
                  Dist2Thresh = criteriaCont$Dist2Thresh)

# - model
mFit <- lm(Dist2Thresh ~ ., 
           data = modelSet)

# - inspect model
summary(mFit)

# - diagnostics: Heteroskedasticity
dgnFrame <- data.frame(
  Residuals = mFit$residuals,
  Predicted = mFit$fitted.values)
ggplot(dgnFrame, aes(x = Predicted, y = Residuals)) +
  geom_smooth(method = 'lm', size = .25) + 
  geom_point(size = 1.2, color = "black") +
  geom_point(size = 1, color = "white") + 
  ggtitle('Inspect Heteroskedasticity') +
  theme(plot.title = element_text(size = 10))

# - qqplot of standardized residuals
qqnorm(scale(mFit$residuals, center = T, scale = T))
qqline(scale(mFit$residuals, center = T, scale = T), col = "red")

# this will explained later (c.f. Model 1D)
pressModel_1B <- sum(rstandard(mFit, type="pred")^2)

### ---------------------------------------------------------- ###
### --- Model 1C:
### --- DV: Dist2Thresh
### --- Predictors: Acf lags + Acf slopes + all categorical/integer
### --- Model: Multiple Linear Regression
### ---------------------------------------------------------- ###

# - select variables --> modelSet
modelSet <- cbind(featureCat,
                  featureLags,
                  featureSlopes,
                  Dist2Thresh = criteriaCont$Dist2Thresh)

# - model
mFit <- lm(Dist2Thresh ~ ., 
           data = modelSet)

# - inspect model
summary(mFit)

# - diagnostics: Heteroskedasticity
dgnFrame <- data.frame(
  Residuals = mFit$residuals,
  Predicted = mFit$fitted.values)
ggplot(dgnFrame, aes(x = Predicted, y = Residuals)) +
  geom_smooth(method = 'lm', size = .25) + 
  geom_point(size = 1.2, color = "black") +
  geom_point(size = 1, color = "white") + 
  ggtitle('Inspect Heteroskedasticity') +
  theme(plot.title = element_text(size = 10))

# - qqplot of standardized residuals
qqnorm(scale(mFit$residuals, center = T, scale = T))
qqline(scale(mFit$residuals, center = T, scale = T), col = "red")

# this will explained later (c.f. Model 1D)
pressModel_1C <- sum(rstandard(mFit, type="pred")^2)

### ---------------------------------------------------------- ###
### --- Model 1D:
### --- DV: Dist2Thresh
### --- Predictors: Acf lags + Acf slopes + FeatureSDs + all categorical/integer
### --- Model: Multiple Linear Regression
### ---------------------------------------------------------- ###

# - select variables --> modelSet
modelSet <- cbind(featureCat,
                  featureLags,
                  featureSDs,
                  featureSlopes,
                  Dist2Thresh = criteriaCont$Dist2Thresh)

# - model
mFit <- lm(Dist2Thresh ~ ., 
           data = modelSet)

# - inspect model
summary(mFit)

# - diagnostics: Heteroskedasticity
dgnFrame <- data.frame(
  Data = modelSet$Dist2Thresh,
  Residuals = mFit$residuals,
  Predicted = mFit$fitted.values)
ggplot(dgnFrame, aes(x = Predicted, y = Residuals)) +
  geom_smooth(method = 'lm', size = .25) + 
  geom_point(size = 1.2, color = "black") +
  geom_point(size = 1, color = "white") + 
  ggtitle('Inspect Heteroskedasticity') +
  theme(plot.title = element_text(size = 10))

# - qqplot of standardized residuals
qqnorm(scale(mFit$residuals, center = T, scale = T))
qqline(scale(mFit$residuals, center = T, scale = T), col = "red")

# - plot predictions
# - NOTE: C.H. - it looks nice, but does it overfit?
ggplot(dgnFrame, aes(x = Predicted, y = Data)) +
  geom_smooth(method = 'lm', size = .25, color = 'red') + 
  geom_point(size = 1.2, color = "black") +
  geom_point(size = 1, color = "white") + 
  ggtitle('Model Predictions') +
  theme(plot.title = element_text(size = 10))

# - PRESS statistic
# - NOTE: C.H. take a look at:
# - https://www.r-bloggers.com/estimating-generalization-error-with-the-press-statistic/
# - The explanation of the code that I will use here is found in the {stats} documentation:
# - https://stat.ethz.ch/R-manual/R-devel/library/stats/html/influence.measures.html:
# - For linear models, rstandard(*, type = "predictive") provides leave-one-out cross validation 
# - or do: ?rstandard
pressModel_1D <- sum(rstandard(mFit, type="pred")^2)

### --- Now compare PRESS statistics for all Model_1 variants:
pressModel_1A
pressModel_1B
pressModel_1C
pressModel_1D

### ---------------------------------------------------------- ###
### --- Model 2A:
### --- DV: Dist2CL
### --- Predictors: ACF lags to zero + all categorical/integer
### --- Model: Multiple Linear Regression
### ---------------------------------------------------------- ###

# - select variables --> modelSet
modelSet <- cbind(featureCat,
                  featureLags,
                  Dist2CL = criteriaCont$Dist2CL)

# - model
mFit <- lm(Dist2CL ~ ., 
           data = modelSet)

# - inspect model
summary(mFit)

# - diagnostics: Heteroskedasticity
dgnFrame <- data.frame(
  Residuals = mFit$residuals,
  Predicted = mFit$fitted.values)
ggplot(dgnFrame, aes(x = Predicted, y = Residuals)) +
  geom_smooth(method = 'lm', size = .25) + 
  geom_point(size = 1.2, color = "black") +
  geom_point(size = 1, color = "white") + 
  ggtitle('Inspect Heteroskedasticity') +
  theme(plot.title = element_text(size = 10))

# - qqplot of standardized residuals
qqnorm(scale(mFit$residuals, center = T, scale = T))
qqline(scale(mFit$residuals, center = T, scale = T), col = "red")

# - PRESS
pressModel_2A <- sum(rstandard(mFit, type="pred")^2)

### ---------------------------------------------------------- ###
### --- Model 2B:
### --- DV: Dist2CL
### --- Predictors: ACF ~ lags Regression Slope + all categorical/integer
### --- Model: Multiple Linear Regression
### ---------------------------------------------------------- ###

# - select variables --> modelSet
modelSet <- cbind(featureCat,
                  featureSlopes,
                  Dist2CL = criteriaCont$Dist2CL)

# - model
mFit <- lm(Dist2CL ~ ., 
           data = modelSet)

# - inspect model
summary(mFit)

# - diagnostics: Heteroskedasticity
dgnFrame <- data.frame(
  Residuals = mFit$residuals,
  Predicted = mFit$fitted.values)
ggplot(dgnFrame, aes(x = Predicted, y = Residuals)) +
  geom_smooth(method = 'lm', size = .25) + 
  geom_point(size = 1.2, color = "black") +
  geom_point(size = 1, color = "white") + 
  ggtitle('Inspect Heteroskedasticity') +
  theme(plot.title = element_text(size = 10))

# - qqplot of standardized residuals
qqnorm(scale(mFit$residuals, center = T, scale = T))
qqline(scale(mFit$residuals, center = T, scale = T), col = "red")

# - PRESS
pressModel_2B <- sum(rstandard(mFit, type="pred")^2)

### ---------------------------------------------------------- ###
### --- Model 2C:
### --- DV: Dist2CL
### --- Predictors: all continuous + all categorical/integer
### --- Model: Multiple Linear Regression
### ---------------------------------------------------------- ###

# - select variables --> modelSet
modelSet <- cbind(featureCat,
                  featureLags,
                  featureSlopes,
                  Dist2CL = criteriaCont$Dist2CL)

# - model
mFit <- lm(Dist2CL ~ ., 
           data = modelSet)

# - inspect model
summary(mFit)

# - diagnostics: Heteroskedasticity
dgnFrame <- data.frame(
  Residuals = mFit$residuals,
  Predicted = mFit$fitted.values)
ggplot(dgnFrame, aes(x = Predicted, y = Residuals)) +
  geom_smooth(method = 'lm', size = .25) + 
  geom_point(size = 1.2, color = "black") +
  geom_point(size = 1, color = "white") + 
  ggtitle('Inspect Heteroskedasticity') +
  theme(plot.title = element_text(size = 10))

# - qqplot of standardized residuals
qqnorm(scale(mFit$residuals, center = T, scale = T))
qqline(scale(mFit$residuals, center = T, scale = T), col = "red")

# - PRESS
pressModel_2C <- sum(rstandard(mFit, type="pred")^2)

### --- Now compare PRESS statistics for all Model_2 variants:
pressModel_2A
pressModel_2B
pressModel_2C

### ---------------------------------------------------------- ###
### --- Model 3A:
### --- DV: Dist2CL_LR
### --- Predictors: all continuous + all categorical/integer
### --- Model: Multiple Linear Regression
### ---------------------------------------------------------- ###

# - select variables --> modelSet
modelSet <- cbind(featureCat,
                  featureLags,
                  featureSlopes,
                  Dist2CL = abs(criteriaCont$Dist2CL_LR)) # NOTE: abs()

# - model
mFit <- lm(Dist2CL ~ ., 
           data = modelSet)

# - inspect model
summary(mFit)

# - diagnostics: Heteroskedasticity
dgnFrame <- data.frame(
  Residuals = mFit$residuals,
  Predicted = mFit$fitted.values)
ggplot(dgnFrame, aes(x = Predicted, y = Residuals)) +
  geom_smooth(method = 'lm', size = .25) + 
  geom_point(size = 1.2, color = "black") +
  geom_point(size = 1, color = "white") + 
  ggtitle('Inspect Heteroskedasticity') +
  theme(plot.title = element_text(size = 10))

# - qqplot of standardized residuals
qqnorm(scale(mFit$residuals, center = T, scale = T))
qqline(scale(mFit$residuals, center = T, scale = T), col = "red")

# - PRESS
pressModel_3A <- sum(rstandard(mFit, type="pred")^2)

### ---------------------------------------------------------- ###
### --- Model 4A:
### --- DV: Bounced Landing
### --- Predictors: categorical/integer
### --- Model: Binary Logistic Regression
### ---------------------------------------------------------- ###

# - NOTE: C.H. take a look at the following discussion:
# - URL: https://stat.ethz.ch/pipermail/r-help/2008-March/156868.html
# - learn about perfect separation in Binomial Logistic Regression
# - NOTE: Very useful for Logistic Regression Diagnostics:
# - URL: https://www.r-bloggers.com/evaluating-logistic-regression-models/

# - Predicting bounced landings will work with categorical features:

# - select variables --> modelSet
modelSet <- cbind(featureCat,
                  Bounced = criteriaCat$Bounce)

# - model
mFit <- glm(Bounced ~ .,
            family = binomial(link = "logit"),
            data = modelSet)

# - inspect model
summary(mFit)

# - improvement from the saturated model significant or not:
p = pchisq(mFit$null.deviance - mFit$deviance,
           df = mFit$df.null - mFit$df.residual,
           lower.tail = F)
p

# test the overall effect of pilot_id
wald.test(b = coef(mFit), 
          Sigma = vcov(mFit), 
          Terms = 2:6) # NOTE: terms are ordered as they are in the mFit model!

# - NOTE: the coefficients are provided on a log-odds scale
# - to transform them to odds:
exp(mFit$coefficients)

# - inspect residuals
stdRes <- rstandard(mFit) # - standardized residuals
summary(stdRes)
# - Only 5% is allowed to lie outside +/-1.96:
sum(stdRes < -1.96 | stdRes > 1.96)/length(stdRes)

# - variable importance w. varImp() from {caret}
# - NOTE: this is the absolute value of the Wald statistic for each model parameter:
mFitCoeff <- as.data.frame(mFit$coefficients)
varImp(mFit)$Overall == abs(mFitCoeff$`z value`[-1])

# - Leave-One-Out Cross-Validation
# - NOTE: The default cost function is the average squared error function
# - ?cv.glm
# - URL: https://stat.ethz.ch/R-manual/R-devel/library/boot/html/cv.glm.html
# - Since the response is a binary variable an appropriate cost function is:
# - cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
# - Take a look here:
# - URL: http://stats.stackexchange.com/questions/45569/what-is-the-cost-function-in-cv-glm-in-rs-boot-package
# - to understand why this cost function is used for binary responses:
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
cvmFit <- cv.glm(modelSet, mFit, cost)
cvmFit$delta[1] # - cross-validation estimate of prediction error

### ---------------------------------------------------------- ###
### --- Model 4B:
### --- DV: Bounced Landing
### --- Predictors: Acf Lags
### --- Model: Binary Logistic Regression
### ---------------------------------------------------------- ###

# - However, predicting bounced landings with continuous features...
# - Acf Lags, for example:

# - select variables --> modelSet
modelSet <- cbind(featureLags,
                  Bounced = criteriaCat$Bounce)

# - model
mFit <- glm(Bounced ~ .,
            family = binomial(link = "logit"),
            data = modelSet) # - perfect separation occurs (!!!)

# - inspect model
summary(mFit)

# - improvement from the saturated model significant or not:
p = pchisq(mFit$null.deviance - mFit$deviance,
           df = mFit$df.null - mFit$df.residual,
           lower.tail = F)
p

# - coefficients
exp(mFit$coefficients)

# - inspect residuals
stdRes <- rstandard(mFit) # - standardized residuals
summary(stdRes)
# - Only 5% is allowed to lie outside +/-1.96:
sum(stdRes < -1.96 | stdRes > 1.96)/length(stdRes)

# - variable importance
varImp(mFit)

# - Leave-One-Out Cross-Validation
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
cvmFit <- cv.glm(modelSet, mFit, cost)
cvmFit$delta[1] # - cross-validation estimate of prediction error

### ---------------------------------------------------------- ###
### --- Model 4C:
### --- DV: Bounced Landing
### --- Predictors: Standard Deviations
### --- Model: Binary Logistic Regression
### ---------------------------------------------------------- ###

# - Predicting bounced landings with continuous features
# - it works with StdDevs:

# - select variables --> modelSet
modelSet <- cbind(featureSDs,
                  Bounced = criteriaCat$Bounce)

# - model
mFit <- glm(Bounced ~ .,
            family = binomial(link = "logit"),
            data = modelSet)

# - inspect model
summary(mFit)

# - improvement from the saturated model significant or not:
p = pchisq(mFit$null.deviance - mFit$deviance,
           df = mFit$df.null - mFit$df.residual,
           lower.tail = F)
p

# - coefficients
exp(mFit$coefficients)

# - inspect residuals
stdRes <- rstandard(mFit) # - standardized residuals
summary(stdRes)
# - Only 5% is allowed to lie outside +/-1.96:
sum(stdRes < -1.96 | stdRes > 1.96)/length(stdRes)

# - variable importance
varImp(mFit)

# - Leave-One-Out Cross-Validation
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
cvmFit <- cv.glm(modelSet, mFit, cost)
cvmFit$delta[1] # - cross-validation estimate of prediction error

### ---------------------------------------------------------- ###
### --- Model 4D:
### --- DV: Bounced Landing
### --- Predictors: Acf Regression Slopes
### --- Model: Binary Logistic Regression
### ---------------------------------------------------------- ###

# - Predicting bounced landings with continuous features
# - it works with Acf Slopes too:

# - select variables --> modelSet
modelSet <- cbind(featureSlopes,
                  Bounced = criteriaCat$Bounce)

# - model
mFit <- glm(Bounced ~ .,
            family = binomial(link = "logit"),
            data = modelSet)

# - inspect model
summary(mFit)

# - improvement from the saturated model significant or not:
p = pchisq(mFit$null.deviance - mFit$deviance,
           df = mFit$df.null - mFit$df.residual,
           lower.tail = F)
p

# - coefficients
exp(mFit$coefficients)

# - inspect residuals
stdRes <- rstandard(mFit) # - standardized residuals
summary(stdRes)
# - Only 5% is allowed to lie outside +/-1.96:
sum(stdRes < -1.96 | stdRes > 1.96)/length(stdRes)

# - variable importance
varImp(mFit)

# - Leave-One-Out Cross-Validation
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
cvmFit <- cv.glm(modelSet, mFit, cost)
cvmFit$delta[1] # - cross-validation estimate of prediction error

### ---------------------------------------------------------- ###
### --- Model 5A:
### --- DV: Precision
### --- Predictors: Acf Lags
### --- Model: Ordinal Logistic Regression
### ---------------------------------------------------------- ###

# - NOTE: Tutorial on the clm() function
# - for ordere logistic regression w. {ordinal} in R
# - URL: https://cran.r-project.org/web/packages/ordinal/vignettes/clm_tutorial.pdf

# - select variables --> modelSet
modelSet <- cbind(featureLags,
                  Precision = criteriaCat$precision)

# - model
mFit <- clm(Precision ~ .,
            link = "logit",
            data = modelSet)

mFit5a <- clm(criteriaCat$precision ~ LagToZero_inertialVerticalSpd + LagToZero_verticalAccel + LagToZero_bodyPitchRate, link = "logit", data = dataSet)
# - inspect model
summary(mFit)
summary(mFit5a)
# - coefficients
mFit$coefficients
mFit5a$coefficients
# - fitted probabilities:
# - the fitted probability is the probability that the observation 
# - falls in the response category that it did. 
mFit$fitted.values
mFit5a$fitted.values
# - predicted class membership:
predict(mFit, type = "class")
predict(mFit5a, type = "class")

# - compare:
modelSet$Precision
# - e.g. Hit rate (Accuracy of Classification)
countHits <- sum(predict(mFit, type = "class")$fit == modelSet$Precision)
dataPoints <- length(modelSet$Precision)
hitRate <- countHits/dataPoints
hitRate
  
### ---------------------------------------------------------- ###
### --- Model 5B:
### --- DV: Precision
### --- Predictors: Acf Regression Slopes
### --- Model: Ordinal Logistic Regression
### ---------------------------------------------------------- ###

# - select variables --> modelSet
modelSet <- cbind(featureSlopes,
                  Precision = criteriaCat$precision)

# - model
mFit <- clm(Precision ~ .,
            link = "logit",
            data = modelSet)

# - inspect model
summary(mFit)

# - coefficients
mFit$coefficients

# - fitted probabilities:
mFit$fitted.values

# - predicted class membership:
predict(mFit, type = "class")
# - Hit rate (Accuracy of Classification)
countHits <- sum(predict(mFit, type = "class")$fit == modelSet$Precision)
dataPoints <- length(modelSet$Precision)
hitRate <- countHits/dataPoints
hitRate

### ---------------------------------------------------------- ###
### --- Model 5C:
### --- DV: Precision
### --- Predictors: Feature Std.Deviations
### --- Model: Ordinal Logistic Regression
### ---------------------------------------------------------- ###

# - select variables --> modelSet
modelSet <- cbind(featureSDs,
                  Precision = criteriaCat$precision)

# - model
mFit <- clm(Precision ~ .,
            link = "logit",
            data = modelSet)

# - inspect model
summary(mFit)

# - coefficients
mFit$coefficients

# - fitted probabilities:
mFit$fitted.values

# - predicted class membership:
predict(mFit, type = "class")
# - Hit rate (Accuracy of Classification)
countHits <- sum(predict(mFit, type = "class")$fit == modelSet$Precision)
dataPoints <- length(modelSet$Precision)
hitRate <- countHits/dataPoints
hitRate

### ---------------------------------------------------------- ###
### --- Model 5D:
### --- DV: Precision
### --- Predictors: Acf Regression Slopes + Feature Std.Deviations
### --- Model: Ordinal Logistic Regression
### ---------------------------------------------------------- ###

# - select variables --> modelSet
modelSet <- cbind(featureSlopes,
                  featureSDs,
                  Precision = criteriaCat$precision)

# - model
mFit <- clm(Precision ~ .,
            data = modelSet)

# - inspect model
summary(mFit)

# - coefficients
mFit$coefficients

# - fitted probabilities:
mFit$fitted.values

# - predicted class membership:
predict(mFit, type = "class")
# - Hit rate (Accuracy of Classification)
countHits <- sum(predict(mFit, type = "class")$fit == modelSet$Precision)
dataPoints <- length(modelSet$Precision)
hitRate <- countHits/dataPoints
hitRate

### ---------------------------------------------------------- ###
### --- NOTES
### ---------------------------------------------------------- ###

### --- Features extracted by Chris:
### --- DV == "Dependent Variable" (Criteria)
### --- IV == "Independent Variable" (Predictors)

# DV Dist2Thresh - Distance the the runway threshold
# DV Dist2CL - Absolute distance the runway centerline
# DV Dist2CL_LR - Distance to CL (negative numbers are left of centerline, postive numbers are right of centerline)
# DV precision - Categorization of precession (optimal, good, out of bounds)
# DV Vert_Accel- vertical acceleration at landing
# DV TDQual - Ordinal measure of vertical acceleration (excellent, good, rough, hard)
# DV landingC - count weight on wheel sensor changes (>1 = bounded landing)
# DV landingL - count weight on wheel sensor changes (>1 = bounded landing)
# DV landingR - count weight on wheel sensor changes (>1 = bounded landing)
# DV Bounce - binomial categorization of bounced/not bounced landing
# DV Uncoord - time difference between left and right WOW sensors (0 = coordinated landing, >1 = uncoordinated landing)

# IV heading - categorization of heading stability
# IV pitch_df - categorization of pitch stability
# IV roll_df - categorization of roll stability
# IV Airspeed - categorization of heading stability
# IV sink - categorization of sink rate stability

# IV Stable_approach - overall categorization of the approach (if any of the individual axes were unstable)
# IV axis_count - # of unstable axes

