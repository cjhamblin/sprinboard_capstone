
### ------------------------------------------------------------------
### ------------------------------------------------------------------
### --- Springboard, Foundations of Data Science
### --- 01/03/2017.
### --- CH_CapProj_ExtractFeatures_v1.R
### --- Autocorrelation based measures 
### --- for Chris Hamblin's Regression Models
### --- Goran S. Milovanovic, Foundations of Data Science Mentor
### ------------------------------------------------------------------

### --- clear/libraries
rm(list=ls())
library(dplyr)
library(stringr)
library(tseries)
library(forecast)
library(ggplot2)
library(tidyr)

### --- Working Directory
wDir1 <- 
  "/Users/e375086/Documents/Data Science/Springboard/Capstone/Project"
setwd(wDir1)
files <- list.files(wDir1)

### --- Target Features
features <- c("inertialVerticalSpd", 
              "verticalAccel",
              "bodyPitchRate",  
              "bodyRollRate",  
              "bodyYawRate", 
              "pitchAngle", 
              "rollAngle", 
              "calibratedAirspeed", 
              "gsDev", 
              "locDev", 
              "lRadAlt")

# store results:
afcMeasures <- matrix(0, length(files), length(features)*3+1)
colnames(afcMeasures) <- c(
  paste(rep(c("LegToZero", "RegCoeff", "R2"), length(features)), 
             unlist(lapply(features, function(x) {rep(x,3)})),
        sep = "_"),
  "completeSecons")
rownames(afcMeasures) <- files

for (i in 1:length(files)) {

  dataSet <- read.csv(files[i],
                      check.names = F,
                      header = T,
                      stringsAsFactors = F)

  # - select features
  variables <- str_extract(colnames(dataSet), "[[:alnum:]]+$")
  dataSet <- dataSet[, which(variables  %in% features)]
  colnames(dataSet) <- variables[which(variables  %in% features)]
  # - reduce data to complete seconds only
  dataLength <- dim(dataSet)[1]
  incompleteSecs <- dataLength %% 80 
  dataSet <- dataSet[-seq(1:incompleteSecs), ]
  # - how many seconds of this flight?
  completeSeconds <- dim(dataSet)[1]/80

  # - autocorrelation based measures
  results <- numeric()
  for (j in 1:length(features)) {
    
    feat <- ts(dataSet[, j], frequency = 80)
    # - acf
    autocor <- acf(feat,
                   lag.max = completeSeconds*80,
                   type = "correlation",
                   plot  = F)
    featFrame <- data.frame(acf = drop(autocor$acf),
                            lag = drop(autocor$lag))
    # - lag at first acorrel <= 0
    w <- which(featFrame$acf <= 0)[1]
    # - select until that lag:
    featFrame <- featFrame[1:w, ]
    lagToZero <- featFrame$lag[w]
    sfit <- lm(acf ~ lag, data = featFrame)
    coeff <- sfit$coefficients[2]
    R2 <- summary(sfit)$r.squared
    
    # - results:
    results <- append(results, c(lagToZero, coeff, R2))
    
  }
  
  # - write to: afcMeasures
  results <- append(results, completeSeconds)
  afcMeasures[i, ] <- results
  
}

# - change working directory and save outputSet as .csv:
wDir2 <- 
  '/Users/e375086/Documents/Data Science/Springboard/Capstone/output'
setwd(wDir2)
write.csv(afcMeasures, "afcFeatures.csv")

