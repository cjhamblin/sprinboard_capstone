
### ------------------------------------------------------------------
### ------------------------------------------------------------------
### --- Springboard, Foundations of Data Science
### --- 01/03/2017.
### --- CH_CapProj_ExtractFeatures_v1.R
### --- Variance Estimates for Chris Hamblin's Regression Models
### --- Goran S. Milovanovic, Foundations of Data Science Mentor
### ------------------------------------------------------------------

### --- clear/libraries
rm(list=ls())
library(dplyr)
library(stringr)
library(tseries)
library(forecast)

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

### --- Results Storage
# means: raw data
flightMeans <- matrix(0, ncol = 11, nrow = length(files))
rownames(flightMeans) <- files
colnames(flightMeans) <- paste0("rawMean_", features)
# variances: raw data
flightVars <- matrix(0, ncol = 11, nrow = length(files))
rownames(flightVars) <- files
colnames(flightVars) <- paste0("rawVar_", features)
# means: diff(raw data, differences = 2)
flightMeansDiff <- matrix(0, ncol = 11, nrow = length(files))
rownames(flightMeansDiff) <- files
colnames(flightMeansDiff) <- paste0("diffMean_", features)
# variances: diff(raw data, differences = 2)
flightVarsDiff <- matrix(0, ncol = 11, nrow = length(files))
rownames(flightVarsDiff) <- files
colnames(flightVarsDiff) <- paste0("diffVar_", features)
# stationarity tests: results for raw variables
testStationarity <- matrix('', ncol = 11, nrow = length(files))
rownames(testStationarity) <- files
colnames(testStationarity) <- paste0("rawADF_", features)
# stationarity tests: results for differencing, order = 2
testStationarityDiff <- matrix('', ncol = 11, nrow = length(files))
rownames(testStationarityDiff) <- files
colnames(testStationarityDiff) <- paste0("diffADF_", features)

### --- Process Source Files
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
  
  # - flightMeans
  flightMeans[i, ] <- apply(dataSet, 2, function(x) {
    mean(x)
  })
  
  # - flightVars
  flightVars[i, ] <- apply(dataSet, 2, function(x) {
    var(x)
  })
  
  # - flightMeansDiff
  flightMeansDiff[i, ] <- apply(dataSet, 2, function(x) {
    mean(diff(x, differences = 2))
  })
  
  # - flightVarsDiff
  flightVarsDiff[i, ] <- apply(dataSet, 2, function(x) {
    var(diff(x, differences = 2))
  })
  
  # - check for stationarity in raw data:
  testP <- apply(dataSet, 2, function(x) {
    # - significance of the Dickey-Fuller Test:
    a <- adf.test(x,
                  alternative = "stationary")$p.value
    # - output:
    # - "S" is for "Stationary", "NS" is for "Non-Stationary"
    if (a < .05) {return("S")} else {return("NS")}
    
  })
  testStationarity[i, ] <- testP
  
  # - check for stationarity following differencing of order 2:
  testP <- apply(dataSet, 2, function(x) {
    # - significance of the Dickey-Fuller Test:
    a <- adf.test(diff(x, differences = 2),
                  alternative = "stationary")$p.value
    # - output:
    # - "S" is for "Stationary", "NS" is for "Non-Stationary"
    if (a < .05) {return("S")} else {return("NS")}
    
  })
  testStationarityDiff[i, ] <- testP

}

# - N.B. Ignore warnings...

outputSet <- cbind(flightMeans,
                   flightVars,
                   flightMeansDiff,
                   flightVarsDiff,
                   testStationarity,
                   testStationarityDiff)

# - change working directory and save outputSet as .csv:
wDir2 <- 
  '/Users/e375086/Documents/Data Science/Springboard/Capstone/output'
setwd(wDir2)
write.csv(outputSet, "basicFeatures.csv")

