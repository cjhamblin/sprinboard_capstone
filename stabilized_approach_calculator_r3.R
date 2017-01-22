# clear all
rm(list=ls())

# load
# NOTE: make sure to change to your working path:
wDir <- ''
setwd("~/Data Science/Springboard/Capstone/Project")

###Load Libraries###
library(dplyr)
library(ggplot2)
library(tidyr)
library(png)
library(purrr)

###Load Data Files###

files <- list.files(pattern = ".csv")
num = length(files)
myfiles <- lapply(files, function(x) {read.csv(x,
                                               header=T,
                                               check.names = F,
                                               stringsAsFactors = F)})

#Subset Variables
landingVar <- lapply(myfiles, 
                     function(x) {
                       x[ ,c(10,14,16,24,71,11,12,13,40, 8)]
                     })

#Rename Columns
for (i in 1:length(landingVar)) {
  colnames(landingVar[[i]]) <- c("Heading","Pitch","Roll","Airspeed","SinkRate","PitchRate","RollRate","YawRate","Altitude","V_XL")
}

##subset data to stop recording at 50ft above the runway
outFrames <- map(landingVar, function(x) { x[which(x$Altitude >= 50),] })

#subset data between 1000-500 ft altitude
appr_1 <- map(landingVar, function(x) {x[which((x$Altitude <= 1000) & (x$Altitude >500)), ]})

#subset data between 500-100 ft altitude
appr_2 <- map(landingVar, function(x) {x[which((x$Altitude <= 500) & (x$Altitude >100)), ]})


##Identifying Flight Data Frame##
files_df <- as.data.frame(files)
QualData <- separate(files_df, files, into = c("pilot_id", "Flt", "Arpt", "Rnwy", "AC"), remove = TRUE, sep = "_")
AllPilotData <- separate(QualData, AC, into = c("Disp", "fil"), remove = TRUE, sep = "\\.")
approach_stability_df <- AllPilotData[,-6]
approach_data <- AllPilotData[,c(-3,-4,-6)]

#descripitive statistics for each approach variable##
appr <- map(landingVar, summary)

#heading stability (wind!!)
mxmn <- function (x) { max(x)-min(x)}
appr_Heading <- map(outFrames, ~ mxmn(.$Heading))
heading_df <- do.call(rbind, appr_Heading)
approach_stability_df$heading <- ifelse(appr_Heading <= 10, "Stable", "Unstable")

#pitch stability >=-5 <=3
maxpitch <- map(outFrames, ~ max(.$Pitch))
minpitch <- map(outFrames, ~ min(.$Pitch))
appr_maxpitch <- do.call(rbind, maxpitch)
appr_minpitch <- do.call(rbind, minpitch)
pitch_df <- data.frame(appr_maxpitch, appr_minpitch)
approach_stability_df$pitch_df <- ifelse(appr_minpitch >= -3 & appr_maxpitch <= 3, "Stable", "Unstable")

#roll stability +/-5
maxroll <- map(outFrames, ~ max(.$RollRate))
minroll <- map(outFrames, ~ min(.$RollRate))
appr_maxroll <- do.call(rbind, maxroll)
appr_minroll <- do.call(rbind, minroll)
roll_df <- data.frame(appr_maxroll, appr_minroll)
approach_stability_df$roll_df <- ifelse (appr_minroll >= -5 & appr_maxroll <= 5, "Stable", "Unstable")

#airspeed stability >= 126 <= 146
maxcas <- map(outFrames, ~ max(.$Airspeed))
mincas <- map(outFrames, ~ min(.$Airspeed))
appr_maxcas <- do.call(rbind, maxcas)
appr_mincas <- do.call(rbind, mincas)
cas_df <- data.frame(appr_maxcas, appr_mincas)
approach_stability_df$Airspeed <- ifelse (appr_mincas >= 126 & appr_maxcas <= 146, "Stable", "Unstable")

#sinkrate stability
appr_sink <- map(outFrames, ~ min(.$SinkRate))
sink_df <- do.call(rbind, appr_sink)
approach_stability_df$sink <- ifelse(appr_sink >= -1000, "Stable","Unstable")

##Conditional Approach Stability Data Frame##
approach_stability_df$stable_approach <- ifelse (approach_stability_df$heading == "Unstable" | approach_stability_df$pitch_df == "Unstable" | approach_stability_df$roll_df == "Unstable" | approach_stability_df$Airspeed == "Unstable" | approach_stability_df$sink == "Unstable", "No", "Yes")
approach_stability_df$axis_count <- rowSums(approach_stability_df == "Unstable")
approach_stability_df

##Continuous Approach Stability Data Frame##
approach_stability_data_df <- data.frame(approach_data, heading_df, pitch_df, roll_df, cas_df, sink_df)
approach_stability_data_df

###Calculate Variance of each performance variable###
CAS_Var <- map(outFrames, ~ var(.$Airspeed))
sink_Var <- map(outFrames, ~ var(.$SinkRate))
vxl_Var <- map(outFrames, ~ var(.$V_XL))
pitch_Var <- map(outFrames, ~ var(.$PitchRate))
roll_Var <- map(outFrames, ~ var(.$RollRate))
Yaw_Var <- map(outFrames, ~ var(.$YawRate))
alt_Var <- map(outFrames, ~ var(.$Altitude))

appr_casvar <- do.call(rbind, CAS_Var)
appr_sinkvar <- do.call(rbind, sink_Var)
appr_vxlvar <- do.call(rbind, vxl_Var)
appr_pitchvar <- do.call(rbind, pitch_Var)
appr_rollvar <- do.call(rbind, roll_Var)
appr_yawvar <- do.call(rbind, Yaw_Var)
appr_altvar <- do.call(rbind, alt_Var)

Var_df <- data.frame(approach_data, appr_casvar, appr_sinkvar, appr_vxlvar, appr_pitchvar, appr_rollvar, appr_yawvar, appr_altvar)

###Analysis: Approach data predicting runway precision (load landing precision calculator)###
All_Approach_Data <- right_join(approach_stability_data_df, Var_df)
Appr2Land_df <- right_join(All_Approach_Data, LandingSummary)

fit1 <- lm(formula = Dist2Thresh ~ appr_casvar + appr_sinkvar + appr_vxlvar + appr_pitchvar + appr_altvar + appr_rollvar + appr_yawvar, data = Appr2Land_df)
fit1a <- lm(formula = Dist2Thresh ~ appr_casvar + appr_sinkvar + appr_vxlvar + appr_altvar, data = Appr2Land_df)
fit1b <- lm(formula = Dist2Thresh ~ appr_casvar + appr_sinkvar + appr_vxlvar, data = Appr2Land_df)
fit1c <- fit1 <- lm(formula = log(Dist2Thresh) ~ appr_casvar + appr_sinkvar + appr_vxlvar + appr_pitchvar + appr_altvar + appr_rollvar + appr_yawvar, data = Appr2Land_df)

fit2 <- lm(formula = Dist2CL ~ appr_casvar + appr_sinkvar + appr_vxlvar + appr_pitchvar + appr_altvar + appr_rollvar + appr_yawvar, data = Appr2Land_df)
fit2a <- lm(formula = Dist2CL ~ appr_rollvar + appr_yawvar, data = Appr2Land_df)

###Analysis: logistic regressions###
fit3 <- glm(formula = precision ~ appr_casvar + appr_sinkvar + appr_vxlvar + appr_pitchvar + appr_altvar + appr_rollvar + appr_yawvar, data = Appr2Land_df)
fit3a <- multinom(precision ~ appr_casvar + appr_sinkvar + appr_vxlvar + appr_pitchvar + appr_altvar + appr_rollvar + appr_yawvar, data = Appr2Land_df)
