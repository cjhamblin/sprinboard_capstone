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
                       x[ ,c(5,6,26,27,28,29,55,56,57,8)]
                     })

#Rename Columns
for (i in 1:length(landingVar)) {
  colnames(landingVar[[i]]) <- c("Minutes","Seconds","Lat","LatFine","Lon","LonFine","WoW_comb","L_WoW","R_WoW", "V_Xl")
}

###Bounced Landing Detector###
#count number of binary flips # should be a function
bin_flip_countr <- function(x) {sum(diff(x) != 0)}

bounceC <- map(landingVar, ~bin_flip_countr(.$WoW_comb))
bounceL <- map(landingVar, ~bin_flip_countr(.$L_WoW))
bounceR <- map(landingVar, ~bin_flip_countr(.$R_WoW))

landingC <- do.call(rbind, bounceC)
landingL <- do.call(rbind, bounceL)
landingR <- do.call(rbind, bounceR)

##bounced landing data frame##
Landing_df <- data.frame(landingC, landingL, landingR)

##uncoordinated landing detector##
countL <- map(landingVar, ~sum(.$L_WoW))
countR <- map(landingVar, ~sum(.$R_WoW))

coordL <- do.call(rbind, countL)
coordR <- do.call(rbind, countR)
coord_df <- data.frame(coordL, coordR)
coord_df$seconds <- (abs(coordL-coordR)/16)

##Add Identifying Flight Data##
files_df <- as.data.frame(files)
QualData <- separate(files_df, files, into = c("pilot_id", "Flt", "Arpt", "Rnwy", "AC"), remove = TRUE, sep = "_")
AllPilotData <- separate(QualData, AC, into = c("Disp", "fil"), remove = TRUE, sep = "\\.")
PilotData <- AllPilotData[,-6]

#combine PilotData And bounced landing dataframes
Landing_Quality <- bind_cols(PilotData, Landing_df)

Landing_Quality$Bounce <- ifelse(Landing_Quality$landingL > 1 | Landing_Quality$landingR >1, "Bounced Landing!"," ")
Landing_Quality$Uncoord <- ifelse(coord_df$seconds >= 1, paste(coord_df$seconds),0)

Landing_Quality
