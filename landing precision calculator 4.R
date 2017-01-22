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
library(ggmap)
library(grid)

###Load Data Files###

files <- list.files(pattern = ".csv")
num = length(files)
myfiles <- lapply(files, function(x) {read.csv(x,
                                               header=T,
                                               check.names = F,
                                               stringsAsFactors = F)}
)

#Subset Variables
landingVar <- lapply(myfiles, 
                     function(x) {
                       x[ ,c(26,27,28,29,55,56,57,8,10,14,16,24,71,11,12,13,40)]
                     })

#Rename Columns
for (i in 1:length(landingVar)) {
  colnames(landingVar[[i]]) <- c("Lat","LatFine","Lon","LonFine","WoW_comb","L_WoW","R_WoW", "V_Xl","Heading","Pitch","Roll","Airspeed","SinkRate","PitchRate","RollRate","YawRate","Altitude")
}

##select landing variables at the landing point
outFrames <- lapply(landingVar, function(x) { x[which((x$WoW_comb == 0) & (x$L_WoW == 1) & (x$R_WoW == 1))[1], ] })

#convert landing variables to a dataframe
landing <- do.call(rbind, outFrames)

#add lat/lon corrections
landingLat <- mutate(landing, TDLat = landing$Lat + landing$LatFine)
landingLatLon <- mutate(landingLat, TDLon = landing$Lon + landing$LonFine)

#create new data frame with corrected Lat/Long points
TDPoint <- select(landingLatLon, TDLat:TDLon)

####Runway Landing Point Calculator####

#GPS position of KSFO 19L Runway Thresholds
Lat_Thresh <- 37.627813
Lat_Opp_Thresh <- 37.607394
Lon_Thresh <- -122.3668
Lon_Opp_Thresh <- -122.380338

deg2rad <- function(deg) {(deg * pi) / (180)}

angle = deg2rad(.5*(Lat_Opp_Thresh + Lat_Thresh))

#Length and width of runway
Xb = deg2rad((Lon_Opp_Thresh-Lon_Thresh)*cos(angle)*20924718.011811)
Yb = deg2rad((Lat_Opp_Thresh-Lat_Thresh)*20924718.011811)

#Distance from AC TD point to Runway Threshold
Xt = deg2rad((TDPoint$TDLon-Lon_Thresh)*cos(angle)*20924718.011811)
Yt = deg2rad((TDPoint$TDLat-Lat_Thresh)*20924718.011811)

#C values
C1 <- Xt*Xb+Yt*Yb
C2 <- sqrt(Xt^2+Yt^2)
C3 <- sqrt(Xb^2+Yb^2)

#Distance to Threshold
Dist2Thresh <- C1/C3

#Distance from Centerline
L = Xb*Yt - Xt*Yb  
Dist2CL <- 1/C3*sqrt(C2^2*C3^2-C1^2)


#Runway distance data frame
RunwayDist <- data.frame(Dist2Thresh, Dist2CL)
RunwayDist$Dist2CL_LR <- ifelse(L < 0, RunwayDist$Dist2CL*-1,RunwayDist$Dist2CL)


#Make a dataframe with files and distances
files_df <- as.data.frame(files)
AllLand <- bind_cols(files_df, RunwayDist)
AllLandingData <- separate(AllLand, files, into = c("pilot_id", "Flt", "Arpt", "Rnwy", "AC"), remove = TRUE, sep = "_")
AllPilotData <- separate(AllLandingData, AC, into = c("Disp", "fil"), remove = TRUE, sep = "\\.")
LandingSummary <- AllPilotData[,-6]

###precision classifier (1 = optimal, 2 = good, 3 = out of bounds) ###
LandingSummary$precision <-ifelse(LandingSummary$Dist2CL <= 25 & LandingSummary$Dist2Thresh > 200 & LandingSummary$Dist2Thresh <= 2200, "optimal",
                                    ifelse(LandingSummary$Dist2CL<= 50 & LandingSummary$Dist2Thresh <= 2700, "good","out of bounds"))

###Hard landing detector (1 = excellent, 2 = good, 3 = rough, 4 = hard)###
LandingSummary$Vert_Accel <- landing$V_Xl
LandingSummary$TDQual <- ifelse(landing$V_Xl <=1.3, "excellent", 
                              ifelse(landing$V_Xl <= 1.5, "good", 
                              ifelse(landing$V_Xl <= 1.7, "rough","hard")))
LandingSummary

##Plot Landings##
img <- readPNG("runway.png") 
runwayImg <- rasterGrob(img, interpolate=TRUE) 

LandingSummary1 <- LandingSummary[c(-60,-90,-91),]

landingplot <- ggplot(LandingSummary1[-60,], aes(x = Dist2Thresh, y = Dist2CL_LR, col = precision))+
  annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")),-Inf, Inf, -Inf)+
  geom_point(size = 2)+
  scale_color_manual(values = c("optimal" = "green1", "good" = "yellow", "out of bounds" = "red"))+
  scale_x_continuous(name="Distance to Threshold", limits=c(0,4500)) +
  scale_y_continuous(name="Distance to Centerline", limits=c(-75,75), breaks=c(-75,-50,-25,0,25,50,75))+
  coord_fixed(ratio = 10)
landingplot 


###Statistical Analysis###

