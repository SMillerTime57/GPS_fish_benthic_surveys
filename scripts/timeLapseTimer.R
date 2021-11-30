rm(list=ls())

library(tidyverse)
library(plotKML)
source('gps_functions.R')

watchDat <- read.csv("watchData.csv", header=TRUE)
rootPath <- ""

pathway <- paste(rootPath, as.character(watchDat$GoPro[1]),
                 "\\", as.character(watchDat$Parent[1]),
                 "\\", as.character(watchDat$Folder[1]), sep="")

files <- dir(path = pathway)

saved_metadata <- generateMetadata(files, watchDat$Year[1], watchDat$Month[1], watchDat$Day[1],
                                   watchDat$Hour[1], watchDat$Minute[1], watchDat$Second[1],
                                   as.character(watchDat$Folder[1]), as.character(watchDat$Track[1]), 
                                   trackPath = "")

for (i in 2:nrow(watchDat)) {
  
  pathway <- paste(rootPath, as.character(watchDat$GoPro[i]),
                   "\\", as.character(watchDat$Parent[i]),
                   "\\", as.character(watchDat$Folder[i]), sep="")
  
  files <- dir(path = pathway)
  
  saved_metadata <- rbind(saved_metadata, generateMetadata(files, watchDat$Year[i], watchDat$Month[i], watchDat$Day[i], watchDat$Hour[i], watchDat$Minute[i], watchDat$Second[i],
                                                           as.character(watchDat$Folder[i]), as.character(watchDat$Track[i])))
  
}

#write.csv(saved_metadata, file = "image_metadata.csv")