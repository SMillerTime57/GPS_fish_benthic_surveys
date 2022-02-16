rm(list=ls())

#Loads in necessary packages and our gps source script
library(tidyverse)
library(plotKML)
source('scripts\\gps_functions.R')

#Reads in the time data from the watch and sets the root path
watchDat <- read.csv("data\\watchData.csv", header=TRUE)
rootPath <- "" #Change this to reflect the full path to your "root" folder of your images - ex., "D:/GoPros/Organized/"
trackPath <- "" #Change this to reflect the full path to your folder containing all the GPS tracks
timelapseInterval <- (2/3) #Change this to reflect the true timelapse interval.  We use 2/3 because that is what the 0.5 interval on our GoPros actually was based on testing

#Uses root path and the information from watchDat to set the pathway directing to the images from the first survey
pathway <- paste(rootPath, as.character(watchDat$GoPro[1]),
                 "\\", as.character(watchDat$Parent[1]),
                 "\\", as.character(watchDat$Folder[1]), sep="")

#Creates a variable to contain all the filenames contained inside your pathway (essentially a vector of all the names of photographs in the destination folder)
files <- dir(path = pathway)

#Uses the generateMetadata() function to associate each image with a time and coordinates from the GPS track
saved_metadata <- generateMetadata(files, watchDat$Year[1], watchDat$Month[1], watchDat$Day[1],
                                   watchDat$Hour[1], watchDat$Minute[1], watchDat$Second[1],
                                   as.character(watchDat$Folder[1]), as.character(watchDat$Track[1]), 
                                   trackPath = trackPath, timeInt = timelapseInterval)

#Now loops through all rows of watchDat to do the same thing for every other row and combines the data
for (i in 2:nrow(watchDat)) {
  
  pathway <- paste(rootPath, as.character(watchDat$GoPro[i]),
                   "\\", as.character(watchDat$Parent[i]),
                   "\\", as.character(watchDat$Folder[i]), sep="")
  
  files <- dir(path = pathway)
  
  saved_metadata <- rbind(saved_metadata, generateMetadata(files, watchDat$Year[i], watchDat$Month[i], watchDat$Day[i], watchDat$Hour[i], watchDat$Minute[i], watchDat$Second[i],
                                                           as.character(watchDat$Folder[i]), as.character(watchDat$Track[i])))
  
}

#Finally, writes the csv containing all of the data
write.csv(saved_metadata, file = "image_metadata.csv")