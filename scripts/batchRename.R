rm(list=ls())

#Loads in the GPS functions used for this project
source('scripts\\gps_functions.R')

#Loads in our data on times
watchDat <- read.csv("data\\watchData.csv", header=TRUE) #Loads in your watchData file
rootPath <- "" #Put the entire pathway to your "root" folder here - ex, "D/Cameras/Organized"

#This loop calls the batchRename() function for each row of the watchDat csv file.  This goes to the folder in your computer
#that matches the information in the row and renames every file within it to include the folder name in front of it
#This is used to generate unique names for each photo in the dataset, because the default names include repeats
for (i in 2:nrow(watchDat)) {

batchRename(gopro = as.character(watchDat$GoPro[i]), parent = as.character(watchDat$Parent[i]), 
            folder = as.character(watchDat$Folder[i]), rootPath = rootPath)

}