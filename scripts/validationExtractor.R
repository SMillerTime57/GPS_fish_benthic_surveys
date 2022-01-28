rm(list=ls())

source('scripts\\gps_functions.R')
library(tidyverse)

image_metadata <- read.csv("image_metadata.csv", header=TRUE, stringsAsFactors = FALSE)
image_metadata$Time <- strptime(image_metadata$Time, "%Y-%m-%d %H:%M:%S")
image_metadata$observer <- substr(image_metadata$Filename, 1, 2)

fish_metadata <- read.csv("fish_time_metadata.csv", header=TRUE)
fish_metadata$observer <- substr(fish_metadata$UniqueCode, 1, 2)
fish_metadata$UniqueCode <- as.character(fish_metadata$UniqueCode)

#Randomly selects 50 transects to use in the validation
sites <- fish_metadata
validation_transects <- sample_n(sites, size = 50, replace = FALSE)

#Now selects random minutes from the transect...
validation_transects$Minute <- 1

for (i in 1:nrow(validation_transects)) {
  
  validation_transects$Minute[i] <- sample(0:validation_transects$Length[i], size = 1)
  
}

validation_transects <- validation_transects %>%
  select(UniqueCode, Minute)

#Creates an empty dataframe to store our extracted images
image_metadata$Survey_Minute <- NA
retainedImages <- image_metadata[FALSE,]

validation_transects <- merge(validation_transects, fish_metadata, by = 'UniqueCode')
validation_transects <- validation_transects[,c('UniqueCode', 'Minute.x', 'Year', 'Month', 'Day', 'Hour', 'observer')]
colnames(validation_transects) <- c('UniqueCode', 'Minute', 'Year', 'Month', 'Day', 'Hour', 'Observer')
validation_transects$UniqueCode <- as.character(validation_transects$UniqueCode)

#Original seq_list 
#Use this when generating the human metadata -- pulls 20 images per minute (can be changed to whatever you want)
human_seq_list <- c(1, 6, 11, 16, 21, 23, 26, 31, 36, 41, 46, 51, 56, 61, 66, 71, 76, 81, 86, 90)

#The seq_list of all photos we have in the dataset
#Use these two lines below when generating the robot metadata (change the seq_list to be the same as the human one above if you change it)
coralnet_seq_list <- setdiff(1:90, human_seq_list) #samples the other images for the robot transects

#This following block prepares the human images
#Now this loop finds the transects selected above in the fish survey metadata and finds the time for each transect
#It then uses the time from the transects to find the appropriate images from the image_metadata dataframe and select the appropriate images
for (i in 1:nrow(validation_transects)) {
  
  #Converts the fish transect times into posix format
  k <- which(fish_metadata$UniqueCode==validation_transects$UniqueCode[i])
  
  begTime <- timeMaker(validation_transects$Year[i], validation_transects$Month[i], validation_transects$Day[i], 
                       fish_metadata$Hour[k], fish_metadata$Minute[k], 0)
  begTime <- begTime + (validation_transects$Minute[i] * 60)
  finTime <- begTime + (60)
  
  #Extracts images associated with a fish transect of row i
  tempImages <- image_metadata[image_metadata$Time >= begTime & image_metadata$Time < finTime & image_metadata$observer == validation_transects$Observer[i],]
  tempImages$UniqueCode <- validation_transects$UniqueCode[i]
  tempImages$Survey_Minute <- validation_transects$Minute[i]
  
  temp <- tempImages[human_seq_list,]
  temp$Image_Number <- human_seq_list
  
  #Saves all the images in tempFrame into our master frame that gets retained
  retainedImages <- rbind(retainedImages, temp)
}

retainedImages$file <- sub('\\..*', '', retainedImages$Filename)
retainedImages <- na.omit(retainedImages)

write.csv(retainedImages, file = "image_metadata_human.csv", row.names=FALSE) #writes the csv with all metadata of selected images
write.csv(unique(retainedImages$file), file = "images_human.csv", row.names=FALSE) #writes the csv with just the unique image names

####CORALNET IMAGES
#Now this loop finds the transects selected above in the fish survey metadata and finds the time for each transect
#It then uses the time from the transects to find the appropriate images from the image_metadata dataframe and select the appropriate images

#Creates an empty dataframe to store our extracted images
image_metadata$Survey_Minute <- NA
retainedImages <- image_metadata[FALSE,]

for (i in 1:nrow(validation_transects)) {
  
  #Converts the fish transect times into posix format
  k <- which(fish_metadata$UniqueCode==validation_transects$UniqueCode[i])
  
  begTime <- timeMaker(validation_transects$Year[i], validation_transects$Month[i], validation_transects$Day[i], 
                       fish_metadata$Hour[k], fish_metadata$Minute[k], 0)
  begTime <- begTime + (validation_transects$Minute[i] * 60)
  finTime <- begTime + (60)
  
  #Extracts images associated with a fish transect of row i
  tempImages <- image_metadata[image_metadata$Time >= begTime & image_metadata$Time < finTime & image_metadata$observer == validation_transects$Observer[i],]
  tempImages$UniqueCode <- validation_transects$UniqueCode[i]
  tempImages$Survey_Minute <- validation_transects$Minute[i]
  
  temp <- tempImages[coralnet_seq_list,]
  temp$Image_Number <- coralnet_seq_list
  
  #Saves all the images in tempFrame into our master frame that gets retained
  retainedImages <- rbind(retainedImages, temp)
}

retainedImages$file <- sub('\\..*', '', retainedImages$Filename)
retainedImages <- na.omit(retainedImages)

write.csv(retainedImages, file = "image_metadata_coralnet.csv", row.names=FALSE) #writes the csv with all metadata of selected images
write.csv(unique(retainedImages$file), file = "images_coralnet.csv", row.names=FALSE) #writes the csv with just the unique image names