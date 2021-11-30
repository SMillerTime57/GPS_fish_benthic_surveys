rm(list=ls())

#Loads in functions and package dependencies
source('gps_functions.R')
library(tidyverse)

#Fields to change
img_lag <- 22 #The number of photographs ahead of the current time that should be sampled to better match the photographs with the actual locations of fish surveys
photos_to_extract <- 64 #Number of photographs to sample from each minute of fish counting

#Loads in image metadata with time and coordinates and converts to proper time format
image_metadata <- read.csv("image_metadata.csv", header=TRUE, stringsAsFactors = FALSE)
image_metadata$Time <- strptime(image_metadata$Time, "%Y-%m-%d %H:%M:%S")
image_metadata$observer <- substr(image_metadata$Filename, 1, 2)

#Loads in metadata about times the fish surveys were conducted
fish_metadata <- read.csv("fish_time_metadata.csv", header=TRUE)
fish_metadata$observer <- substr(fish_metadata$UniqueCode, 1, 2)
fish_metadata$UniqueCode <- as.character(fish_metadata$UniqueCode)

#Loads in the metadata about each site
site_metadata <- read.csv('site_metadata.csv', header=TRUE)

#Generates a list of unique values for each minute of fish counting used to extract relevant photos
#NOTE: depending on your needs or size of dataset, it may be useful to filter site_metadata here
sites_to_use <- data.frame("UniqueCode" = character(), "Minute" = integer(), stringsAsFactors = FALSE)

for (site in as.character(sites$UniqueCode)){
  
  tempFrame <- data.frame("UniqueCode" = character(length = sites$SurveyLength[sites$UniqueCode==site]), "Minute" = integer(length = sites$SurveyLength[sites$UniqueCode==site]), stringsAsFactors = FALSE)
  
  for (i in 0:(sites$SurveyLength[sites$UniqueCode==site]-1)){
    
    tempFrame$UniqueCode[i+1] <- as.character(site)
    tempFrame$Minute[i+1] <- i
    
  }
  
  sites_to_use <- rbind(sites_to_use, tempFrame)
  
}

#Creates an additional variable, allCode, which is a unique identifier for each survey minute (overall transect name with minute appended)
sites_to_use$allCode <- paste(sites_to_use$UniqueCode, sites_to_use$Minute, sep ="_")

#Creates an empty dataframe to store our extracted images
image_metadata$Survey_Minute <- NA
retainedImages <- image_metadata[FALSE,]

#Merges the individual transect segments to the times associated with fish surveys and pulls out relevant columns
sites_to_use <- merge(sites_to_use, fish_metadata, by = 'UniqueCode')
sites_to_use <- sites_to_use[,c('UniqueCode', 'Minute.x', 'allCode', 'Year', 'Month', 'Day', 'Hour', 'observer')]
colnames(sites_to_use) <- c('UniqueCode', 'Minute', 'allCode', 'Year', 'Month', 'Day', 'Hour', 'Observer')
sites_to_use$UniqueCode <- as.character(sites_to_use$UniqueCode)

#Selects image numbers within each minute to be used for analysis
seq_list <- sample(1:90, photo_to_extract, replace = FALSE)

#LIST OF ALL SITES REMAINING IN DATA
all_sites <- unique(sites_to_use$UniqueCode)

#EXTRACTS A SITE TO BE USED
sample_sites <- sites_to_use

error_frame <- data.frame('Site' = character(), 'i' = integer(), 'n_images' = integer(), stringsAsFactors = FALSE)

#Loops through the list of minute-long transect segments to associate the photographs with each transect segment
for (i in 1:nrow(sample_sites)) {
  
  #Converts the fish transect times into posix format and shifts the time window forward based on imgLag to better-associate with fish surveys
  k <- which(fish_metadata$UniqueCode==sample_sites$UniqueCode[i])
  
  begTime <- timeMaker(sample_sites$Year[i], sample_sites$Month[i], sample_sites$Day[i], fish_metadata$Hour[k], fish_metadata$Minute[k], 0)
  begTime <- begTime + (sample_sites$Minute[i] * 60) + img_lag
  finTime <- begTime + (60)
  
  #Extracts images associated with a fish transect of row i
  tempImages <- image_metadata[image_metadata$Time >= begTime & image_metadata$Time < finTime & image_metadata$observer == sample_sites$Observer[i],]
  
  #If there are not 90 images (the value we expect) in a survey segment, it places the information in the error dataframe for later inspection
  #Otherwise, it saves the relevant information into a dataframe that is appended to our larger frame
  if(nrow(tempImages) != 90){
    
    temp_err <- data.frame('Site' = sample_sites$allCode[i], 'i' = i, 'n_images' = nrow(tempImages))
    error_frame <- rbind(error_frame, temp_err)
    
  } else{
    
    tempImages$UniqueCode <- sample_sites$UniqueCode[i]
    tempImages$Survey_Minute <- sample_sites$Minute[i]
    
    temp <- tempImages[seq_list,]
    temp$Image_Number <- seq_list
    
    #Saves all the images in tempFrame into our master frame that gets retained
    retainedImages <- rbind(retainedImages, temp)
    
  }
  
}

#Creates an additional column so the "Name" contains the entire photograph name, while "file" has the name without the file extension
colnames(retainedImages)[1] <- 'Name'
retainedImages$file <- sub('\\..*', '', retainedImages$Name)
retainedImages <- na.omit(retainedImages)

#Creates three csv files
#"error_minutes" returns any survey minutes with fewer than 90 images
#"extracted_image_metadata" contains each photograph with additional metadata on its paired fish survey
#"extracted_images" returns just the names of images used without the file extension at the end.  This is used in future steps to automatically copy the sampled photographs to a new folder
write.csv(error_frame, file = "error_minutes.csv", row.names=FALSE)
write.csv(retainedImages, file = "extracted_image_metadata.csv", row.names=FALSE)
write.csv(unique(retainedImages$file), file = "extracted_images.csv", row.names=FALSE)
