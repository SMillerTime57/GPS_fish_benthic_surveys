rm(list=ls())

######################
######################
#Loads in packages, source files, and data
######################
######################
source('scripts\\validationFunctions.R')
humanCover <- read.csv("data\\validation_manual_cover.csv", stringsAsFactors = F)
robotCover <- read.csv("data\\validation_coralnet_cover.csv", stringsAsFactors = F)

######################
######################
#Generates 'human' (i.e., manually-annotated) evaluation
######################
######################

#Creates a master datasheet to hold results from all the manually-annotated data
master_human <- data.frame('nImages' = integer(), 'Replicate' = integer(), 'Substrate' = character(), 
                           'R_square' = numeric(), 'Slope' = numeric(), 'Intercept' = numeric())

#Sets the different numbers of manually-annotated sample images per transect segment we want to explore
nImages_human <- 1:8

#Makes the vector of benthic covers we're interested in
covers <- c('Coral', 'Algae', 'SoftSub', 'HardSub', 'Obscured')

#Begins a loop to create the numerous regressions.  
#The outer loop (i in 1:100) determines how many random reference datasets will be made (this is different than "reps" which determines how many times the samples will be randomly drawn per reference set).  
#The inner loop (j in nImages_human) iterates over the different number of sample images you desire to use
for (i in 1:100) {
  
  #Generates the reference dataset using 12 photographs per transect segment
  reference_data <- generate_reference(humanCover, 12, cover = covers)
  
  for (j in nImages_human) {
    
    #Generates "reps" number of replicates of the sample dataset using "j" number of photographs per transect segment, randomizing photographs each time
    sample_data <- sample_human(humanCover, reference_data[[2]], sample_n = j, reps = 1000, cover = covers)
    
    #Calculates regressions for each replicate of the sample data against the static reference dataset
    relationship_data <- generate_relationships(reference_data[[1]], sample_data, cover = covers)
    
    #Combines the data from each run into the master datasheet
    master_human <- rbind(master_human, relationship_data)
    
  }
}

#Writes the outputs to a csv so this time-consuming process doesn't need to be repeated
write.csv(master_human, 'master_human_output.csv', row.names=FALSE)

######################
######################
#Generates 'robot' (i.e., CoralNet-annotated) evaluation
######################
######################

#Creates a master datasheet to hold results for all the CoralNet-annotated data
master_robot <- data.frame('nImages' = integer(), 'Replicate' = integer(), 'Substrate' = character(), 
                           'R_square' = numeric(), 'Slope' = numeric(), 'Intercept' = numeric())

#Sets the different numbers of manually-annotated sample images per transect segment we want to explore
nImages_robot <- c(1, 2, 4, 8, 16, 32, 64)

#Begins a loop to create the numerous regressions.  
#The outer loop (i in 1:100) determines how many random reference datasets will be made (this is different than "reps" which determines how many times the samples will be randomly drawn per reference set).  
#The inner loop (j in nImages_robot) iterates over the different number of sample images you desire to use
for (i in 1:100) {
  
  #Generates the reference dataset using 12 photographs per transect segment
  reference_data <- generate_reference(humanCover, 12, cover = covers)
  
  for (j in nImages_robot) {
    
    #Generates "reps" number of replicates of the sample dataset using "j" number of photographs per transect segment, randomizing photographs each time
    sample_data <- sample_robot(robotCover, sample_n = j, reps = 1000, cover = covers)
    
    #Calculates regressions for each replicate of the sample data against the static reference dataset
    relationship_data <- generate_relationships(reference_data[[1]], sample_data, cover = covers)
    
    #Combines the data from each run into the master datasheet
    master_robot <- rbind(master_robot, relationship_data)
    
  }
}

#Writes the outputs to a csv so this time-consuming process doesn't need to be repeated
write.csv(master_robot, 'master_robot_output.csv', row.names=FALSE)

######################
######################
#Visualizing results
######################
######################

#Using this function, the results for all three metrics for each substrate can be seen.  
#The arguments "substrates" and "metrics" can also accept single values, in case you only want to plot a subset of the entire results
plot_octave(na.omit(master_human), na.omit(master_robot), substrates = covers, metrics = c("R_square", "Slope", "Intercept"))
