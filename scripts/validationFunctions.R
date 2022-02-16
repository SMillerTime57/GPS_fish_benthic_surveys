library(tidyverse)

generate_reference <- function(dat, num, cover = c('Coral', 'Algae', 'LowRel', 'HardSub', 'OtherSub', 'Obscured')) {
  
  #Creates an empty frame that we use for saving the 'reference' covers
  reference_cover <- setNames(data.frame(matrix(ncol = length(cover) + 1, nrow = 0)), c('SurveyCode', cover))
  
  #Creates an empty frame that we save the image names used in the 'reference' cover calculation
  reference_images <- data.frame('SurveyCode' = character(), 'ImageName' = character())
  
  for (survey in unique(dat$SurveyCode)) {
    
    #Randomly samples num number of images from the human annotated data
    temp <- sample_n(dat[dat$SurveyCode==survey,], num, replace = FALSE) %>%
      dplyr::select(Name, SurveyCode, cover)
    
    #Calculates and saves the 'reference' cover estimates from this sample
    temp_cover <- colMeans(temp[,cover])
    reference_cover[nrow(reference_cover)+1, 'SurveyCode'] <- survey
    reference_cover[nrow(reference_cover), cover] <- temp_cover
    
    #Saves the images dplyr::selected for the 'reference' cover, so in future steps we remove these from the image pool
    temp_images <- data.frame('SurveyCode' = rep(survey, times = num), 'ImageName' = as.character(temp$Name))
    reference_images <- rbind(reference_images, temp_images)
    
  }
  
  return(list(reference_cover, reference_images))
  
}

sample_human <- function(dat, files_to_exclude, sample_n, reps, cover = c('Coral', 'Algae', 'LowRel', 'HardSub', 'OtherSub', 'Obscured')) {
  
  #Creates an empty frame that we use for saving the 'reference' covers
  sample_cover <- setNames(data.frame(matrix(ncol = length(cover) + 3, nrow = 0)), c('SurveyCode', 'nImages', 'Replicate', cover))
  
  for (survey in unique(dat$SurveyCode)) {
    
    #Isolates the images to filter out (because they were used to estimate 'reference' cover)
    filter_images <- files_to_exclude %>%
      filter(SurveyCode == survey)
    
    #Extracts the set of unused images from the target survey
    temp_images <- dat %>%
      filter(SurveyCode == survey) %>%
      filter(!Name %in% as.character(filter_images$ImageName))
    
    for (i in 1:reps) {
      
      #Randomly samples sample_n number of images from the available images
      temp <- sample_n(temp_images, sample_n, replace = FALSE) %>%
        dplyr::select(Name, SurveyCode, cover)
      
      #Calculates and saves the sample cover estimates from this replicate
      temp_cover <- colMeans(temp[,cover])
      sample_cover[nrow(sample_cover)+1, 'SurveyCode'] <- survey
      sample_cover[nrow(sample_cover), 'nImages'] <- sample_n
      sample_cover[nrow(sample_cover), 'Replicate'] <- i
      sample_cover[nrow(sample_cover), cover] <- temp_cover
      
    }
  }
  
  return(sample_cover)
  
}

sample_robot <- function(dat, sample_n, reps, cover = c('Coral', 'Algae', 'LowRel', 'HardSub', 'Obscured')) {
  
  #Creates an empty frame that we use for saving the robot sample covers
  sample_cover <- setNames(data.frame(matrix(ncol = length(cover) + 3, nrow = 0)), c('SurveyCode', 'nImages', 'Replicate', cover))
  
  for (survey in unique(dat$SurveyCode)) {
    
    #Extracts the set of images from the target survey
    temp_images <- dat %>%
      filter(SurveyCode == survey)
    
    for (i in 1:reps) {
      
      #Randomly samples sample_n number of images from the available images
      temp <- sample_n(temp_images, sample_n, replace = FALSE) %>%
        dplyr::select(Name, SurveyCode, cover)
      
      #Calculates and saves the sample cover estimates from this replicate
      temp_cover <- colMeans(temp[,cover])
      sample_cover[nrow(sample_cover)+1, 'SurveyCode'] <- survey
      sample_cover[nrow(sample_cover), 'nImages'] <- sample_n
      sample_cover[nrow(sample_cover), 'Replicate'] <- i
      sample_cover[nrow(sample_cover), cover] <- temp_cover
      
    }
  }
  
  return(sample_cover)
  
}

generate_relationships <- function(reference_dat, sample_dat, cover = c('Coral', 'Algae', 'LowRel', 'HardSub', 'Obscured')) {
  
  #Sorts the true dataset alphabetically
  reference_dat <- reference_dat %>%
    arrange(SurveyCode)
  
  #Generates the master frame used to hold all the relevant information that is returned
  master_relationship <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c('nImages', 'Replicate', 'Substrate', 'R_square', 'Slope', 'Intercept'))
  
  for (i in 1:max(sample_dat$Replicate)) {
    
    #Filters data based on the replicate #
    filtered_dat <- sample_dat %>%
      filter(Replicate == i) %>%
      arrange(SurveyCode)
    
    for (substrate in cover) {
      
      #Creates a temporary dataframe to hold our two comparison values together
      temp_frame <- data.frame('SurveyCode' = reference_dat$SurveyCode, 'ReferenceCover' = reference_dat[,substrate], 'SampleCover' = filtered_dat[,substrate])
      
      #Generates a linear model between the 'true' cover and the sample data
      temp_model <- lm(ReferenceCover ~ SampleCover, dat = temp_frame)
      
      master_relationship[nrow(master_relationship)+1, 'nImages'] <- sample_dat$nImages[1]
      master_relationship[nrow(master_relationship), 'Replicate'] <- i
      master_relationship[nrow(master_relationship), 'Substrate'] <- substrate
      master_relationship[nrow(master_relationship), 'R_square'] <- summary(temp_model)$r.squared
      master_relationship[nrow(master_relationship), 'Slope'] <- temp_model$coefficients[2]
      master_relationship[nrow(master_relationship), 'Intercept'] <- temp_model$coefficients[1]
      
      
    }
    
  }
  
  return(master_relationship)
  
}

plot_octave <- function(humanDat, robotDat, substrates = c('Coral', 'Algae', 'LowRel', 'HardSub'), metrics = c('R_square', 'Slope', 'Intercept')) {
  
  for (substrate in substrates){
    
    plot_human <- humanDat %>%
      filter(Substrate == substrate)
    
    plot_robot <- robotDat %>%
      filter(Substrate == substrate,
             nImages <= 64)
    
    #Changes the robot x-values to plot on octave scale
    plot_robot$plotX <- 1
    plot_robot$plotX[plot_robot$nImages == 2] <- 2
    plot_robot$plotX[plot_robot$nImages == 4] <- 4
    plot_robot$plotX[plot_robot$nImages == 8] <- 8
    plot_robot$plotX[plot_robot$nImages == 16] <- 9
    plot_robot$plotX[plot_robot$nImages == 32] <- 10
    plot_robot$plotX[plot_robot$nImages == 64] <- 11
    
    for (metric in metrics){
      
      human_mean <- tapply(plot_human[,metric], plot_human[,'nImages'], FUN = mean)
      human05 <- tapply(plot_human[,metric], plot_human[,'nImages'], FUN = quantile, probs = 0.025, na.rm=TRUE)
      human95 <- tapply(plot_human[,metric], plot_human[,'nImages'], FUN = quantile, probs = 0.975, na.rm=TRUE)
      
      robot_mean <- tapply(plot_robot[,metric], plot_robot[,'plotX'], FUN = mean)
      robot05 <- tapply(plot_robot[,metric], plot_robot[,'plotX'], FUN = quantile, probs = 0.025, na.rm=TRUE)
      robot95 <- tapply(plot_robot[,metric], plot_robot[,'plotX'], FUN = quantile, probs = 0.975, na.rm=TRUE)
      
      if (metric == 'R_square'){
        
        lowlim <- 0
        uplim <- 1
        
      } else {
        
        lowlim <- min(c(plot_human[,metric], plot_robot[,metric])) * 1.1
        uplim <- max(c(plot_human[,metric], plot_robot[,metric])) * 1.1
        
      }
      
      dev.new()
      #plots human data and creates plot window
      plot(plot_human$nImages-.1, plot_human[,metric], xlab = 'Number of Images', ylab = metric, 
           cex.lab = .5, col = 'azure2', main = paste(substrate, metric, sep = " "), xlim = c(0,11), ylim = c(lowlim, uplim), cex.lab = 1.4, xaxt = 'n')
      axis(side = 1, at = 1:11, labels = c(1, 2, 3, 4, 5, 6, 7, 8, 16, 32, 64))
      #Plots robot data points
      points(plot_robot$plotX+.1, plot_robot[,metric], 
             cex.lab = .5, col = 'azure2')
      
      #Plots human medians/means and bars
      points(unique(plot_human$nImages-.1), human_mean, cex = 1.5, col = 'firebrick3', pch = 16)
      arrows(unique(plot_human$nImages-.1), human_mean, 
             unique(plot_human$nImages-.1), human05, angle = 90, col='firebrick3', length = 0.15, lwd = 2)
      arrows(unique(plot_human$nImages-.1), human_mean, 
             unique(plot_human$nImages-.1), human95, angle = 90, col='firebrick3', length = 0.15, lwd = 2)
      
      #Plots human medians/means and bars
      points(unique(plot_robot$plotX+.1), robot_mean, cex = 1.5, col = '#0072B2', pch = 16)
      arrows(unique(plot_robot$plotX+.1), robot_mean, 
             unique(plot_robot$plotX+.1), robot05, angle = 90, col='#0072B2', length = 0.15, lwd = 2)
      arrows(unique(plot_robot$plotX+.1), robot_mean, 
             unique(plot_robot$plotX+.1), robot95, angle = 90, col='#0072B2', length = 0.15, lwd = 2)
      
      legend('bottomright', c('Human', 'Computer Vision'), fill = c('firebrick3', '#0072B2'))
      
    }
  }
}
