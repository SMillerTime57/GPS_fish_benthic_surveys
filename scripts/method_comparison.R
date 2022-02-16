rm(list=ls())

#Loads packages
library(tidyverse)
library(plotrix)

###################################
#Loads in necessary data and makes minor transformations
###################################
gps_count_data <- read.csv("data\\fish_counts_gps_enabled.csv", header = T, stringsAsFactors = F)
gps_no_trio <- gps_count_data %>%
  filter(taxonomy != 'Acanthurus triostegus')

site_metadata <- read.csv("data\\fish_survey_metadata.csv", header = T, stringsAsFactors = F)

spp_met <- read.csv('data\\species_metadata.csv', header=TRUE)
spp_met <- spp_met %>%
  select(taxonomy, Herb_Type)

lterDat <- read.csv('data\\LTER_fish_counts.csv', header=TRUE, stringsAsFactors = FALSE)
lterDat$UniqueCode <- paste(as.character(lterDat$Year), as.character(lterDat$Transect), sep="_")
lterDat$Taxonomy[lterDat$Taxonomy=="Chlorurus spilurus"] <- "Chlorurus sordidus"
lterDat$Taxonomy[lterDat$Taxonomy == 'Zebrasoma velifer'] <- 'Zebrasoma veliferum'
lterDat$Total_Length <- lterDat$Total_Length / 10

#This is how you filter out years, habitat (backreef), site, Swath (always keep this at 5), and trohpic level (in this case, herbivores) and length (in our counts, we only counted >10cm)
lterDat_back <- lterDat %>%
  filter(Year == 2018) %>%
  filter(Habitat == "BA") %>%
  filter(Swath == 5) %>%
  filter(Fine_Trophic == "Herbivore/Detritivore") %>%
  filter(Total_Length >= 10) %>%
  filter(Taxonomy != "Acanthurus triostegus")

lterDat_back <- merge(lterDat_back, spp_met, by.x = 'Taxonomy', by.y = 'taxonomy', all.x = TRUE)

#################################
#Function for our fish_sum_stats function that generates proper density/biomass values based on the area sampled
#################################

fish_sum_stats <- function(allDat, dat, type, taxonomy = "all", species) {
  #Generates summary stats from fish counts for use in the Moorea CNH project
  #allDat is the UNALTERED dataframe of ALL fish observations -- used to generate areas for each minute
  #dat is the dataframe of fish counts -- can be filtered based on what you want to subset by
  #type is either "minute" or "transect", depending on the resolution of data that you want
  #taxonomy is the column title to be used to filter the fish: takes "all", "family", "genus", "taxonomy", "Coarse_Trophic", "Fine_Trophic", "Herb_Type"
  #Species is then the value of the taxonomy field you want to filter
  
  
  #Creates an empty dataframe using all the fish counts to fill in filtered-out transects.  
  #The length of this varies based on whether transects or minutes are selected, hence the if statement
  if (type == "transect") {
    
    
    all_fish <- allDat %>%
      group_by(Survey_code, Minute) %>%
      summarize(
        area = mean(area),
        Biomass = sum(mass, na.rm=TRUE),
        Density = sum(Number, na.rm=TRUE)
      ) %>%
      group_by(Survey_code) %>%
      summarize(
        area = sum(area),
        Biomass = (sum(Biomass) / sum(area)),
        Density = (sum(Density) / sum(area))
      )
    
    empty_fish <- all_fish
    empty_fish[, c("Biomass", "Density")] <- NA
    
    site_areas <- empty_fish[,c("Survey_code", "area")]
    
    all_fish_minute <- allDat %>%
      group_by(Survey_code, Minute) %>%
      summarize(
        area = mean(area),
        Biomass = sum(mass, na.rm=TRUE),
        Density = sum(Number, na.rm=TRUE),
        allCode = first(allCode)
      )
    
    empty_fish_minute <- all_fish_minute
    empty_fish_minute[, c("Biomass", "Density")] <- NA
    
    site_areas_minute <- empty_fish_minute[,c("Survey_code", "allCode", "area")]
    
    #If 'all' fishes are selected, then the dataframe is returned (no need to continue)
    if (taxonomy == 'all') {
      
      return_fish <- dat %>%
        group_by(Survey_code, Minute) %>%
        summarize(
          allCode = first(allCode),
          area = mean(area),
          Biomass = sum(mass),
          Density = sum(Number)
        )
      
    } else {
      
      #If taxonomy is something else, it moves to this step where the data are filtered and merged onto the empty frame
      return_fish <- dat[dat[,taxonomy] == species,] %>%
        group_by(Survey_code, Minute) %>%
        summarize(
          allCode = first(allCode),
          area = mean(area),
          Biomass = sum(mass),
          Density = sum(Number)
        )
    }
    
    return_fish <- merge(empty_fish_minute, return_fish, by = 'allCode', all.x= TRUE) %>%
      select(allCode, Survey_code.x, Minute.x, area.x, Biomass.y, Density.y)
    colnames(return_fish) <- c('allCode', 'Survey_code', 'Minute', 'area', 'Biomass', 'Density')
    return_fish$Biomass[is.na(return_fish$Biomass)] <- 0
    return_fish$Density[is.na(return_fish$Density)] <- 0
    
    return_fish <- return_fish %>%
      group_by(Survey_code) %>%
      summarize(
        area = sum(area),
        Biomass = (sum(Biomass) / sum(area)),
        Density = (sum(Density) / sum(area))
      )
    
    return_fish <- merge(empty_fish, return_fish, by = "Survey_code", all.x=TRUE)
    return_fish$area.y <- return_fish$area.x
    return_fish$area.x <- NULL
    return_fish$Biomass.x <- NULL
    return_fish$Density.x <- NULL
    colnames(return_fish) <- c("Survey_code", "area", "Biomass", "Density")
    return_fish$Biomass[is.na(return_fish$Biomass)] <- 0
    return_fish$Density[is.na(return_fish$Density)] <- 0
    
    return(return_fish)
    
    #generates the empty frame for 'minute' segments
  } else if (type == "minute") {
    
    all_fish <- allDat %>%
      group_by(Survey_code, Minute) %>%
      summarize(
        area = mean(area),
        Biomass = sum(mass, na.rm=TRUE),
        Density = sum(Number, na.rm=TRUE),
        allCode = first(allCode)
      )
    
    empty_fish <- all_fish
    empty_fish[, c("Biomass", "Density")] <- NA
    
    site_areas <- empty_fish[,c("Survey_code", "area")]
    
    #If 'all' fishes are selected, then the dataframe is returned (no need to continue)
    if (taxonomy == 'all') {
      
      return_fish <- dat %>%
        group_by(Survey_code, Minute) %>%
        summarize(
          area = mean(area),
          Biomass = sum(mass) / mean(area),
          Density = sum(Number) / mean(area),
          allCode = first(allCode)
        ) 
      
    } else {
      
      #If taxonomy is something else, it moves to this step where the data are filtered and merged onto the empty frame
      return_fish <- dat[dat[,taxonomy] == species,] %>%
        group_by(Survey_code, Minute) %>%
        summarize(
          area = mean(area),
          Biomass = sum(mass) / mean(area),
          Density = sum(Number) / mean(area),
          allCode = first(allCode)
        ) 
    }
    
    return_fish <- merge(empty_fish, return_fish, by = "allCode", all.x=TRUE)
    return_fish$area.y <- return_fish$area.x
    return_fish$area.x <- NULL
    return_fish$Biomass.x <- NULL
    return_fish$Density.x <- NULL
    return_fish$Survey_code.y <- NULL
    return_fish$Minute.y <- NULL
    colnames(return_fish) <- c("allCode", "Survey_code", "Minute", "area", "Biomass", "Density")
    return_fish$Biomass[is.na(return_fish$Biomass)] <- 0
    return_fish$Density[is.na(return_fish$Density)] <- 0
    
    return(return_fish)
    
  }
}

###############################################################################
#############
#Summarizes the data by site for gps-enabled counts
#############

cnh_summary <- data.frame("Lagoon" = character(), "Site" = integer(), "Habitat" = character(), "DataType" = character(), "Species" = character(), "Biomass" = numeric(), "Density" = numeric(), stringsAsFactors = FALSE)
spp_list <- c('Grazer', 'Browser', 'Detritivore', 'Scraper', 'Excavator')

for (i in 1:length(spp_list)){
  
  tempDat <- fish_sum_stats(gps_count_data, gps_no_trio, type = 'minute', taxonomy = 'Herb_Type', species = spp_list[i])
  tempDat <- merge(tempDat, site_metadata, by = 'allCode')
  
  tempDat <- tempDat %>%
    group_by(LTER_site, Habitat) %>%
    summarize(Biomass = mean(Biomass),
              Density = mean(Density))
  
  tempDat$Lagoon <- c('Hilton', 'MRB', 'Temae', 'Haapiti', 'Hauru')
  tempDat$Species <- spp_list[i]
  tempDat$DataType <- 'CNH'
  tempDat <- tempDat[,c('Lagoon', 'LTER_site', 'Habitat', 'DataType', 'Species', 'Biomass', 'Density')]
  colnames(tempDat)[2] <- 'Site'
  tempDat <- data.frame(tempDat)
  cnh_summary <- rbind(cnh_summary,tempDat)
}

###############################################################################
#############
#Summarizes the data by site for LTER counts
#############

lagoon_list <- c('Hilton', 'MRB', 'Temae', 'Farehau', 'Haapiti', 'Hauru')
LTER_summary <- data.frame("Lagoon" = character(), "Site" = integer(), "Habitat" = character(), "DataType" = character(), "Species" = character(), "Biomass" = numeric(), "Density" = numeric(), stringsAsFactors = FALSE)


for (i in 1:length(lagoon_list)){
  
  tempDat <- lterDat_back
  tempLTER <- tempDat %>%
    filter(Site == i)
  
  tempFrame <- data.frame("Lagoon" = character(length=1), "Site" = integer(length=1), "Habitat" = character(length = 1), "DataType" = character(length=1), "Species" = character(length=1), "Biomass" = numeric(length=1), "Density" = numeric(length=1), stringsAsFactors = FALSE)
  
  for (k in 1:length(spp_list)){
    
    temp <- tempLTER %>%
      group_by(UniqueCode) %>%
      filter(Herb_Type == spp_list[k], .preserve = TRUE) %>%
      summarize(Biomass = sum(Biomass) / 250,
                Density = sum(Count) / 250)
    
    tempFrame$Lagoon[1] <- lagoon_list[i]
    tempFrame$Site[1] <- i
    tempFrame$DataType[1] <- "LTER"
    tempFrame$Habitat[1] <- "Back"
    tempFrame$Species[1] <- spp_list[k]
    tempFrame$Biomass[1] <- mean(temp$Biomass)
    tempFrame$Density[1] <- mean(temp$Density)
    
    LTER_summary <- rbind(LTER_summary, tempFrame)
  }
}

################################################################################
##############
#Combines the two datasets and calculates correleations
##############

all_summary <- rbind(cnh_summary, LTER_summary)
all_summary$allCode <- paste(all_summary$Site, all_summary$Habitat, sep = "_")
site_codes <- unique(all_summary$allCode[all_summary$DataType=="CNH"])

lter_short <- LTER_summary %>%
  filter(Site != 4)

lagoon_list <- c('Hilton', 'MRB', 'Temae', 'Haapiti', 'Hauru')

spp_col <- c('#440154ff', '#39568cff', '#1f968bff', '#73d055ff', 'goldenrod')
lag_pch <- c(15, 16, 17, 3, 4)
lag_pch <- c(16, 16, 16, 16, 16)

LTER_summary_temp <- LTER_summary %>%
  filter(Lagoon != 'Farehau')

x <- LTER_summary_temp[order(LTER_summary_temp$Site, LTER_summary_temp$Species),]
y <- cnh_summary[order(cnh_summary$Site, cnh_summary$Species),]

bio500 <- cor.test(x$Biomass, y$Biomass)
dens500 <- cor.test(x$Density, y$Density)

################################################################################
##########
#Plots biomass
##########

dev.new()
par(oma = c(1, 2, 1, 1))
plot(-1,-1, xlab = '', ylab = '', xlim = c(0,20), ylim = c(0,20),
     cex.lab = 1.5, cex.axis = 1.35)
segments(0,0,30,30, col = 'gray50', lty = 2, lwd = 1.5)

for(i in 1:length(spp_list)) {
  
  for(j in 1:length(lagoon_list)){
    
    lter_plot <- LTER_summary_temp %>%
      filter(Species == spp_list[i]) %>%
      filter(Lagoon == lagoon_list[j])
    
    cnh_plot <- cnh_summary %>%
      filter(Species == spp_list[i]) %>%
      filter(Lagoon == lagoon_list[j])
    
    points(lter_plot$Biomass, cnh_plot$Biomass, col = spp_col[i], pch = lag_pch[j], cex = 2)
    
  }
  
}
text(2,17, labels = paste('r = ', round(bio500$estimate, 4), sep = ""), cex = 1.25)
text(0.5, 19.5, labels = 'a)', cex = 2.5)
mtext(expression(paste('LTER Biomass (g/m'^'2'*')')), side = 1, line = 3.5, cex = 1.75)
mtext(expression(paste('GPS-enabled Biomass (g/m'^'2'*')')), side = 2, line = 2.5, cex = 1.75)

#Makes another plot for the legends
legend_pch <- c(15, 16, 17, 18, 3, 4)
dev.new()
plot(FALSE)
legend(1.11, 1, legend = c('LTER 1', 'LTER 2', 'LTER 3', 'LTER 4', 'LTER 5', 'LTER 6'), pch = legend_pch, cex = 1.5, bty = 'n', title = 'Site')
legend(1.09, 0, legend = spp_list, fill = spp_col, cex = 1.5, bty = 'n', title = 'Herbivore Group')


################################################################################
##########
#Plots density
##########

dev.new()
par(oma = c(1, 2, 1, 1))
plot(-1,-1, xlab = '', ylab = '', xlim = c(0,.2), ylim = c(0,.2),
     cex.lab = 1.75, cex.axis = 1.35)
segments(0,0,20,20, col = 'gray50', lty = 2, lwd = 1.5)

for(i in 1:length(spp_list)) {
  
  for(j in 1:length(lagoon_list)){
    
    lter_plot <- LTER_summary_temp %>%
      filter(Species == spp_list[i]) %>%
      filter(Lagoon == lagoon_list[j])
    
    cnh_plot <- cnh_summary %>%
      filter(Species == spp_list[i]) %>%
      filter(Lagoon == lagoon_list[j])
    
    points(lter_plot$Density, cnh_plot$Density, col = spp_col[i], pch = lag_pch[j], cex = 2)
    
  }
  
}

text(.02,.17, labels = paste('r = ', round(dens500$estimate, 4), sep = ""), cex = 1.25)
text(0.005, 0.198, labels = 'b)', cex = 2.5)
mtext(expression(paste('LTER Density (ind/m'^'2'*')')), side = 1, line = 3.5, cex = 1.75)
mtext(expression(paste('GPS-enabled Density (ind/m'^'2'*')')), side = 2, line = 2.5, cex = 1.75)


################################################################################
##########################
##########################
#BIAS PLOTS
##########################
##########################

##########
#BIOMASS
##########

lter_bias <- data.frame('dataType' = character(length=length(spp_list)), 'HerbType' = character(length=length(spp_list)), 
                        'avg' = numeric(length=length(spp_list)), 'stdDev' = numeric(length=length(spp_list)), 
                        'stdErr' = numeric(length=length(spp_list)), stringsAsFactors = FALSE)

cnh_bias <- data.frame('dataType' = character(length=length(spp_list)), 'HerbType' = character(length=length(spp_list)), 
                       'avg' = numeric(length=length(spp_list)), 'stdDev' = numeric(length=length(spp_list)), 
                       'stdErr' = numeric(length=length(spp_list)), stringsAsFactors = FALSE)

for(i in 1:length(spp_list)) {
  
  lter_plot <- LTER_summary_temp %>%
    filter(Species == spp_list[i])
  
  lter_bias$dataType[i] <- 'LTER'
  lter_bias$HerbType[i] <- spp_list[i]
  lter_bias$avg[i] <- mean(lter_plot$Biomass)
  lter_bias$stdDev[i] <- sd(lter_plot$Biomass)
  lter_bias$stdErr[i] <- std.error(lter_plot$Biomass)
  
  cnh_plot <- cnh_summary %>%
    filter(Species == spp_list[i])
  
  cnh_bias$dataType[i] <- 'cnh'
  cnh_bias$HerbType[i] <- spp_list[i]
  cnh_bias$avg[i] <- mean(cnh_plot$Biomass)
  cnh_bias$stdDev[i] <- sd(cnh_plot$Biomass)
  cnh_bias$stdErr[i] <- std.error(cnh_plot$Biomass)
  
}

dev.new()
par(oma = c(1, 2, 1, 1))
plot(-1,-1, xlab = '', ylab = '', xlim = c(0,8), ylim = c(0,15),
     cex.lab = 1.5, cex.axis = 1.35, xaxt = 'n')

cnh_x <- c(1,2.5,4,5.5,7)
lter_x <- c(1.5, 3, 4.5, 6, 7.5)

points(c(1,2.5,4,5.5,7), cnh_bias$avg, pch = 16, cex = 2, col = spp_col)
points(c(1.5, 3, 4.5, 6, 7.5), lter_bias$avg, pch = 15, cex = 2, col = spp_col)
arrows(c(1, 2.5, 4, 5.5, 7), cnh_bias$avg, c(1, 2.5, 4, 5.5, 7), cnh_bias$avg + cnh_bias$stdErr, length = 0.1, angle = 90)
arrows(c(1, 2.5, 4, 5.5, 7), cnh_bias$avg, c(1, 2.5, 4, 5.5, 7), cnh_bias$avg - cnh_bias$stdErr, length = 0.1, angle = 90)
arrows(c(1.5, 3, 4.5, 6, 7.5), lter_bias$avg, c(1.5, 3, 4.5, 6, 7.5), lter_bias$avg + lter_bias$stdErr, length = 0.1, angle = 90)
arrows(c(1.5, 3, 4.5, 6, 7.5), lter_bias$avg, c(1.5, 3, 4.5, 6, 7.5), lter_bias$avg - lter_bias$stdErr, length = 0.1, angle = 90)

axis(1, at = (cnh_x + lter_x)/2, labels = spp_list, tick = TRUE, cex.axis = 1.15)
text(0.1, 14.8, labels = 'c)', cex = 2.5)
mtext('Herbivore Group', side = 1, line = 3.5, cex = 1.75)
mtext(expression(paste('Mean Biomass (g/m'^'2'*')')), side = 2, line = 2.5, cex = 1.75)
legend('topright', legend = c('GPS-enabled', 'Fixed-area'), pch = c(16, 15))

################################################################################
##########
#DENSITY
##########

lter_bias <- data.frame('dataType' = character(length=length(spp_list)), 'HerbType' = character(length=length(spp_list)), 
                        'avg' = numeric(length=length(spp_list)), 'stdDev' = numeric(length=length(spp_list)), 
                        'stdErr' = numeric(length=length(spp_list)), stringsAsFactors = FALSE)

cnh_bias <- data.frame('dataType' = character(length=length(spp_list)), 'HerbType' = character(length=length(spp_list)), 
                       'avg' = numeric(length=length(spp_list)), 'stdDev' = numeric(length=length(spp_list)), 
                       'stdErr' = numeric(length=length(spp_list)), stringsAsFactors = FALSE)

for(i in 1:length(spp_list)) {
  
  lter_plot <- LTER_summary_temp %>%
    filter(Species == spp_list[i])
  
  lter_bias$dataType[i] <- 'LTER'
  lter_bias$HerbType[i] <- spp_list[i]
  lter_bias$avg[i] <- mean(lter_plot$Density)
  lter_bias$stdDev[i] <- sd(lter_plot$Density)
  lter_bias$stdErr[i] <- std.error(lter_plot$Density)
  
  cnh_plot <- cnh_summary %>%
    filter(Species == spp_list[i])
  
  cnh_bias$dataType[i] <- 'cnh'
  cnh_bias$HerbType[i] <- spp_list[i]
  cnh_bias$avg[i] <- mean(cnh_plot$Density)
  cnh_bias$stdDev[i] <- sd(cnh_plot$Density)
  cnh_bias$stdErr[i] <- std.error(cnh_plot$Density)
  
}

dev.new()
par(oma = c(1, 2, 1, 1))
plot(-1,-1, xlab = '', ylab = '', xlim = c(0,8), ylim = c(0,0.15),
     cex.lab = 1.5, cex.axis = 1.35, xaxt = 'n')

cnh_x <- c(1,2.5,4,5.5,7)
lter_x <- c(1.5, 3, 4.5, 6, 7.5)

points(c(1,2.5,4,5.5,7), cnh_bias$avg, pch = 16, cex = 2, col = spp_col)
points(c(1.5, 3, 4.5, 6, 7.5), lter_bias$avg, pch = 15, cex = 2, col = spp_col)
arrows(c(1, 2.5, 4, 5.5, 7), cnh_bias$avg, c(1, 2.5, 4, 5.5, 7), cnh_bias$avg + cnh_bias$stdErr, length = 0.1, angle = 90)
arrows(c(1, 2.5, 4, 5.5, 7), cnh_bias$avg, c(1, 2.5, 4, 5.5, 7), cnh_bias$avg - cnh_bias$stdErr, length = 0.1, angle = 90)
arrows(c(1.5, 3, 4.5, 6, 7.5), lter_bias$avg, c(1.5, 3, 4.5, 6, 7.5), lter_bias$avg + lter_bias$stdErr, length = 0.1, angle = 90)
arrows(c(1.5, 3, 4.5, 6, 7.5), lter_bias$avg, c(1.5, 3, 4.5, 6, 7.5), lter_bias$avg - lter_bias$stdErr, length = 0.1, angle = 90)

axis(1, at = (cnh_x + lter_x)/2, labels = spp_list, tick = TRUE, cex.axis = 1.15)
text(0.1, 0.148, labels = 'd)', cex = 2.5)
mtext('Herbivore Group', side = 1, line = 3.5, cex = 1.75)
mtext(expression(paste('Mean Density (ind/m'^'2'*')')), side = 2, line = 2.5, cex = 1.75)
legend('topright', legend = c('GPS-enabled', 'Fixed-area'), pch = c(16, 15))