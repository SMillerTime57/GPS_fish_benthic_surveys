rm(list=ls())

library(tidyverse)

setwd('F:\\Moorea\\BertTesting')
source('bert_test_functions.R')
metaDat <- read.csv('image_metadata_human_all.csv', header=TRUE)
bertMeta <- read.csv('image_metadata_robot_all.csv', header=TRUE)
humanCover <- read.csv('human_cover_all.csv', header=TRUE)
bertAnn <- read.csv('robot_annotations_all.csv', header=TRUE)
otherDat <- read.csv('F:\\Moorea\\BertTesting\\Exports\\bert_test_other_images.csv', header=TRUE)

otherDat <- mutate(otherDat, SurveyCode = paste(UniqueCode, Survey_Minute, sep="_"))
metaDat <- mutate(metaDat, SurveyCode = paste(UniqueCode, Survey_Minute, sep="_"))

survey_list <- unique(metaDat$SurveyCode)

other_human <- otherDat %>%
  filter(SurveyCode %in% survey_list) %>%
  dplyr::select(1:39, UniqueCode, Survey_Minute, SurveyCode)

humanCover <- humanCover %>%
  left_join(metaDat) %>%
  dplyr::select(1:39, UniqueCode, Survey_Minute, SurveyCode)

humanCover <- rbind(humanCover, other_human) %>%
  dplyr::select(1, 4:42)

attach(humanCover)

#####
#####
#HIGHEST LEVEL - LOW RELIEF
#####
#####
#humanCover$Coral <- Acr_bra + Acrop + CSO + DAMI + LETT + MASE_SMO_P + Monti + OTH.HC + Pocill + P..rus
#humanCover$Algae <- ASP + Dict + Hali + Macro + Pad + Sarg + Turbin
#humanCover$LowRel <- Sand + SOFT_RUBB + Pav + DET_TURB
#humanCover$HardSub <- HARD_SUB + Turf
#humanCover$Obscured <- WATE + DEEP + obscured + Unk
#humanCover$OtherSub <- CYAN + Other + ANUN + CMOR + COTS + EUR + GiCl + Mille + SECO

# human_lowrel_totalcover <- data.frame("Coral" = sum(humanCover$Coral), "Algae" = sum(humanCover$Algae), 
#                                       "HardSub" = sum(humanCover$HardSub), "LowRel" = sum(humanCover$LowRel))

#####
#####
#HIGHEST LEVEL - HARD SUBSTRATE
#####
#####
humanCover$Coral <- Acr_bra + Acrop + CSO + DAMI + LETT + MASE_SMO_P + Monti + OTH.HC + Pocill + P..rus
humanCover$Algae <- ASP + Dict + Hali + Macro + Pad + Sarg + Turbin
humanCover$SoftSub <- Sand + SOFT_RUBB + DET_TURB
humanCover$HardSub <- HARD_SUB + Turf + Pav
humanCover$Obscured <- WATE + DEEP + obscured + Unk
humanCover$OtherSub <- CYAN + Other + ANUN + CMOR + COTS + EUR + GiCl + Mille + SECO

human_hardsub_totalcover <- data.frame("Coral" = sum(humanCover$Coral), "Algae" = sum(humanCover$Algae), 
                                      "HardSub" = sum(humanCover$HardSub), "SoftSub" = sum(humanCover$SoftSub))


#####
#####
#FIRST SPLIT
#####
#####
# humanCover$BranchingCoral <- Acr_bra + Acrop + DAMI + Pocill
# humanCover$OtherCoral <- CSO + LETT + OTH.HC
# humanCover$MassPorit <- MASE_SMO_P
# humanCover$Monti_Rus <- Monti + P..rus
# humanCover$OtherSub <- ANUN + CMOR + COTS + EUR + GiCl + Mille + SECO + CYAN + Other
# humanCover$HardSub <- HARD_SUB + Turf
# humanCover$LowRel <- Pav + DET_TURB + Sand + SOFT_RUBB
# humanCover$Obscured <- DEEP + obscured + Unk + WATE
# humanCover$OtherMacro <- Macro + ASP
# humanCover$BranchMacro <- Dict + Sarg + Turbin
# humanCover$Halimeda <- Hali
# humanCover$Padina <- Pad
# 
# human_firstsplit_totalcover <- data.frame("BranchingCoral" = sum(humanCover$BranchingCoral), "MassPorit" = sum(humanCover$MassPorit), 
#                                        "Monti_Rus" = sum(humanCover$Monti_Rus), "HardSub" = sum(humanCover$HardSub),
#                                        "LowRel" = sum(humanCover$LowRel), "Obscured" = sum(humanCover$Obscured),
#                                        "BranchMacro" = sum(humanCover$LowRel), "Halimeda" = sum(humanCover$Halimeda),
#                                        "Padina" = sum(humanCover$Padina))

human_all_totalcover <- humanCover[,2:37]
human_all_totalcover <- colSums(human_all_totalcover)
human_all_totalcover <- human_all_totalcover[order(human_all_totalcover, decreasing = TRUE)]

######################
######################
#Calculates percent cover from the machine annotations
######################
######################

bertAnn <- bertAnn[,c('Name', 'Machine.suggestion.1')]
bertAnn$dummy <- 1
bertAnn$point <- rep(1:30)

#Converts annotations to long format
bertAnn <- spread(bertAnn, key = 'Machine.suggestion.1', value = 'dummy', fill=0)
colnames(bertAnn)[as.integer(which(colnames(bertAnn) == "P. rus"))] <- 'P.rus' ###CHANGE THIS DEPENDIGN ON ITS LOCATION

bertAnn <- bertAnn %>%
  group_by(Name) %>%
  summarize(Acr_bra = sum(Acr_bra),
            Dict = sum(Dict),
            Hali = sum(Hali),
            HARD_SUB = sum(HARD_SUB),
            MASE_SMO_P = sum(MASE_SMO_P),
            Monti = sum(Monti),
            obscured = sum(obscured),
            P.rus = sum(P.rus),
            Pad = sum(Pad),
            Pav = sum(Pav),
            Pocill = sum(Pocill),
            Sand = sum(Sand),
            Sarg = sum(Sarg),
            SOFT_RUBB = sum(SOFT_RUBB),
            Turbin = sum(Turbin),
            Turf = sum(Turf)
  )

attach(bertAnn)

#####
#####
#HIGHEST LEVEL - LOW RELIEF
#####
#####
# bertAnn$Coral <- Acr_bra + MASE_SMO_P + Monti + Pocill + P.rus
# bertAnn$Algae <- Dict + Hali + Pad + Sarg + Turbin
# bertAnn$LowRel <- Sand + SOFT_RUBB + Pav
# bertAnn$HardSub <- HARD_SUB + Turf
# bertAnn$Obscured <- obscured

#####
#####
#HIGHEST LEVEL - HARD SUBSTRATE
#####
#####
bertAnn$Coral <- Acr_bra + MASE_SMO_P + Monti + Pocill + P.rus
bertAnn$Algae <- Dict + Hali + Pad + Sarg + Turbin
bertAnn$SoftSub <- Sand + SOFT_RUBB
bertAnn$HardSub <- HARD_SUB + Turf + Pav
bertAnn$Obscured <- obscured

#####
#####
#FIRST SPLIT
#####
#####
#bertAnn$BranchingCoral <- Acr_bra + Pocill
#bertAnn$MassPorit <- MASE_SMO_P
#bertAnn$Monti_Rus <- Monti + P.rus
#bertAnn$HardSub <- HARD_SUB + Turf
#bertAnn$LowRel <- Pav + Sand + SOFT_RUBB
#bertAnn$Obscured <- obscured
#bertAnn$BranchMacro <- Dict + Sarg + Turbin
#bertAnn$Halimeda <- Hali
#bertAnn$Padina <- Pad

attach(bertAnn)

#This total was just used to make sure there were 30 points per image
#bertAnn$total <- Coral + Algae + LowRel + HardSub + Obscured

bertAnn[,2:22] <- (bertAnn[,2:22] / 30) * 100 #THIS NEEDS TO BE CHANGED WHEN YOU CHANGE THE SPLIT

######################
######################
#Merge data and calculate grouped variables
######################
######################
robotCover <- merge(bertMeta, bertAnn, by = 'Name')
robotCover$SurveyCode <- paste(robotCover$UniqueCode, robotCover$Survey_Minute, sep = "_")
robotCover <- robotCover %>%
  dplyr::select(1, 11:26, 'UniqueCode', 'Survey_Minute', 'SurveyCode', 27:31)


######################
######################
#Generates 'human' evaluation
######################
######################

master_human <- data.frame('nImages' = integer(), 'Replicate' = integer(), 'Substrate' = character(), 
                           'R_square' = numeric(), 'Slope' = numeric(), 'Intercept' = numeric())
nImages_human <- 1:8
#nImages_human <- 8

#cover <- c('Turbin', 'Sand', 'MASE_SMO_P', 'Sarg')
#cover <- c('Coral', 'Algae')

#Low Relief cover
covers <- c('Coral', 'Algae', 'LowRel', 'HardSub', 'Obscured')
#Soft sub cover
covers <- c('Coral', 'Algae', 'SoftSub', 'HardSub', 'Obscured')
covers <- c("Pocill", "Coral")
#First Split Cover
#covers <- c('BranchingCoral', 'MassPorit', 'Monti_Rus', 'HardSub', 'LowRel', 'Obscured', 'BranchMacro', 'Halimeda', 'Padina')



for (i in 1:100) {
  
  true_data <- generate_true(humanCover, 12, cover = covers)
  
  for (j in nImages_human) {
    
    sample_data <- sample_human(humanCover, true_data[[2]], sample_n = j, reps = 100, cover = covers)
    relationship_data <- generate_relationships(true_data[[1]], sample_data, cover = covers)
    
    master_human <- rbind(master_human, relationship_data)
    
  }
}

write.csv(master_human, 'master_human_firstsplit_2.19.20.csv', row.names=FALSE)

#GENERATES ALL RELATIONSHIPS
true_data <- generate_true(humanCover, 21, cover = covers)
sample_data <- sample_robot(robotCover, sample_n = 69, reps = 1, cover = covers)
relationship_data <- generate_relationships(true_data[[1]], sample_data, cover=covers)

robotDat <- read.csv('F:\\Moorea\\BertTesting\\Exports\\master_robot_lowrel_4.8.20.csv', header=TRUE)

robotDat <- rbind(robotDat, relationship_data)

write.csv(robotDat, file = 'F:\\Moorea\\BertTesting\\Exports\\master_robot_lowrel_4.8.20.csv', row.names=FALSE)

#Generates plots 

#master_human <- read.csv('master_human_2.4.20.csv', header=TRUE)
#master_human1 <- read.csv('master_human_1.7.20.csv', header=TRUE)
generate_plots(na.omit(master_human), substrates = covers, annotator = 'Human', metrics = 'R_square')


######################
######################
#Generates 'robot' evaluation
######################
######################

master_robot <- data.frame('nImages' = integer(), 'Replicate' = integer(), 'Substrate' = character(), 
                           'R_square' = numeric(), 'Slope' = numeric(), 'Intercept' = numeric())
#nImages_robot <- c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 69)
nImages_robot <- c(1, 2, 4, 8, 16, 32, 64, 69)

for (i in 1:50) {
  
  true_data <- generate_true(humanCover, 12, cover = covers)
  
  for (j in nImages_robot) {
    
    sample_data <- sample_robot(robotCover, sample_n = j, reps = 100, cover = covers)
    relationship_data <- generate_relationships(true_data[[1]], sample_data, cover = covers)
    
    master_robot <- rbind(master_robot, relationship_data)
    
  }
}

write.csv(master_robot, 'master_robot_firstsplit_rep2_2.19.20.csv', row.names=FALSE)

#Generates plots

#master_robot1 <- read.csv('master_robot_1.7.20.csv', header=TRUE)
master_robot <- read.csv('master_robot_2.4.20.csv', header=TRUE)
generate_plots(na.omit(master_robot), annotator = 'Robot', substrates = covers, metrics = 'R_square')

plot_octave(master_human, master_robot, substrates = "Coral", metrics = "Intercept")

master_human <- read.csv('master_human_lowrel_2.19.20.csv', header=TRUE)
master_robot <- read.csv('master_robot_lowrel_2.19.20.csv', header=TRUE)

plot_octave(na.omit(master_human), na.omit(master_robot), substrates = "Coral", metrics = "Intercept")

#############
#############
#True cover test
#############
#############


covers <- c('Coral', 'Algae', 'LowRel', 'HardSub')

data_sheet <- data.frame('SurveyCode' = character(), 'nImages' = integer(), 'Substrate' = character(), 
                         'MeanCover' = numeric(), 'Quantile05' = numeric(), 'Quantile95' = numeric())

for (j in 1:20) {
  
  true_sheet <- data.frame('SurveyCode' = character(), 'Coral' = numeric(), 'Algae' = numeric(), 'LowRel' = numeric(),
                           'HardSub' = numeric(), 'OtherSub' = numeric(), 'Obscured' = numeric())
  
  for (i in 1:100) {
    
    true_data <- generate_true(humanCover, j)
    true_sheet <- rbind(true_sheet, true_data[[1]])
    
  }
  
  for (survey in unique(true_sheet$SurveyCode)) {
    
    temp <- true_sheet %>%
      filter(SurveyCode == survey)
    
    for (cover in covers) {
      
      tempMean <- mean(temp[,cover])
      tempQuant <- quantile(temp[,cover], probs = c(0.025, 0.975))
      
      tempAdd <- data.frame('SurveyCode' = survey, nImages = j, 'Substrate' = cover, 'MeanCover' = tempMean, 
                            'Quantile05' = tempQuant[1], 'Quantile95' = tempQuant[2])
      
      data_sheet <- rbind(data_sheet, tempAdd)
      
    }
  }
}

data_sheet$Range <- data_sheet$Quantile95 - data_sheet$Quantile05

plotDat <- data_sheet %>%
  filter(Substrate == 'HardSub', nImages == 12) 
plotDat <- plotDat[order(plotDat$MeanCover),]
plotDat$plotOrder <- 1:10

dev.new()
par(mfrow = c(1,2))
plot(plotDat$plotOrder, plotDat$MeanCover, ylab = 'Percent Cover Coral', xlab = 'Site', main = 'Hard Substrate - 12 images', ylim = c(0, 50))
arrows(plotDat$plotOrder, plotDat$MeanCover, plotDat$plotOrder, plotDat$Quantile05, angle = 90, length = 0.15)
arrows(plotDat$plotOrder, plotDat$MeanCover, plotDat$plotOrder, plotDat$Quantile95, angle = 90, length = 0.15)

plotDat <- data_sheet %>%
  filter(Substrate == 'HardSub', nImages == 19) 
plotDat <- plotDat[order(plotDat$MeanCover),]
plotDat$plotOrder <- 1:10

plot(plotDat$plotOrder, plotDat$MeanCover, ylab = 'Percent Cover Coral', xlab = 'Site', main = 'Hard Substrate - 19 images', ylim = c(0, 50))
arrows(plotDat$plotOrder, plotDat$MeanCover, plotDat$plotOrder, plotDat$Quantile05, angle = 90, length = 0.15)
arrows(plotDat$plotOrder, plotDat$MeanCover, plotDat$plotOrder, plotDat$Quantile95, angle = 90, length = 0.15)


checkMe <- master_human %>%
  filter(nImages == 8, Substrate == 'Algae')

checkMe1 <- checkMe %>%
  filter(R_square < 0.8)



master_human <- read.csv('G:\\Moorea\\BertTesting\\master_human_12.16.19.csv', header=TRUE)
master_robot <- read.csv('G:\\Moorea\\BertTesting\\master_robot_octave.csv', header=TRUE)

plot_octave(master_human, master_robot)


true_dat1 <- generate_true(humanCover, 12)
true_dat <- true_dat1[[1]]

sample_dat <- sample_human(humanCover, true_dat1[[2]], 1, reps = 10)

generate_relationships <- function(true_dat, sample_dat, cover = c('Coral', 'Algae', 'LowRel', 'HardSub', 'Obscured')) {
  
  #Sorts the true dataset alphabetically
  true_dat <- true_dat %>%
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
      temp_frame <- data.frame('SurveyCode' = true_dat$SurveyCode, 'TrueCover' = true_dat[,substrate], 'SampleCover' = filtered_dat[,substrate])
      
      #Generates a linear model between the 'true' cover and the sample data
      temp_model <- lm(TrueCover ~ SampleCover, dat = temp_frame)
      
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

library(randomcoloR)

true_dat1 <- generate_true(humanCover, 12)
true_dat <- true_dat1[[1]]

sample_dat <- sample_human(humanCover, true_dat1[[2]], 8, reps = 100)
i <- 1

#Filters data based on the replicate #
filtered_dat <- sample_dat %>%
  filter(Replicate == i) %>%
  arrange(SurveyCode)

#Creates a temporary dataframe to hold our two comparison values together
temp_frame <- data.frame('SurveyCode' = true_dat$SurveyCode, 'TrueCover' = true_dat[,substrate], 'SampleCover' = filtered_dat[,substrate])

#Generates a linear model between the 'true' cover and the sample data
temp_model <- lm(TrueCover ~ SampleCover, dat = temp_frame)

#col <- randomColor(1, 'random', 'random')

dev.new()
plot(temp_frame$SampleCover, temp_frame$TrueCover, col = 'black', pch = 16, cex = 1.3, xlab = 'Sample Cover', 
     ylab = '"True" Cover', main = 'Coral - nImages = 8')
segments(0, 0, 100, 100, col = 'red', lty = 2, lwd = 2)
abline(temp_model, col = 'black')

for (i in 2:max(sample_dat$Replicate)) {
  
  #Filters data based on the replicate #
  filtered_dat <- sample_dat %>%
    filter(Replicate == i) %>%
    arrange(SurveyCode)
  
  #Creates a temporary dataframe to hold our two comparison values together
  temp_frame <- data.frame('SurveyCode' = true_dat$SurveyCode, 'TrueCover' = true_dat[,substrate], 'SampleCover' = filtered_dat[,substrate])
    
  #Generates a linear model between the 'true' cover and the sample data
  temp_model <- lm(TrueCover ~ SampleCover, dat = temp_frame)
  
  col <- randomColor(1, 'random', 'random')
  
  points(temp_frame$SampleCover, temp_frame$TrueCover, col = col, pch = 16, cex = 1.3)
  abline(temp_model, col = col)
  
}


##########################
##########################
##########################
##########################
##########################
##########################
#COVER TESTING LOW RELIEF
##########################
##########################
##########################
##########################
##########################
##########################

humanCover <- humanCover %>%
  select(38:45)
humanCover$Annotator <- "human"

robotCover <- robotCover %>%
  select(18:25)
robotCover$Annotator <- "robot"

allCover <- rbind(humanCover, robotCover)

allSummary <- allCover %>%
  group_by(UniqueCode) %>%
  summarize(
    Coral = mean(Coral),
    Algae = mean(Algae),
    LowRel = mean(LowRel),
    HardSub = mean(HardSub),
    Obscured = mean(Obscured)
  )%>%
  mutate(
    CoralStd = Coral / (Coral + Algae + HardSub),
    AlgaeStd = Algae / (Coral + Algae + HardSub),
    HardStd = HardSub / (Coral + Algae + HardSub)
  )
allSummary$Annotator <- "all"

humanSummary <- allCover %>%
  filter(Annotator == "human") %>%
  group_by(UniqueCode) %>%
  summarize(
    Coral = mean(Coral),
    Algae = mean(Algae),
    LowRel = mean(LowRel),
    HardSub = mean(HardSub),
    Obscured = mean(Obscured)
  )%>%
  mutate(
    CoralStd = Coral / (Coral + Algae + HardSub),
    AlgaeStd = Algae / (Coral + Algae + HardSub),
    HardStd = HardSub / (Coral + Algae + HardSub)
  )
humanSummary$Annotator <- "human"

robotSummary <- allCover %>%
  filter(Annotator == "robot") %>%
  group_by(UniqueCode) %>%
  summarize(
    Coral = mean(Coral),
    Algae = mean(Algae),
    LowRel = mean(LowRel),
    HardSub = mean(HardSub),
    Obscured = mean(Obscured)
  )%>%
  mutate(
    CoralStd = Coral / (Coral + Algae + HardSub),
    AlgaeStd = Algae / (Coral + Algae + HardSub),
    HardStd = HardSub / (Coral + Algae + HardSub)
  )
robotSummary$Annotator <- "robot"

comboSummary <- rbind(allSummary, humanSummary, robotSummary)

comboSummary <- comboSummary %>%
  filter(Annotator != 'all')

longSummary <- merge(humanSummary, robotSummary, by = "UniqueCode")
longSummary <- longSummary %>%
  mutate(CoralDiff = Coral.y - Coral.x,
         AlgaeDiff = Algae.y - Algae.x,
         LowRelDiff = LowRel.y - LowRel.x,
         HardSubDiff = HardSub.y - HardSub.x,
         CoralStdDiff = CoralStd.y - CoralStd.x,
         AlgaeStdDiff = AlgaeStd.y - AlgaeStd.x,
         HardStdDiff = HardStd.y - HardStd.x)

coral <- ggplot(data=comboSummary, aes(x=reorder(UniqueCode, -Coral), y=Coral, fill=Annotator)) +
  theme_classic()+ #just a nice simple theme, removes background crap
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+
  theme(legend.title = element_blank()) + ##removes legend title because unneccsary
  theme(axis.text.x = element_blank()) + ## removes site names because clumpy - we can also turn them sideways if you want like 45 degrees to see them all - but if unncessary
  theme(axis.ticks = element_blank())+ ## removes the axis tick marks
  xlab("Site")+
  ylab("Coral Percent Cover\n")+ ## the \n just makes a space after so the words aren't cramping the numbers
  theme(text = element_text(size=12, face = "bold")) + ## you can change all the text this way - for diff size or face - you can do italic, bold italtic etc... if you want to just change specific text would be like... axis.text.x, axis.text.y, axis.text, etc...
  scale_fill_manual(values = c("#79a7d3", "#8a307f"),
                    breaks = c("human", "robot"),
                    labels = c("Human", "Robot")) ## i always use manual because I like to choose specific hex codes - if it was color specified instead of fill specified you would 

coralStd <- ggplot(data=comboSummary, aes(x=reorder(UniqueCode, -CoralStd), y=CoralStd, fill=Annotator)) +
  theme_classic()+ #just a nice simple theme, removes background crap
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+
  theme(legend.title = element_blank()) + ##removes legend title because unneccsary
  theme(axis.text.x = element_blank()) + ## removes site names because clumpy - we can also turn them sideways if you want like 45 degrees to see them all - but if unncessary
  theme(axis.ticks = element_blank())+ ## removes the axis tick marks
  xlab("Site")+
  ylab("Coral Standardized Cover\n")+ ## the \n just makes a space after so the words aren't cramping the numbers
  theme(text = element_text(size=12, face = "bold")) + ## you can change all the text this way - for diff size or face - you can do italic, bold italtic etc... if you want to just change specific text would be like... axis.text.x, axis.text.y, axis.text, etc...
  scale_fill_manual(values = c("#79a7d3", "#8a307f"),
                    breaks = c("human", "robot"),
                    labels = c("Human", "Robot")) ## i always use manual because I like to choose specific hex codes - if it was color specified instead of fill specified you would 

coralDiff <- ggplot(data=longSummary, aes(x=reorder(UniqueCode, -CoralDiff), y=CoralDiff)) +
  theme_classic()+ #just a nice simple theme, removes background crap
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+
  theme(legend.title = element_blank()) + ##removes legend title because unneccsary
  theme(axis.text.x = element_blank()) + ## removes site names because clumpy - we can also turn them sideways if you want like 45 degrees to see them all - but if unncessary
  theme(axis.ticks = element_blank())+ ## removes the axis tick marks
  xlab("Site")+
  ylab("Coral Percent Cover Difference (Robot - Human)\n")+ ## the \n just makes a space after so the words aren't cramping the numbers
  theme(text = element_text(size=12, face = "bold")) + ## you can change all the text this way - for diff size or face - you can do italic, bold italtic etc... if you want to just change specific text would be like... axis.text.x, axis.text.y, axis.text, etc...
  scale_fill_manual(values = c("#79a7d3")) ## i always use manual because I like to choose specific hex codes - if it was color specified instead of fill specified you would 

coralStdDiff <- ggplot(data=longSummary, aes(x=reorder(UniqueCode, -CoralStdDiff), y=CoralStdDiff)) +
  theme_classic()+ #just a nice simple theme, removes background crap
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+
  theme(legend.title = element_blank()) + ##removes legend title because unneccsary
  theme(axis.text.x = element_blank()) + ## removes site names because clumpy - we can also turn them sideways if you want like 45 degrees to see them all - but if unncessary
  theme(axis.ticks = element_blank())+ ## removes the axis tick marks
  xlab("Site")+
  ylab("Coral Standardized Cover Difference (Robot - Human)\n")+ ## the \n just makes a space after so the words aren't cramping the numbers
  theme(text = element_text(size=12, face = "bold")) + ## you can change all the text this way - for diff size or face - you can do italic, bold italtic etc... if you want to just change specific text would be like... axis.text.x, axis.text.y, axis.text, etc...
  scale_fill_manual(values = c("#79a7d3")) ## i always use manual because I like to choose specific hex codes - if it was color specified instead of fill specified you would 

algae <- ggplot(data=comboSummary, aes(x=reorder(UniqueCode, -Algae), y=Algae, fill=Annotator)) +
  theme_classic()+ #just a nice simple theme, removes background crap
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+
  theme(legend.title = element_blank()) + ##removes legend title because unneccsary
  theme(axis.text.x = element_blank()) + ## removes site names because clumpy - we can also turn them sideways if you want like 45 degrees to see them all - but if unncessary
  theme(axis.ticks = element_blank())+ ## removes the axis tick marks
  xlab("Site")+
  ylab("algae Percent Cover\n")+ ## the \n just makes a space after so the words aren't cramping the numbers
  theme(text = element_text(size=12, face = "bold")) + ## you can change all the text this way - for diff size or face - you can do italic, bold italtic etc... if you want to just change specific text would be like... axis.text.x, axis.text.y, axis.text, etc...
  scale_fill_manual(values = c("#79a7d3", "#8a307f"),
                    breaks = c("human", "robot"),
                    labels = c("Human", "Robot")) ## i always use manual because I like to choose specific hex codes - if it was color specified instead of fill specified you would 

algaeStd <- ggplot(data=comboSummary, aes(x=reorder(UniqueCode, -AlgaeStd), y=AlgaeStd, fill=Annotator)) +
  theme_classic()+ #just a nice simple theme, removes background crap
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+
  theme(legend.title = element_blank()) + ##removes legend title because unneccsary
  theme(axis.text.x = element_blank()) + ## removes site names because clumpy - we can also turn them sideways if you want like 45 degrees to see them all - but if unncessary
  theme(axis.ticks = element_blank())+ ## removes the axis tick marks
  xlab("Site")+
  ylab("Algae Standardized Cover\n")+ ## the \n just makes a space after so the words aren't cramping the numbers
  theme(text = element_text(size=12, face = "bold")) + ## you can change all the text this way - for diff size or face - you can do italic, bold italtic etc... if you want to just change specific text would be like... axis.text.x, axis.text.y, axis.text, etc...
  scale_fill_manual(values = c("#79a7d3", "#8a307f"),
                    breaks = c("human", "robot"),
                    labels = c("Human", "Robot")) ## i always use manual because I like to choose specific hex codes - if it was color specified instead of fill specified you would 

algaeDiff <- ggplot(data=longSummary, aes(x=reorder(UniqueCode, -AlgaeDiff), y=AlgaeDiff)) +
  theme_classic()+ #just a nice simple theme, removes background crap
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+
  theme(legend.title = element_blank()) + ##removes legend title because unneccsary
  theme(axis.text.x = element_blank()) + ## removes site names because clumpy - we can also turn them sideways if you want like 45 degrees to see them all - but if unncessary
  theme(axis.ticks = element_blank())+ ## removes the axis tick marks
  xlab("Site")+
  ylab("Algae Percent Cover Difference (Robot - Human)\n")+ ## the \n just makes a space after so the words aren't cramping the numbers
  theme(text = element_text(size=12, face = "bold")) + ## you can change all the text this way - for diff size or face - you can do italic, bold italtic etc... if you want to just change specific text would be like... axis.text.x, axis.text.y, axis.text, etc...
  scale_fill_manual(values = c("#79a7d3")) ## i always use manual because I like to choose specific hex codes - if it was color specified instead of fill specified you would 

algaeStdDiff <- ggplot(data=longSummary, aes(x=reorder(UniqueCode, -AlgaeStdDiff), y=AlgaeStdDiff)) +
  theme_classic()+ #just a nice simple theme, removes background crap
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+
  theme(legend.title = element_blank()) + ##removes legend title because unneccsary
  theme(axis.text.x = element_blank()) + ## removes site names because clumpy - we can also turn them sideways if you want like 45 degrees to see them all - but if unncessary
  theme(axis.ticks = element_blank())+ ## removes the axis tick marks
  xlab("Site")+
  ylab("Algae Standardized Cover Difference (Robot - Human)\n")+ ## the \n just makes a space after so the words aren't cramping the numbers
  theme(text = element_text(size=12, face = "bold")) + ## you can change all the text this way - for diff size or face - you can do italic, bold italtic etc... if you want to just change specific text would be like... axis.text.x, axis.text.y, axis.text, etc...
  scale_fill_manual(values = c("#79a7d3")) ## i always use manual because I like to choose specific hex codes - if it was color specified instead of fill specified you would 

lowrel <- ggplot(data=comboSummary, aes(x=reorder(UniqueCode, -LowRel), y=LowRel, fill=Annotator)) +
  theme_classic()+ #just a nice simple theme, removes background crap
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+
  theme(legend.title = element_blank()) + ##removes legend title because unneccsary
  theme(axis.text.x = element_blank()) + ## removes site names because clumpy - we can also turn them sideways if you want like 45 degrees to see them all - but if unncessary
  theme(axis.ticks = element_blank())+ ## removes the axis tick marks
  xlab("Site")+
  ylab("Low Relief Percent Cover\n")+ ## the \n just makes a space after so the words aren't cramping the numbers
  theme(text = element_text(size=12, face = "bold")) + ## you can change all the text this way - for diff size or face - you can do italic, bold italtic etc... if you want to just change specific text would be like... axis.text.x, axis.text.y, axis.text, etc...
  scale_fill_manual(values = c("#79a7d3", "#8a307f"),
                    breaks = c("human", "robot"),
                    labels = c("Human", "Robot")) ## i always use manual because I like to choose specific hex codes - if it was color specified instead of fill specified you would 

lowrelDiff <- ggplot(data=longSummary, aes(x=reorder(UniqueCode, -LowRelDiff), y=LowRelDiff)) +
  theme_classic()+ #just a nice simple theme, removes background crap
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+
  theme(legend.title = element_blank()) + ##removes legend title because unneccsary
  theme(axis.text.x = element_blank()) + ## removes site names because clumpy - we can also turn them sideways if you want like 45 degrees to see them all - but if unncessary
  theme(axis.ticks = element_blank())+ ## removes the axis tick marks
  xlab("Site")+
  ylab("Low Relief Percent Cover Difference (Robot - Human)\n")+ ## the \n just makes a space after so the words aren't cramping the numbers
  theme(text = element_text(size=12, face = "bold")) + ## you can change all the text this way - for diff size or face - you can do italic, bold italtic etc... if you want to just change specific text would be like... axis.text.x, axis.text.y, axis.text, etc...
  scale_fill_manual(values = c("#79a7d3")) ## i always use manual because I like to choose specific hex codes - if it was color specified instead of fill specified you would 

hardsub <- ggplot(data=comboSummary, aes(x=reorder(UniqueCode, -HardSub), y=HardSub, fill=Annotator)) +
  theme_classic()+ #just a nice simple theme, removes background crap
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+
  theme(legend.title = element_blank()) + ##removes legend title because unneccsary
  theme(axis.text.x = element_blank()) + ## removes site names because clumpy - we can also turn them sideways if you want like 45 degrees to see them all - but if unncessary
  theme(axis.ticks = element_blank())+ ## removes the axis tick marks
  xlab("Site")+
  ylab("Hard Substrate Percent Cover\n")+ ## the \n just makes a space after so the words aren't cramping the numbers
  theme(text = element_text(size=12, face = "bold")) + ## you can change all the text this way - for diff size or face - you can do italic, bold italtic etc... if you want to just change specific text would be like... axis.text.x, axis.text.y, axis.text, etc...
  scale_fill_manual(values = c("#79a7d3", "#8a307f"),
                    breaks = c("human", "robot"),
                    labels = c("Human", "Robot")) ## i always use manual because I like to choose specific hex codes - if it was color specified instead of fill specified you would 

hardStd <- ggplot(data=comboSummary, aes(x=reorder(UniqueCode, -HardStd), y=HardStd, fill=Annotator)) +
  theme_classic()+ #just a nice simple theme, removes background crap
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+
  theme(legend.title = element_blank()) + ##removes legend title because unneccsary
  theme(axis.text.x = element_blank()) + ## removes site names because clumpy - we can also turn them sideways if you want like 45 degrees to see them all - but if unncessary
  theme(axis.ticks = element_blank())+ ## removes the axis tick marks
  xlab("Site")+
  ylab("Hard Substrate Standardized Cover\n")+ ## the \n just makes a space after so the words aren't cramping the numbers
  theme(text = element_text(size=12, face = "bold")) + ## you can change all the text this way - for diff size or face - you can do italic, bold italtic etc... if you want to just change specific text would be like... axis.text.x, axis.text.y, axis.text, etc...
  scale_fill_manual(values = c("#79a7d3", "#8a307f"),
                    breaks = c("human", "robot"),
                    labels = c("Human", "Robot")) ## i always use manual because I like to choose specific hex codes - if it was color specified instead of fill specified you would 

hardsubDiff <- ggplot(data=longSummary, aes(x=reorder(UniqueCode, -HardSubDiff), y=HardSubDiff)) +
  theme_classic()+ #just a nice simple theme, removes background crap
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+
  theme(legend.title = element_blank()) + ##removes legend title because unneccsary
  theme(axis.text.x = element_blank()) + ## removes site names because clumpy - we can also turn them sideways if you want like 45 degrees to see them all - but if unncessary
  theme(axis.ticks = element_blank())+ ## removes the axis tick marks
  xlab("Site")+
  ylab("Hard Substrate Percent Cover Difference (Robot - Human)\n")+ ## the \n just makes a space after so the words aren't cramping the numbers
  theme(text = element_text(size=12, face = "bold")) + ## you can change all the text this way - for diff size or face - you can do italic, bold italtic etc... if you want to just change specific text would be like... axis.text.x, axis.text.y, axis.text, etc...
  scale_fill_manual(values = c("#79a7d3")) ## i always use manual because I like to choose specific hex codes - if it was color specified instead of fill specified you would 

hardStdDiff <- ggplot(data=longSummary, aes(x=reorder(UniqueCode, -HardStdDiff), y=HardStdDiff)) +
  theme_classic()+ #just a nice simple theme, removes background crap
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+
  theme(legend.title = element_blank()) + ##removes legend title because unneccsary
  theme(axis.text.x = element_blank()) + ## removes site names because clumpy - we can also turn them sideways if you want like 45 degrees to see them all - but if unncessary
  theme(axis.ticks = element_blank())+ ## removes the axis tick marks
  xlab("Site")+
  ylab("Hard Substrate Standardized Cover Difference (Robot - Human)\n")+ ## the \n just makes a space after so the words aren't cramping the numbers
  theme(text = element_text(size=12, face = "bold")) + ## you can change all the text this way - for diff size or face - you can do italic, bold italtic etc... if you want to just change specific text would be like... axis.text.x, axis.text.y, axis.text, etc...
  scale_fill_manual(values = c("#79a7d3")) ## i always use manual because I like to choose specific hex codes - if it was color specified instead of fill specified you would 

ggsave('F:\\Moorea\\BertTesting\\Output\\LowRel\\Covers\\coral.jpg', coral, device = 'jpg')
ggsave('F:\\Moorea\\BertTesting\\Output\\LowRel\\Covers\\algae.jpg', algae, device = 'jpg')
ggsave('F:\\Moorea\\BertTesting\\Output\\LowRel\\Covers\\lowrel.jpg', lowrel, device = 'jpg')
ggsave('F:\\Moorea\\BertTesting\\Output\\LowRel\\Covers\\hardsub.jpg', hardsub, device = 'jpg')

ggsave('F:\\Moorea\\BertTesting\\Output\\LowRel\\Covers\\coralDiff.jpg', coralDiff, device = 'jpg')
ggsave('F:\\Moorea\\BertTesting\\Output\\LowRel\\Covers\\algaeDiff.jpg', algaeDiff, device = 'jpg')
ggsave('F:\\Moorea\\BertTesting\\Output\\LowRel\\Covers\\lowrelDiff.jpg', lowrelDiff, device = 'jpg')
ggsave('F:\\Moorea\\BertTesting\\Output\\LowRel\\Covers\\hardsubDiff.jpg', hardsubDiff, device = 'jpg')

#Standardized
ggsave('F:\\Moorea\\BertTesting\\Output\\LowRel\\Standardized\\coralStd.jpg', coralStd, device = 'jpg')
ggsave('F:\\Moorea\\BertTesting\\Output\\LowRel\\Standardized\\algaeStd.jpg', algaeStd, device = 'jpg')
ggsave('F:\\Moorea\\BertTesting\\Output\\LowRel\\Standardized\\hardsubStd.jpg', hardStd, device = 'jpg')

ggsave('F:\\Moorea\\BertTesting\\Output\\LowRel\\Standardized\\coralStdDiff.jpg', coralStdDiff, device = 'jpg')
ggsave('F:\\Moorea\\BertTesting\\Output\\LowRel\\Standardized\\algaeStdDiff.jpg', algaeStdDiff, device = 'jpg')
ggsave('F:\\Moorea\\BertTesting\\Output\\LowRel\\Standardized\\hardsubStdDiff.jpg', hardStdDiff, device = 'jpg')

##########################
##########################
##########################
##########################
##########################
##########################
#COVER TESTING Soft Substrate
##########################
##########################
##########################
##########################
##########################
##########################

humanCover <- humanCover %>%
  select(38:45)
humanCover$Annotator <- "human"

robotCover <- robotCover %>%
  select(18:25)
robotCover$Annotator <- "robot"

allCover <- rbind(humanCover, robotCover)

allSummary <- allCover %>%
  group_by(UniqueCode) %>%
  summarize(
    Coral = mean(Coral),
    Algae = mean(Algae),
    SoftSub = mean(SoftSub),
    HardSub = mean(HardSub),
    Obscured = mean(Obscured)
  ) %>%
  mutate(
    CoralStd = Coral / (Coral + Algae + HardSub),
    AlgaeStd = Algae / (Coral + Algae + HardSub),
    HardStd = HardSub / (Coral + Algae + HardSub)
  )
allSummary$Annotator <- "all"

humanSummary <- allCover %>%
  filter(Annotator == "human") %>%
  group_by(UniqueCode) %>%
  summarize(
    Coral = mean(Coral),
    Algae = mean(Algae),
    SoftSub = mean(SoftSub),
    HardSub = mean(HardSub),
    Obscured = mean(Obscured)
  ) %>%
  mutate(
    CoralStd = Coral / (Coral + Algae + HardSub),
    AlgaeStd = Algae / (Coral + Algae + HardSub),
    HardStd = HardSub / (Coral + Algae + HardSub)
  )
humanSummary$Annotator <- "human"

robotSummary <- allCover %>%
  filter(Annotator == "robot") %>%
  group_by(UniqueCode) %>%
  summarize(
    Coral = mean(Coral),
    Algae = mean(Algae),
    SoftSub = mean(SoftSub),
    HardSub = mean(HardSub),
    Obscured = mean(Obscured)
  ) %>%
  mutate(
    CoralStd = Coral / (Coral + Algae + HardSub),
    AlgaeStd = Algae / (Coral + Algae + HardSub),
    HardStd = HardSub / (Coral + Algae + HardSub)
  )
robotSummary$Annotator <- "robot"

comboSummary <- rbind(allSummary, humanSummary, robotSummary)

comboSummary <- comboSummary %>%
  filter(Annotator != 'all')

longSummary <- merge(humanSummary, robotSummary, by = "UniqueCode")
longSummary <- longSummary %>%
  mutate(CoralDiff = Coral.y - Coral.x,
         AlgaeDiff = Algae.y - Algae.x,
         SoftSubDiff = SoftSub.y - SoftSub.x,
         HardSubDiff = HardSub.y - HardSub.x,
         CoralStdDiff = CoralStd.y - CoralStd.x,
         AlgaeStdDiff = AlgaeStd.y - AlgaeStd.x)

coral <- ggplot(data=comboSummary, aes(x=reorder(UniqueCode, -Coral), y=Coral, fill=Annotator)) +
  theme_classic()+ #just a nice simple theme, removes background crap
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+
  theme(legend.title = element_blank()) + ##removes legend title because unneccsary
  theme(axis.text.x = element_blank()) + ## removes site names because clumpy - we can also turn them sideways if you want like 45 degrees to see them all - but if unncessary
  theme(axis.ticks = element_blank())+ ## removes the axis tick marks
  xlab("Site")+
  ylab("Coral Percent Cover\n")+ ## the \n just makes a space after so the words aren't cramping the numbers
  theme(text = element_text(size=12, face = "bold")) + ## you can change all the text this way - for diff size or face - you can do italic, bold italtic etc... if you want to just change specific text would be like... axis.text.x, axis.text.y, axis.text, etc...
  scale_fill_manual(values = c("#79a7d3", "#8a307f"),
                    breaks = c("human", "robot"),
                    labels = c("Human", "Robot")) ## i always use manual because I like to choose specific hex codes - if it was color specified instead of fill specified you would 

coralDiff <- ggplot(data=longSummary, aes(x=reorder(UniqueCode, -CoralDiff), y=CoralDiff)) +
  theme_classic()+ #just a nice simple theme, removes background crap
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+
  theme(legend.title = element_blank()) + ##removes legend title because unneccsary
  theme(axis.text.x = element_blank()) + ## removes site names because clumpy - we can also turn them sideways if you want like 45 degrees to see them all - but if unncessary
  theme(axis.ticks = element_blank())+ ## removes the axis tick marks
  xlab("Site")+
  ylab("Coral Percent Cover Difference (Robot - Human)\n")+ ## the \n just makes a space after so the words aren't cramping the numbers
  theme(text = element_text(size=12, face = "bold")) + ## you can change all the text this way - for diff size or face - you can do italic, bold italtic etc... if you want to just change specific text would be like... axis.text.x, axis.text.y, axis.text, etc...
  scale_fill_manual(values = c("#79a7d3")) ## i always use manual because I like to choose specific hex codes - if it was color specified instead of fill specified you would 

algae <- ggplot(data=comboSummary, aes(x=reorder(UniqueCode, -Algae), y=Algae, fill=Annotator)) +
  theme_classic()+ #just a nice simple theme, removes background crap
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+
  theme(legend.title = element_blank()) + ##removes legend title because unneccsary
  theme(axis.text.x = element_blank()) + ## removes site names because clumpy - we can also turn them sideways if you want like 45 degrees to see them all - but if unncessary
  theme(axis.ticks = element_blank())+ ## removes the axis tick marks
  xlab("Site")+
  ylab("algae Percent Cover\n")+ ## the \n just makes a space after so the words aren't cramping the numbers
  theme(text = element_text(size=12, face = "bold")) + ## you can change all the text this way - for diff size or face - you can do italic, bold italtic etc... if you want to just change specific text would be like... axis.text.x, axis.text.y, axis.text, etc...
  scale_fill_manual(values = c("#79a7d3", "#8a307f"),
                    breaks = c("human", "robot"),
                    labels = c("Human", "Robot")) ## i always use manual because I like to choose specific hex codes - if it was color specified instead of fill specified you would 

algaeDiff <- ggplot(data=longSummary, aes(x=reorder(UniqueCode, -AlgaeDiff), y=AlgaeDiff)) +
  theme_classic()+ #just a nice simple theme, removes background crap
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+
  theme(legend.title = element_blank()) + ##removes legend title because unneccsary
  theme(axis.text.x = element_blank()) + ## removes site names because clumpy - we can also turn them sideways if you want like 45 degrees to see them all - but if unncessary
  theme(axis.ticks = element_blank())+ ## removes the axis tick marks
  xlab("Site")+
  ylab("Algae Percent Cover Difference (Robot - Human)\n")+ ## the \n just makes a space after so the words aren't cramping the numbers
  theme(text = element_text(size=12, face = "bold")) + ## you can change all the text this way - for diff size or face - you can do italic, bold italtic etc... if you want to just change specific text would be like... axis.text.x, axis.text.y, axis.text, etc...
  scale_fill_manual(values = c("#79a7d3")) ## i always use manual because I like to choose specific hex codes - if it was color specified instead of fill specified you would 


softsub <- ggplot(data=comboSummary, aes(x=reorder(UniqueCode, -SoftSub), y=SoftSub, fill=Annotator)) +
  theme_classic()+ #just a nice simple theme, removes background crap
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+
  theme(legend.title = element_blank()) + ##removes legend title because unneccsary
  theme(axis.text.x = element_blank()) + ## removes site names because clumpy - we can also turn them sideways if you want like 45 degrees to see them all - but if unncessary
  theme(axis.ticks = element_blank())+ ## removes the axis tick marks
  xlab("Site")+
  ylab("Soft Substrate Percent Cover\n")+ ## the \n just makes a space after so the words aren't cramping the numbers
  theme(text = element_text(size=12, face = "bold")) + ## you can change all the text this way - for diff size or face - you can do italic, bold italtic etc... if you want to just change specific text would be like... axis.text.x, axis.text.y, axis.text, etc...
  scale_fill_manual(values = c("#79a7d3", "#8a307f"),
                    breaks = c("human", "robot"),
                    labels = c("Human", "Robot")) ## i always use manual because I like to choose specific hex codes - if it was color specified instead of fill specified you would 

softsubDiff <- ggplot(data=longSummary, aes(x=reorder(UniqueCode, -SoftSubDiff), y=SoftSubDiff)) +
  theme_classic()+ #just a nice simple theme, removes background crap
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+
  theme(legend.title = element_blank()) + ##removes legend title because unneccsary
  theme(axis.text.x = element_blank()) + ## removes site names because clumpy - we can also turn them sideways if you want like 45 degrees to see them all - but if unncessary
  theme(axis.ticks = element_blank())+ ## removes the axis tick marks
  xlab("Site")+
  ylab("Soft Substrate Percent Cover Difference (Robot - Human)\n")+ ## the \n just makes a space after so the words aren't cramping the numbers
  theme(text = element_text(size=12, face = "bold")) + ## you can change all the text this way - for diff size or face - you can do italic, bold italtic etc... if you want to just change specific text would be like... axis.text.x, axis.text.y, axis.text, etc...
  scale_fill_manual(values = c("#79a7d3")) ## i always use manual because I like to choose specific hex codes - if it was color specified instead of fill specified you would 


hardsub <- ggplot(data=comboSummary, aes(x=reorder(UniqueCode, -HardSub), y=HardSub, fill=Annotator)) +
  theme_classic()+ #just a nice simple theme, removes background crap
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+
  theme(legend.title = element_blank()) + ##removes legend title because unneccsary
  theme(axis.text.x = element_blank()) + ## removes site names because clumpy - we can also turn them sideways if you want like 45 degrees to see them all - but if unncessary
  theme(axis.ticks = element_blank())+ ## removes the axis tick marks
  xlab("Site")+
  ylab("Hard Substrate Percent Cover\n")+ ## the \n just makes a space after so the words aren't cramping the numbers
  theme(text = element_text(size=12, face = "bold")) + ## you can change all the text this way - for diff size or face - you can do italic, bold italtic etc... if you want to just change specific text would be like... axis.text.x, axis.text.y, axis.text, etc...
  scale_fill_manual(values = c("#79a7d3", "#8a307f"),
                    breaks = c("human", "robot"),
                    labels = c("Human", "Robot")) ## i always use manual because I like to choose specific hex codes - if it was color specified instead of fill specified you would 

hardsubDiff <- ggplot(data=longSummary, aes(x=reorder(UniqueCode, -HardSubDiff), y=HardSubDiff)) +
  theme_classic()+ #just a nice simple theme, removes background crap
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+
  theme(legend.title = element_blank()) + ##removes legend title because unneccsary
  theme(axis.text.x = element_blank()) + ## removes site names because clumpy - we can also turn them sideways if you want like 45 degrees to see them all - but if unncessary
  theme(axis.ticks = element_blank())+ ## removes the axis tick marks
  xlab("Site")+
  ylab("Hard Substrate Percent Cover Difference (Robot - Human)\n")+ ## the \n just makes a space after so the words aren't cramping the numbers
  theme(text = element_text(size=12, face = "bold")) + ## you can change all the text this way - for diff size or face - you can do italic, bold italtic etc... if you want to just change specific text would be like... axis.text.x, axis.text.y, axis.text, etc...
  scale_fill_manual(values = c("#79a7d3")) ## i always use manual because I like to choose specific hex codes - if it was color specified instead of fill specified you would 

ggsave('F:\\Moorea\\BertTesting\\Output\\SoftSub\\Covers\\coral.jpg', coral, device = 'jpg')
ggsave('F:\\Moorea\\BertTesting\\Output\\SoftSub\\Covers\\algae.jpg', algae, device = 'jpg')
ggsave('F:\\Moorea\\BertTesting\\Output\\SoftSub\\Covers\\softsub.jpg', softsub, device = 'jpg')
ggsave('F:\\Moorea\\BertTesting\\Output\\SoftSub\\Covers\\hardsub.jpg', hardsub, device = 'jpg')

ggsave('F:\\Moorea\\BertTesting\\Output\\SoftSub\\Covers\\coralDiff.jpg', coralDiff, device = 'jpg')
ggsave('F:\\Moorea\\BertTesting\\Output\\SoftSub\\Covers\\algaeDiff.jpg', algaeDiff, device = 'jpg')
ggsave('F:\\Moorea\\BertTesting\\Output\\SoftSub\\Covers\\softsubDiff.jpg', softsubDiff, device = 'jpg')
ggsave('F:\\Moorea\\BertTesting\\Output\\SoftSub\\Covers\\hardsubDiff.jpg', hardsubDiff, device = 'jpg')



############
############
#ALL HUMAN VS ALL ROBOT REGRESSION PLOTS
############
############
coral.test <- lm(Coral.y ~ Coral.x, data = longSummary)

plot(longSummary$Coral.x, longSummary$Coral.y, pch = 16, cex = 1.5, 
     main = "Coral", xlab = "Human-scored coral cover", ylab = "Robot-scored coral cover", xlim = c(0,35), ylim = c(0,35))
segments(0,0,40,40, lty = 2, col = 'red', lwd = 1.5)
abline(coral.test, lty = 1, col = 'black', lwd = 1.5)


algae.test <- lm(Algae.y ~ Algae.x, data = longSummary)

plot(longSummary$Algae.x, longSummary$Algae.y, pch = 16, cex = 1.5, 
     main = "Algae", xlab = "Human-scored algal cover", ylab = "Robot-scored algal cover", xlim = c(0,50), ylim = c(0,50))
segments(0,0,40,40, lty = 2, col = 'red', lwd = 1.5)
abline(algae.test, lty = 1, col = 'black', lwd = 1.5)

soft.test <- lm(SoftSub.y ~ SoftSub.x, data = longSummary)

plot(longSummary$SoftSub.x, longSummary$SoftSub.y, pch = 16, cex = 1.5, 
     main = "Soft Substrate", xlab = "Human-scored soft substrate cover", ylab = "Robot-scored soft substrate cover", xlim = c(0,100), ylim = c(0,100))
segments(0,0,100,100, lty = 2, col = 'red', lwd = 1.5)
abline(soft.test, lty = 1, col = 'black', lwd = 1.5)

hard.test <- lm(HardSub.y ~ HardSub.x, data = longSummary)

plot(longSummary$HardSub.x, longSummary$HardSub.y, pch = 16, cex = 1.5, 
     main = "Hard Substrate", xlab = "Human-scored hard substrate cover", ylab = "Robot-scored hard substrate cover", xlim = c(0,80), ylim = c(0,80))
segments(0,0,100,100, lty = 2, col = 'red', lwd = 1.5)
abline(hard.test, lty = 1, col = 'black', lwd = 1.5)