# Loads in dependencies
library(plotKML)
library(geosphere)

timeConvert <- function(year, month, day, startHour, startMin, endHour, endMin) {
  
  #This function takes information on the start time and end time of a transect and converts these values into the 
  #POSIXt format which is a variable format that stores dates/times.  This allows us to make direct numerical operations
  #on the time.  The output is a list of length 2 that contains the startTime and the endTime in POSIXt
  #Written by Scott Miller
  
  temp <- paste(year, "-", 
                formatC(month, width=2, format="d", flag="0"), "-", 
                formatC(day, width=2, format="d", flag="0"), " ", 
                formatC(startHour, width=2, format="d", flag="0"), ":", 
                formatC(startMin, width=2, format="d", flag="0"), ":", "00", sep="")
  startTime <- strptime(temp, "%Y-%m-%d %H:%M:%S")
  
  temp <- paste(year, "-", 
                formatC(month, width=2, format="d", flag="0"), "-", 
                formatC(day, width=2, format="d", flag="0"), " ", 
                formatC(endHour, width=2, format="d", flag="0"), ":", 
                formatC(endMin, width=2, format="d", flag="0"), ":", "00", sep="")
  endTime <- strptime(temp, "%Y-%m-%d %H:%M:%S")
  
  output <- list(startTime, endTime)
  
  return(output)
  
}

extractTrack <- function(file, code, timeList, timeZoneOffset = -10) {
  #This function takes a GPX file and a list of length 2 that contains start and end times of a transect (generated from the timeConvert() function)
  #The code argument is a user-defined code (oftentimes the survey code) that they want to be associated with the GPS route
  #This then extracts the route information from the GPX file in between the times in timeList
  #timeZoneOffset is a value that determines how far off (in hours) from GMT time the transect (NOTE: this is because our GPS saves in GMT, rather than local time)
  #NOTE: this only works if the GPX file has only one element in the tracks dataframe.
  #This means the track was cleared immediately prior to the survey and saved immediately after.  If the GPS was turned off between clearing and saving the track, the track is split into two dataframes and this needs to be manually adjusted.
  #Written by Scott Miller
  
  # Dependencies: package "plotKML" - for readGPX()
  
  #Generates empty vectors
  index <- c()
  latitude <- c()
  longitude <- c()
  time <- c()
    
  #Reads the GPX file and pulls out the route information from it
  route <- readGPX(file)
  location <- route$tracks[[1]][[1]]
    
  index <- rep(code, dim(location)[1]) #Generates a vector repeating "code" to be used as an identifier for the user later
  
  #Saves the coordinates and time in the respective temporary vectors
  latitude <- location$lat
  longitude <- location$lon
  time <- location$time
  time <- as.POSIXlt(time, format="%Y-%m-%dT%H:%M:%S")
  time <- time + (timeZoneOffset*60*60) #Adjustment to change the time into Moorea time rather than the default time (10 hours * 60 minutes * 60 seconds)
  
  #Combines the data into a dataframe
  route <- data.frame(index, latitude, longitude, time)
  
  #Extracts the relevant section of the track based on timeList
  extracted_route <- route[route$time >= timeList[[1]] & route$time <= timeList[[2]],]
  
  return(extracted_route)
  
}

timeMaker <- function(year, month, day, hour, min, sec) {
  #This is just a function that takes individual date and time data and converts it into POSIXt format
  #Used in the generateMetadata() function
  #Written by Scott Miller
  
  temp <- paste(year, "-", 
                formatC(month, width=2, format="d", flag="0"), "-", 
                formatC(day, width=2, format="d", flag="0"), " ", 
                formatC(hour, width=2, format="d", flag="0"), ":", 
                formatC(min, width=2, format="d", flag="0"), ":",
                formatC(sec, width=2, format="d", flag="0"), sep="")
  temp <- strptime(temp, "%Y-%m-%d %H:%M:%S")
  
  return(temp)
  
}
  
extractBenthicTrack <- function(file, code, timeZoneOffset = -10) {
  #Extracts GPS track associated with benthic data
  #This is similar to extractTrack, but this does not filter based on timeList - it returns the entire track
  #Written by Scott Miller
  
  index <- c()
  latitude <- c()
  longitude <- c()
  time <- c()
  
  route <- readGPX(file)
  location <- route$tracks[[1]][[1]]
  
  index <- rep(code, dim(location)[1])
  latitude <- location$lat
  longitude <- location$lon
  time <- location$time
  time <- as.POSIXlt(time, format="%Y-%m-%dT%H:%M:%S")
  time <- time + (timeZoneOffset*60*60)
  
  route <- data.frame(index, latitude, longitude, time)
  
  return(route)
  
}

generateMetadata <- function(files, year, month, day, hour, min, sec, code, track, trackPath = "", backwards = FALSE, timeInt = (2/3)) {
  #This function generates image metadata (times and coordinates) for benthic images
  #It assigns each image a time assuming a "timeInt" time lapse interval from the time in the function and a "year", "month", etc. start time
  #It then uses this to associate each image with its closest GPS point from the track and assign these coordinates to the image
  #Returns a dataframe with the filename, time, and lat/lon coordinates
  #"code" is the value assigned to each photo
  #"track" is the name of the GPX track
  #trackPath is the pathway to where the track is saved
  #In some cases, there were no clear views of the watchface in the beginning of the transect, but there was one from the end.  
  #If backwards = FALSE, it assumes the watch was filmed in the beginning of the transect.  If backwards = TRUE, then it operates backwards from the end
  #Written by Scott Miller
  
  initialTime <- timeMaker(year, month, day, hour, min, sec)
  runningTime <- initialTime
  
  #This if/else statement changes the order in which the times are generated.  The default is forwards (backwards == FALSE).
  #However, in some cases the watch was only filmed at the end of a transect, rather than the beginning, so the time needs to work itself backwards
  #These operate similarly except that the time starts at the end and time is subtracted, rather than starting at the beginning and being added
  if (backwards == TRUE) {
    
    k <- 0:(length(files)-1) * - timeInt
    runningTime <- initialTime + k
    
    ##Associates each image with a time based on the start watch
    #Time needs to be reversed because it is backwards
    metadata <- data.frame("Filename" = files, "Time" = rev(runningTime))
    
  } else {
    
    k <- 0:(length(files)-1) * timeInt
    runningTime <- initialTime + k
    
    ##Associates each image with a time based on the start watch
    metadata <- data.frame("Filename" = files, "Time" = runningTime)
    
  }
  
  ###Step 2: Extracts the GPS data and associates each image with a lat/lon based on its time
  
  #Makes a length 2 list of the first and last time assigned to an image, which is fed into extractBenthicTrack
  times <- list(metadata[1,2], metadata[nrow(metadata),2])
  routes <- extractBenthicTrack(paste(trackPath, track, sep = ""), code)
  
  latitude <- c()
  longitude <- c()
  
  for (i in 1:nrow(metadata)) {
    
    latitude <- c(latitude, routes$latitude[which.min(abs(routes$time - metadata[i,2]))])
    longitude <- c(longitude, routes$longitude[which.min(abs(routes$time - metadata[i,2]))])
    
  }
  
  metadata <- cbind(metadata, latitude, longitude)
  
  return(metadata)
  
}

batchRename <- function(gopro, parent, folder, rootPath = "G:\\Moorea\\GoPros\\Organized\\") {
  #This is a function that automatically renames files contained in subfolders
  #This is used in the workflow to make every filename unique, because the default names are repeated in the dataset
  #Adds the name of the folder that the files are contained in to the front of the filename
  #E.g., "G0016727.jpg" stored in the "SM_July_01_01" folder becomes "SM_July_01_01_G0016727.jpg"
  #Written by Scott Miller
  
  #Creates the pathway based on the information provided and sets working directory to this
  pathway <- paste(rootPath, gopro, "\\", parent, "\\", folder, sep="")
  setwd(pathway)
  
  #Generates a vector of filenames contained in that folder
  files <- dir(path = pathway)
  
  #Renames each file in the folder by adding the folder name and "_" in front of the existing name
  file.rename(files, paste(folder, "_", files, sep = ""))
  
  return(pathway)
}

generateVertices <- function(beg, fin, width = 5) {
  #This function takes a matrix of beginning coordinates (representing the beginning center of a fish count minute) and
  #a corresponding matrix of ending coordinates and generates vertices based on the width of the swath for a rectangular polygon
  #beg is a 2 column matrix of coordinates (lon, lat) from the beginning of fish count minutes
  #fin is a 2 column matrix of coordinates (lon, lat) from the end of fish count minutes
  #width is the width of the swath in meters
  #Returns a dataframe of coordinates that can later be converted into polygons or spatialPolygons
  #Written by Scott Miller
  
  
  begFin <- bearing(beg, fin)
  finBeg <- bearing(fin, beg)
  
  beg1 <- destPoint(beg, begFin + 90, width / 2)
  beg2 <- destPoint(beg, begFin - 90, width / 2)
  
  fin1 <- destPoint(fin, finBeg + 90, width / 2)
  fin2 <- destPoint(fin, finBeg - 90, width / 2)
  
  output <- data.frame(begLon1 = beg1[,1], begLat1 = beg1[,2], endLon1 = fin1[,1], endLat1 = fin1[,2],
                       begLon2 = beg2[,1], begLat2 = beg2[,2], endLon2 = fin2[,1], endLat2 = fin2[,2])
  
  return(output)
}
