##########################################################################
# Author: Frank Witmer
#
# 20 Dec 2018
#	 read weather observations from http://data.rcc-acis.org
#   calculate the longest consecutive snowfall streak
#
##########################################################################

packages <- c("httr") # depends on 'curl'
for (package in packages) {
  if (!(package %in% installed.packages())) { 
    install.packages(package) 
  } 
}


######################
# function to retrieve ACIS station data and return it as a data frame
######################
getACIS_StnData <- function(citySID, cityName) {
  require(httr)
  req <- GET("http://data.rcc-acis.org/StnData", 
    query = list(sid=citySID, sdate="por", edate="por", elems="1,2,4,10,11", output="csv"))
  #str(req)
  colnms <- c("Date", "MaxTemperature", "MinTemperature", "Precipitation", "Snowfall", "SnowDepth")
    
  txt <- content(req, as="text")
  #txt <- content(req, as="text", type="text/csv")
  #str(txt)
  txtLines <- unlist(strsplit(txt, "\n"))
#  str(txtLines)
#  head(txtLines[-1])
  dailyObs <- read.table(text = txtLines[-1], sep=",", col.names=colnms, stringsAsFactors = FALSE)
#  str(dailyObs)
#  head(dailyObs)
  print(paste("Retrieved", nrow(dailyObs), "observations for", cityName))
  dailyObs
}

######################
# function to print snowfall streak information for the given daily observations
######################
printSnowfallStreaks <- function(dailyObs, cityName) {
  # remove trace and missing values
  dailyObs[(dailyObs$Snowfall == " T" | dailyObs$Snowfall == "T"), "Snowfall"] <- "0.0"
  dailyObs[(dailyObs$Snowfall == " M" | dailyObs$Snowfall == "M"), "Snowfall"] <- "0.0"
  
  dailyObs$SnowBinary <- as.numeric(dailyObs$Snowfall) > 0
  
  #tmp <- fix(dailyObs)
  
  # modify code from here to find consecutive snowfall streaks
  #   https://masterr.org/r/how-to-find-consecutive-repeats-in-r/
  runs <- rle(dailyObs$SnowBinary)
  
  maxLength <- max(runs$lengths[runs$values == TRUE]) 
  # for ANC, 20 if we count 'trace', 9 otherwise
  print(paste("Max run of consecutive days of snowfall for", cityName, "is", maxLength))
  # extract the indices for each run
  maxRuns <- which(runs$values == TRUE & runs$lengths == maxLength)
        
  allRunEnds <- cumsum(runs$lengths)
  maxEnds <- allRunEnds[maxRuns]
  
  startIs <- ifelse(maxRuns>1, maxRuns-1, 0)
  starts <- allRunEnds[startIs] + 1
  if (0 %in% startIs) starts <- c(1,starts)
  
  # calculate & print total snow accumulation
  for(i in 1:length(starts)) {
    cumSnow <- sum(as.numeric(dailyObs[starts[i]:maxEnds[i], "Snowfall"]))
    print(paste("  For streak starting ",dailyObs$Date[starts[i]],", ", cumSnow, " inches accumulated", sep=""))
  }
}

###############################
# INSTRUCTIONS: set citySID to the desired city station
# can find new station IDs using:
#     https://www.ncdc.noaa.gov/cdo-web/datatools/findstation
#     https://www.ncdc.noaa.gov/cdo-web/datatools/selectlocation

cityCode <- "ANC"; cityName <- "Anchorage"; citySID <- 26451 # citySID <- 500280
#cityCode <- "ANC"; cityName <- "AnchorageMerrill"; citySID <- 26409
dailyObs <- getACIS_StnData(citySID, cityName)
printSnowfallStreaks(dailyObs, cityName)

cityCode <- "ROC"; cityName <- "Rochester"; citySID <- 14768 # via Buffalo CWA
dailyObs <- getACIS_StnData(citySID, cityName)
printSnowfallStreaks(dailyObs, cityName)


#################### cut code #################

if (FALSE) {
  #cityCode <- "FAI"; cityName <- "Fairbanks"; citySID <- 26411 # Int'l Airport
  #cityCode <- "FAI"; cityName <- "Fairbanks"; citySID <- 26441 # Univ Exp Sta
  cityCode <- "FAI"; cityName <- "Fairbanks"; citySID <- "FAIthr" # Fairbanks threaded (combination of above 2)
  cityCode <- "ORD"; cityName <- "Chicago O'Hare"
  cityCode <- "CHI"; cityName <- "Chicago"
  #cityCode <- "WORC"; cityName <- "Worcester, MA" # snowdepth mostly missing, so don't bother running
  cityCode <- "ALB"; cityName <- "Albany"
  cityCode <- "BUF"; cityName <- "Buffalo"
  cityCode <- "BRECK"; cityName <- "Breckenridge"; citySID <- "050909"
}


if (FALSE) {
	rm(list=ls())
	gc()
	q()
}