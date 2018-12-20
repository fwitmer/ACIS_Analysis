##########################################################################
# Author: Frank Witmer
#
# 20 Dec 2018
#	 read weather observations from http://data.rcc-acis.org
#   calculate the longest consecutive snowfall streak
#
##########################################################################

# INSTRUCTIONS: set citySID to the desired city station
# can find new station IDs using:
#     https://www.ncdc.noaa.gov/cdo-web/datatools/findstation
#     https://www.ncdc.noaa.gov/cdo-web/datatools/selectlocation
# either of these sids works for ANC
cityCode <- "ANC"; cityName <- "Anchorage"; citySID <- 26451 # citySID <- 500280
if (FALSE) {
#cityCode <- "FAI"; cityName <- "Fairbanks"; citySID <- 26411 # Int'l Airport
#cityCode <- "FAI"; cityName <- "Fairbanks"; citySID <- 26441 # Univ Exp Sta
cityCode <- "FAI"; cityName <- "Fairbanks"; citySID <- "FAIthr" # Fairbanks threaded (combination of above 2)
cityCode <- "ROC"; cityName <- "Rochester"; citySID <- 14768 # via Buffalo CWA
cityCode <- "ORD"; cityName <- "Chicago O'Hare"
cityCode <- "CHI"; cityName <- "Chicago"
#cityCode <- "WORC"; cityName <- "Worcester, MA" # snowdepth mostly missing, so don't bother running
cityCode <- "ALB"; cityName <- "Albany"
cityCode <- "BUF"; cityName <- "Buffalo"
cityCode <- "BRECK"; cityName <- "Breckenridge"; citySID <- "050909"
}

	
packages <- c("httr") # depends on 'curl'
for (package in packages) {
  if (!(package %in% installed.packages())) { 
    install.packages(package) 
  } 
}

library(httr)
req <- GET("http://data.rcc-acis.org/StnData", 
  query = list(sid=citySID, sdate="por", edate="por", elems="1,2,4,10,11", output="csv"))
#str(req)
colnms <- c("Date", "MaxTemperature", "MinTemperature", "Precipitation", "Snowfall", "SnowDepth")
  
txt <- content(req, as="text")
#txt <- content(req, as="text", type="text/csv")
#str(txt)
txtLines <- unlist(strsplit(txt, "\n"))
str(txtLines)
head(txtLines[-1])
dailyObs <- read.table(text = txtLines[-1], sep=",", col.names=colnms, stringsAsFactors = FALSE)
str(dailyObs)
head(dailyObs)
nrow(dailyObs)

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
print(paste("Max run of consecutive days of snowfall", maxLength))
# extract the indices for each run
maxRuns <- which(runs$values == TRUE & runs$lengths == maxLength)
      
allRunEnds <- cumsum(runs$lengths)
maxEnds <- allRunEnds[maxRuns]

startIs <- ifelse(maxRuns>1, maxRuns-1, 0)
starts <- allRunEnds[startIs] + 1
if (0 %in% startIs) starts <- c(1,starts)

dailyObs[(starts[1]-1):(maxEnds[1]+1), ]


for(i in 1:length(starts)) {
  print(dailyObs$Date[starts[i]])
}



#################### cut code #################

if (FALSE) {
	rm(list=ls())
	gc()
	q()
}