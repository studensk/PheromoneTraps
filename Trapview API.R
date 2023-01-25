################################################################################
###
### Trapview API - Retrieve data from the Trapview servers
###
################################################################################
library(httr)
library(jsonlite)

## Access to Trapview API
user <- "jcandau"
pass <- "budworm99"

# SubjectId is a code for the user
getSubjectId <- function(username=user,password=pass) {
  a <- GET(paste0("https://app.efos.si/trapviewWS/v2/getUser/",username), authenticate(username,password, type = "basic"))
  b <- content(a, "parsed")
  b$user$idSubject
}

# Report the details of each device
getDevices <- function(username=user,password=pass){
  Id <- getSubjectId(username,password)
  a <- GET(paste0("https://app.efos.si/trapviewWS/v2/getDevices/",Id), authenticate(username,password, type = "basic"))
  fromJSON(rawToChar(a$content))
}

# Retrieve events for a particular trap
# To disregard current monitoring period and get all the "historic" events between given dates, add "history=true" to your API call.
getEvents <- function(TrapID,fromDate,toDate,utc=TRUE,history=FALSE) {
  c <- GET(paste0("https://app.efos.si/trapviewWS/v2/getEvents/",TrapID,"?fromDate=",fromDate,"&toDate=",toDate,"&history=",history,"&utc=",utc),
           authenticate(user,pass, type = "basic"))
  d <- fromJSON(rawToChar(c$content))
  d
}


# Retrieve all the events for a particular time period for all the traps
# To disregard current monitoring period and get all the "historic" events between given dates, add "history=true" to your API call.
getAllEvents <- function(fromDate,toDate,utc=TRUE,history=TRUE) {
  # Get all the devices Devices
  D <- getDevices()
  
  # Identify Traps IDs
  Traps.ID <- D$device$id
  
  # Number of traps
  ntrap <- length(Traps.ID)
  
  All.Events <- data.frame(matrix(nrow=0,ncol=17))
  colnames(All.Events) <- c("idEvent","timestamp","deviceName","eventType","typeMessage",
                     "eventStatus","statusMessage","noPests","userReviewed",  
                     "pestName","pestCode","pestLatinName","batteryVoltage",
                     "signalStrength","XCoord","YCoord","dLastModified")
  
  # Get all the events for all the traps
  # Result in "All.Events"
  for (i in 1:ntrap) {
    print(i)
    tmp <- getEvents(Traps.ID[i],fromDate,toDate,history=history,utc=utc)$event
    # If the columns of tmp are the same as All.Events
    if (length(intersect(colnames(tmp),colnames(All.Events))) == dim(All.Events)[2]) {
      All.Events <- rbind(All.Events,tmp)
    }
  }
  All.Events
}


# Retrieve weather for particular trap
getWeather <- function(TrapID,fromDate,toDate,utc=TRUE) {
  c1 <- GET(paste0("https://app.efos.si/trapviewWS/v2/getSensorData/",TrapID,"/",fromDate,"/",toDate,"&utc=",utc),
            authenticate(user,pass, type = "basic"))
  d1 <- fromJSON(rawToChar(c1$content))
  d1
}
