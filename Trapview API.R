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

# Retrieve weather for particular trap
getWeather <- function(TrapID,fromDate,toDate,utc=TRUE) {
  c1 <- GET(paste0("https://app.efos.si/trapviewWS/v2/getSensorData/",TrapID,"/",fromDate,"/",toDate,"&utc=",utc),
            authenticate(user,pass, type = "basic"))
  d1 <- fromJSON(rawToChar(c1$content))
  d1
}
