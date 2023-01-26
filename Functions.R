################################################################################
##
## This code reads the capture data from the Trapview site 
## does a few checks, saves the capture data in an Excel workbook
## and uploads it to a Google sheet workbook
##
################################################################################
library(lubridate)
library(googledrive)
library(googlesheets4)
library(openxlsx)
library(sf)
library(fuzzyjoin)
library(tidyverse)

# Download annual googlesheets from 2018-2022 and output all the data
# in a single tibble
GetAllTrapData <- function() {
  drive_auth(email="jean.noel.candau@gmail.com")
  All.Traps <- do.call("rbind",lapply(2018:2022, function(Year) {
    drive_download(paste0("SBWTeam/PheromoneTraps/Data/Raw/Trap Results ",Year),
                   type="xlsx",path="tmp/tmp.xlsx",overwrite=TRUE)
    do.call("rbind",lapply(c("Morning","Afternoon","Evening","Night"),function(x) {
      Traps <- readxl::read_xlsx("tmp/tmp.xlsx",sheet = x) %>%
        select(Location,Code,Longitude,Latitude,starts_with(as.character(Year))) %>%
        filter(!is.na(Code)) %>%
        pivot_longer(cols=starts_with(as.character(Year)),names_to = "FlightDay",values_to = "MothCount") %>%
        mutate(TimeOfDay=x)
      Traps
    }))
  }))
  All.Traps %>% mutate(FlightDay = ymd(FlightDay))
}
