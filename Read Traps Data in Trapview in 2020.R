################################################################################
##
## This code reads the capture data from the Trapview site 
## does a few checks, saves the capture data in an Excel workbook
## and uploads it to a Google sheet workbook
##
################################################################################
library(lubridate)
library(googledrive)
library(openxlsx)
library(sf)
library(fuzzyjoin)
library(tidyverse)

################################################################################
## NOTES for 2020
##
## 1- I made the mistake of copying the 2019 list of locations and codes as a 
##    template when I started the 2020 season but some traps were moved around
##    and the locations and trap codes did not correspond anymore in 2020 for
##    traps in Port Saunders, St Anthony. It was corrected on the Google sheet
##    on Jan. 21, 2022. The others were all checked and good.
## 2- 
################################################################################


# Utilitary function to calculate the number of non-null values in a column
# Have to remove last value that corresponds to the column sum
gt0 <- function(data) {
  tmp <- data[!is.na(data)]
  sum(floor(tmp)>0)
}

sumgt0 <- function(data) {
  tmp <- data[!is.na(data)]
  sum(floor(tmp))
}

## Google sheet access
## If you have a problem with the credentials, go to ~/.cache/gargle
## and delete everything
drive_auth(email="sbudworm@gmail.com")


## Read trap names and coordinates
drive_download("Trap Monitoring 2020",path="tmp2.xlsx",overwrite=TRUE)
Traps <- readxl::read_excel("tmp2.xlsx",)
Traps$Code <- sub("S","S0",Traps$Code)
Traps <- Traps[,c("Name","Code","Latitude","Longitude")] %>% filter(stringr::str_detect(Code,"S0"))

# Source the functions to access the API
source("Trapview API.R")

# Define some parameters
Year <- 2020
# Which provinces are we analysing
Prov <- "^QC|^NB|^ME|^NS|^NL"

# Get all the devices Devices
D <- getDevices()

# Select Eastern traps for 2021
D.2020 <- left_join(Traps,D$device,by=c("Code"="partNo")) %>%
  mutate(tz=timeZone$name) %>%
  dplyr::select(id,Code,Name,Longitude,Latitude,tz) %>%
  arrange(Longitude)


# Recuperate a layer of state/province names
state_prov <- rnaturalearth::ne_states(c("united states of america", "canada"))

# Intersect and recover code for Prov or State
D.2020 <- st_join(st_as_sf(D.2020,coords = c("Longitude",'Latitude'),crs=st_crs(state_prov),remove=FALSE)
                  ,st_as_sf(state_prov)) %>%
  dplyr::select(id,Code,Name,Longitude,Latitude,postal,tz) %>%
  rename(Prov=postal) %>%
  st_drop_geometry()

# Remove Magdeleine Islands
D.2020 <- D.2020 %>%
  dplyr::filter(Code != "S04707")

# Sort by name
D.2020 <- D.2020 %>% 
  arrange(Name) %>% 
  rename(Location=Name) %>%
  select(id,Code,Location,Prov,Longitude,Latitude)

# Fix a couple of Prov names
D.2020 <- D.2020 %>% mutate(Prov = replace(Prov,Code == "S04255" , "QC")) %>%
  mutate(Prov = replace(Prov,Code == "S04607" , "NL")) %>%
  mutate(Prov = replace(Prov,Code == "S04620" , "NL"))


################################################################################
#### Analysis 1: Calculate number of budworm caught every flight night
####             A flight night (FN) is the combination of afternoon and evening
####             of day 1 plus night of day 2. I keep the morning separate.
################################################################################

# Define the time window for the analysis
fromDate <- "2020-06-15T00:00:00"
toDate <- "2020-08-24T00:00:00"

# Create the Excel workbook that will receive the results 
wb <- createWorkbook()

# Identify Traps IDs
Traps.ID <- D.2020$id

# Number of traps
ntrap <- length(Traps.ID)

# Get all the events for all the traps
# Result in "All.Events"
for (i in 1:ntrap) {
  print(i)
  if (i == 1) { 
    All.Events <- getEvents(Traps.ID[i],fromDate,toDate,history=TRUE,utc=FALSE)$event
  } else {
    tmp <- getEvents(Traps.ID[i],fromDate,toDate,history=TRUE,utc=FALSE)$event
    All.Events <- rbind(All.Events,tmp)
  }
  # Read Events
}

# Post-treatment of All.Events to add location name 
All.Events <- All.Events %>%
  mutate(timestamp=ymd_hms(timestamp)) %>%
  select(idEvent,timestamp,deviceName,eventType,typeMessage,
         eventStatus,statusMessage,noPests) %>%
  dplyr::left_join(.,D.2020,by=c("deviceName"="Code"))

# Remove Start and End of monitoring periods because they are not always recorded
# and therefore not instructive
All.Events <- filter(All.Events, eventType != "4" & eventType != "END")

# Check if there are any missing values
setdiff(All.Events,drop_na(All.Events))

# Some early-season images were checked but not recorded. These records have to
# be set to 0
list1 <- c("St Modeste","Escourt","Duchenier","Arseneault","Duniere","Gaspe",
           "Doaktown","Saint Quentin","Saint Leonard","Juniper",
           "Plaster Rock","South Tetagouche","Robinsonville",
           "Jacquet River")
All.Events <- mutate(All.Events,noPests = replace(noPests, Location %in% list1 & (as.Date(timestamp) < ymd("2020-06-15") | as.Date(timestamp) > ymd("2020-08-15")) ,0)) 
list2 <- c("Arisaig")
All.Events <- mutate(All.Events,noPests = replace(noPests, Location %in% list2 & (as.Date(timestamp) < ymd("2020-06-16") | as.Date(timestamp) > ymd("2020-08-15")) ,0)) 
list3 <- c("Pikauba","Forestville","Havre St Pierre","Baie Comeau","Sept-Iles",
           "Petit Etang","Inverness","Baldwin","Miramichi")
All.Events <- mutate(All.Events,noPests = replace(noPests, Location %in% list3 & (as.Date(timestamp) < ymd("2020-06-24") | as.Date(timestamp) > ymd("2020-08-15")) ,0))
list4 <- c("St Anthony","Pointe au mal","Port Saunders","Cheesemans Park")
All.Events <- mutate(All.Events,noPests = replace(noPests, Location %in% list4 & (as.Date(timestamp) < ymd("2020-07-01") | as.Date(timestamp) > ymd("2020-08-24")) ,0))


#### Cleanup the events when there was a problem with paper tweak or image sent
# 1- List the potential problems
Problems <- All.Events %>%
  filter(eventStatus != 1 & eventType != 2) 
# 2- Too many problems with New Canada and Ingonish so we eliminate these traps
#All.Events <- All.Events %>% 
#  filter(!(Location %in% c("New Canada","Ingonish")))
# 3- If there is a problem with the image, eliminate the event
All.Events <- All.Events %>%
  filter(!(eventType == 1 & eventStatus != 1))

# Histograms of picture times to help setup the cut times in between pictures
tmp <- All.Events %>%
  dplyr::mutate(Hour=as.integer(format(ymd_hms(All.Events$timestamp),"%H"))) %>%
  dplyr::filter(eventType == 1) %>%
  dplyr::select(timestamp,Hour,deviceName)
ggplot(data=tmp,aes(x=Hour)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(0, 24, by = 1))
# In 2022 we have breaks at 3,10,16,and 21
# In 2021 we have breaks at 1:30,9:30,16,and 21:30
# In 2020 we have breaks at 1:30,9:30,13:30,and 21:30

# Create master list of start times and end times
# #### REMEMBER: We code a FlightDay from noon one day to noon the next day
Date1 <- seq(from=ymd_hms("2020-06-15 01:30:00"), to=ymd_hms("2020-08-24 01:30:00"), by ="day")
Date2 <- seq(from=ymd_hms("2020-06-15 09:30:00"), to=ymd_hms("2020-08-24 09:30:00"), by ="day")
Date3 <- seq(from=ymd_hms("2020-06-15 13:30:00"), to=ymd_hms("2020-08-24 13:30:00"), by ="day")
Date4 <- seq(from=ymd_hms("2020-06-15 21:30:00"), to=ymd_hms("2020-08-24 21:30:00"), by ="day")
tmp <- sort(c(Date1,Date2,Date3,Date4))
Dates <- tibble(StartDates=tmp[1:length(tmp)-1]) %>%
  mutate(EndDates=tmp[-1]) %>%
  mutate(TimeOfDay=rep_along(StartDates,c("Night","Morning","Afternoon","Evening"))) %>%
  mutate(FlightDay=if_else(TimeOfDay %in% c("Night","Morning"),lubridate::as_date(StartDates)-1,lubridate::as_date(StartDates)))

# NewPests takes a portion of All.Events corresponding to a trap
# and calculates a new variables that counts the number of new moths
# captured every time step
NewPests.f <- function(df) {
  # Create the Time of Day and the correct Flight day
  tmp1 <- fuzzy_left_join(Dates,
                          df %>% mutate(dates=ymd_hms(timestamp)),
                          by = c("StartDates"="dates", "EndDates"="dates"),
                          match_fun = list(`<=`, `>=`)) 
  # Do a little house cleaning on the pics
  Pics <- tmp1 %>%
    filter(eventType == 1 & eventStatus == 1) %>%
    # If a picture was taken twice during a time period then keep only the last one, i.e. the picture
    # with the maximum number of moths
    group_by(FlightDay,TimeOfDay) %>%
    distinct(noPests,.keep_all = TRUE) %>%
    filter(noPests == max(noPests)) %>%
    ungroup() 
  # Select the tweaks  
  Tweaks <- tmp1 %>%
    filter(eventType %in% c(6,"RSR") & eventStatus == 1)
  # Put pics and tweaks back together
  tmp2 <- bind_rows(Pics,Tweaks) %>%
    # Group by dates
    arrange(dates) %>%
    rename(Code=deviceName) %>%
    select(dates,StartDates,EndDates,TimeOfDay,FlightDay,idEvent,Code,Location,eventType,eventStatus,noPests) %>%
    mutate(NewPests=NA)
  
  # Now run the algorithm to calculate the number of new pests
  if(dim(tmp2)[1]>1) {
    for (n in 2:dim(tmp2)[1]) {
      if (tmp2$eventType[n] == 1) {
        if(tmp2$TimeOfDay[n] == "Morning") {
          if(tmp2$TimeOfDay[n-1] == "Night" & tmp2$FlightDay[n] == tmp2$FlightDay[n-1]) {
            if(tmp2$eventType[n-1] == 1) {
              tmp2$NewPests[n] = tmp2$noPests[n] - tmp2$noPests[n-1]
            }
            if(tmp2$eventType[n-1] %in% c("6","RSR")) {
              tmp2$NewPests[n] = tmp2$noPests[n]
            }
          }
        }
        if(tmp2$TimeOfDay[n] == "Night") {
          if(tmp2$TimeOfDay[n-1] == "Evening" & tmp2$FlightDay[n] == tmp2$FlightDay[n-1]) {
            if(tmp2$eventType[n-1] == 1) {
              tmp2$NewPests[n] = tmp2$noPests[n] - tmp2$noPests[n-1]
            }
            if(tmp2$eventType[n-1] %in% c("6","RSR")) {
              tmp2$NewPests[n] = tmp2$noPests[n]
            }
          }
        }
        if(tmp2$TimeOfDay[n] == "Evening") {
          if(tmp2$TimeOfDay[n-1] == "Afternoon" & tmp2$FlightDay[n] == tmp2$FlightDay[n-1]) {
            if(tmp2$eventType[n-1] == 1) {
              tmp2$NewPests[n] = tmp2$noPests[n] - tmp2$noPests[n-1]
            }
            if(tmp2$eventType[n-1] %in% c("6","RSR")) {
              tmp2$NewPests[n] = tmp2$noPests[n]
            }
          }
        }
        if(tmp2$TimeOfDay[n] == "Afternoon") {
          if(tmp2$TimeOfDay[n-1] == "Morning" & tmp2$FlightDay[n] == tmp2$FlightDay[n-1]+1) {
            if(tmp2$eventType[n-1] == 1) {
              tmp2$NewPests[n] = tmp2$noPests[n] - tmp2$noPests[n-1]
            }
            if(tmp2$eventType[n-1] %in% c("6","RSR")) {
              tmp2$NewPests[n] = tmp2$noPests[n]
            }
          }
        }
        
      }
    }
  }
  tmp2
}

TrapCatches <- All.Events %>%
  group_by(deviceName) %>%
  group_map(~ NewPests.f(.x),.keep = TRUE) %>%
  do.call("rbind",.)

Format.sheet <- function(traps.data,traps.sheet) {
  lowStyle <- createStyle(bgFill = "yellow")
  mediumStyle <- createStyle(bgFill = "pink")
  severeStyle <- createStyle(bgFill = "red")
  errorStyle <- createStyle(fontColour = "yellow", bgFill = "purple", textDecoration = "Bold")
  warningStyle <- createStyle(borderColour = "orange",borderStyle="thick")
  First.Col <- min(which(grepl(Year,colnames(traps.data))))
  Last.Col <- max(which(grepl(Year,colnames(traps.data))))
  First.Row <- 2
  Last.Row <- dim(traps.data)[1]-1
  conditionalFormatting(wb, traps.sheet, cols=First.Col:Last.Col, rows=First.Row:Last.Row, rule=c(1,9), type="between", style = lowStyle)
  conditionalFormatting(wb, traps.sheet, cols=First.Col:Last.Col, rows=First.Row:Last.Row, rule=c(10,19), type="between",style = mediumStyle)
  conditionalFormatting(wb, traps.sheet, cols=First.Col:Last.Col, rows=First.Row:Last.Row, rule=">19", style = severeStyle)
  conditionalFormatting(wb, traps.sheet, cols=First.Col:Last.Col, rows=First.Row:Last.Row, rule="<0", style = errorStyle)
  coords.style <- createStyle(numFmt = "0.000")
  addStyle(wb, traps.sheet, style = coords.style, cols = 3:4, rows = First.Row:Last.Row,gridExpand = TRUE)
  margins.style <- createStyle(textDecoration = "Bold")
  addStyle(wb, traps.sheet, style = margins.style, rows = (Last.Row+1):(Last.Row+2), cols = 1:(Last.Col+2), gridExpand = TRUE)
  addStyle(wb, traps.sheet, style = margins.style, rows = 1:(Last.Row+2), cols = (Last.Col+1):(Last.Col+2), gridExpand = TRUE)
  
}

### Calculate new pests for each part of the day
purrr::map(list("Morning","Night","Afternoon","Evening"), function(x) {
  selectdates <- seq(as.Date(fromDate),as.Date(toDate),by="day") 
  # Calculate the table
  tmp1 <- TrapCatches %>%
    filter(eventType == 1) %>%
    filter(TimeOfDay == x) %>%
    dplyr::select(Location,FlightDay,NewPests) %>%
    arrange(FlightDay,.by_group = TRUE) %>%
    pivot_wider(names_from = FlightDay,values_from = NewPests,names_sort = TRUE) %>%
    full_join(.,D.2020) %>%
    ungroup() %>%
    dplyr::select(any_of(c("Location","Code","Longitude","Latitude",as.character(selectdates)))) %>%
    arrange(Location)
  yesdates <- tmp1 %>% select(starts_with(as.character(Year))) %>% colnames() %>% as.Date()
  nodates <- selectdates[!(selectdates %in% yesdates)]
  # Fill in dates that are not recorded on Trapview
  tb <- as_tibble(matrix(double(),nrow = nrow(tmp1), ncol = length(nodates), dimnames = list(NULL, as.character(nodates)))) %>%
    bind_cols(select(tmp1,starts_with(as.character(Year)))) %>%
    select(order(colnames(.)))
  tmp2 <- bind_cols(select(tmp1,Location,Code,Longitude,Latitude),tb)
  # Process totals for rows and columns
  tmp3 <- tmp2 %>%
    bind_rows(tibble::tibble_row(Location="Total",summarise(.,across(starts_with(as.character(Year)),sum,na.rm=TRUE)))) %>%
    bind_rows(tibble::tibble_row(Location="N>0",summarise(slice_head(.,n=dim(.)[1]-1),across(starts_with(as.character(Year)),gt0)))) %>%
    rowwise() %>%
    mutate(Total=sumgt0(c_across(starts_with(as.character(Year))))) %>%
    mutate("N>0"=gt0(c_across(starts_with(as.character(Year)))))
  # Save to workbook
  addWorksheet(wb,x)
  writeDataTable(wb,sheet=x,x = tmp3,tableStyle = "TableStyleLight9")
  # Format
  Format.sheet(tmp3,x)
})

# Select the trap count part of each spreadsheet
a <- purrr::map(list("Morning","Night","Afternoon","Evening"), function(x) {
  read.xlsx(wb,x) %>% 
    dplyr::select(starts_with(as.character(Year))) %>%
    slice(1:(n() - 2)) 
  #  %>%
  #    pivot_longer(cols=starts_with(as.character(Year)),names_to="Date",values_to="NewPests")
})
# Calculate the sum - Adds 0.1 everytime there is a NA so the values can be x.1, x.2, x.3 
# If all the time of days are NA then the sum is NA
Total <- Reduce(`+`, lapply(list(a[[1]],a[[2]],a[[3]],a[[4]]),function(x) {x[is.na(x)] <-0.1;x}))
Total[Total == 0.4] <- NA
# Add the first columns and row an colums sums back
Daily.Captures <- read.xlsx(wb,"Morning") %>% slice(1:(n() - 2)) %>% select(1:4) %>% bind_cols(Total) %>%
  bind_rows(tibble::tibble_row(Location="Total",summarise(.,across(starts_with(as.character(Year)),function(x) {sum(floor(x),na.rm=TRUE)})))) %>%
  bind_rows(tibble::tibble_row(Location="N>0",summarise(slice_head(.,n=dim(.)[1]-1),across(starts_with(as.character(Year)),gt0)))) %>%
  rowwise() %>%
  mutate(Total=sumgt0(c_across(starts_with(as.character(Year))))) %>%
  mutate("N>0"=gt0(c_across(starts_with(as.character(Year)))))

# Save the daily captures in a separate sheet of the workbook
# and format the results to facilitate reading the results
addWorksheet(wb,"Daily Captures")
writeDataTable(wb, "Daily Captures", x = Daily.Captures,tableStyle = "TableStyleLight9")
# Format the sheet
Format.sheet(Daily.Captures,"Daily Captures")

# Save the Excel file locally
saveWorkbook(wb, "Trap Monitoring 2020.xlsx", overwrite = TRUE)

# Copy the Excel file to Google Drive 
#drive_upload(media="Trap Monitoring 2021.xlsx",
#             path = "Data/Trap Monitoring/2021/",
#             name = "Trap Monitoring 2021",
#             overwrite = TRUE)

## Google sheet access
drive_auth(email="jean.noel.candau@gmail.com")
drive_upload(media="Trap Monitoring 2020.xlsx",
             path = "SBWTeam/PheromoneTraps/Data/Raw/",
             name = "Trap Monitoring 2020",
             overwrite = TRUE)
