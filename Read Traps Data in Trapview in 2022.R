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

## Google sheet access
drive_auth(email="sbudworm@gmail.com")

## Read trap names and coordinates
## We need to do that because the API has only access to the trap number and
## a trap can change location from year to year 
drive_download("Trap names and coordinates 2022",path="tmp/tmp2.xlsx",overwrite=TRUE)
Traps <- readxl::read_excel("tmp/tmp2.xlsx")

# Source the functions to access the API
source("Trapview API.R")

# Define some parameters
Year <- 2022
# Which provinces are we analysing
Prov <- "^QC|^NB|^ME|^NS|^NL"

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


# Get all the devices Devices
D <- getDevices()

# Select Eastern traps for 2022
# Rename some locations to adhere to naming standard for the project
D.2022 <- inner_join(D$device,Traps,by=c("partNo"="Code")) %>%
  dplyr::select(id,partNo,Location,Prov,Longitude,Latitude) %>%
  mutate(Location = replace(Location, Location == "Pointe au Mal", "Point au Mal")) %>%
  mutate(Location = replace(Location, Location == "Zinc Mine Road", "Zinc Mine Rd")) %>%
  mutate(Location = replace(Location, Location == "Gaspé", "Gaspe")) %>%
  mutate(Location = replace(Location, Location == "Dunière", "Duniere")) %>%
  mutate(Location = replace(Location, Location == "Duchénier", "Duchenier")) %>%
  rename(Code=partNo)

rm(D)

# Here is the list of traps
D.2022

################################################################################
#### Analysis 1: Calculate number of budworm caught every flight night
####             A flight night (FN) is the combination of afternoon and evening
####             of day 1 plus night of day 2. I keep the morning separate.
################################################################################

# Define the time window for the analysis
fromDate <- "2022-06-15T00:00:00"
toDate <- "2022-08-24T00:00:00"

# Create the Excel workbook that will receive the results 
wb <- createWorkbook()

# Identify Traps IDs
Traps.ID <- D.2022$id

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

# Fix some problems with device names in 2022
All.Events[All.Events$deviceName == "Clearwater 2022","deviceName"] <- "S04285"
All.Events[All.Events$deviceName == "Arisaig 2022","deviceName"] <- "S06207"

# Post-treatment of All.Events to add location name 
All.Events <- All.Events %>%
  mutate(timestamp=ymd_hms(timestamp)) %>%
  select(idEvent,timestamp,deviceName,eventType,typeMessage,
         eventStatus,statusMessage,noPests) %>%
  dplyr::left_join(.,D.2022,by=c("deviceName"="Code"))


#### Cleanup the events when there was a problem with paper tweak or image sent
# 1- List the potential problems
Problems <- All.Events %>%
  filter(eventStatus != 1 & eventType != 2) 
# 2- Too many problems with New Canada and Ingonish so we eliminate these traps
All.Events <- All.Events %>% 
  filter(!(Location %in% c("New Canada","Ingonish")))
# 3- If there is a problem with the image, eliminate the event
All.Events <- All.Events %>%
  filter(!(eventType == 1 & eventStatus != 1))
# 4- For some reason one paper tweak in Arisaig occurred several after
# the command was sent so we are giving it a fake time
All.Events <- mutate(All.Events,timestamp = replace(timestamp,deviceName == "S06207" & timestamp == ymd_hms("2022-06-30 20:00:01"),ymd_hms("2022-06-30 23:30:00")))

### Cleanup the cases where saturation occurred
# 1- List of saturation dates
saturations <- data.frame(Name="Cheeseman",Code="S04540",Start=ymd_hms("2022-07-18 06:00:00"),End=ymd_hms("2022-07-18 23:59:00"))
saturations[2,] <- list("Cheeseman","S04540",ymd_hms("2022-07-20 23:59:00"),ymd_hms("2022-07-21 14:00:00"))
saturations[3,] <- list("Port Saunders","S04620",ymd_hms("2022-07-22 23:59:00"),ymd_hms("2022-07-26 20:00:00"))
saturations[4,] <- list("Port Saunders","S04620",ymd_hms("2022-08-02 23:59:00"),ymd_hms("2022-08-03 20:00:00"))
saturations[5,] <- list("Zinc Mine Rd","S04610",ymd_hms("2022-07-22 23:59:00"),ymd_hms("2022-07-26 20:00:00"))
saturations[6,] <- list("New Canada","S04716",ymd_hms("2022-07-15 23:59:00"),ymd_hms("2022-07-22 05:00:00"))
# 2- Tag the records with saturated images as noPests is NA
for (i in 1:dim(All.Events)[1]) {
  if (All.Events[i,"eventType"] == 1 & All.Events[i,"eventStatus"] == 1 & All.Events[i,"deviceName"] %in% saturations$Code) {
    for (j in 1:dim(saturations)[1]) {
      if (All.Events[i,"deviceName"] == saturations[j,"Code"] & 
          ymd_hms(All.Events[i,"timestamp"]) > saturations[j,"Start"] & 
          ymd_hms(All.Events[i,"timestamp"]) < saturations[j,"End"]) {
        All.Events[i,"noPests"] <- NA
      }
    }
  }
}
# 3- Filter out the saturated images
All.Events <- filter(All.Events,!(eventType == 1 & is.na(noPests)))

# Histograms of picture times to help setup the cut times in between pictures
tmp <- All.Events %>%
  dplyr::mutate(Hour=as.integer(format(ymd_hms(All.Events$timestamp),"%H"))) %>%
  dplyr::filter(eventType == 1) %>%
  dplyr::select(timestamp,Hour,deviceName)
ggplot(data=tmp,aes(x=Hour)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(0, 24, by = 1))
# In 2022 we have breaks at 3,10,16,and 21

# Create master list of start times and end times
# #### REMEMBER: We code a FlightDay from noon one day to noon the next day
Date1 <- seq(from=ymd_hms("2022-06-15 02:00:00"), to=ymd_hms("2022-08-24 02:00:00"), by ="day")
Date2 <- seq(from=ymd_hms("2022-06-15 08:00:00"), to=ymd_hms("2022-08-24 08:00:00"), by ="day")
Date3 <- seq(from=ymd_hms("2022-06-15 16:00:00"), to=ymd_hms("2022-08-24 16:00:00"), by ="day")
Date4 <- seq(from=ymd_hms("2022-06-15 21:00:00"), to=ymd_hms("2022-08-24 21:00:00"), by ="day")
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

# Fix some problems by hand
# Correct Arisaig record by hand (problem with the timing of paper tweak
# recorded in Trapview)
TrapCatches <- mutate(TrapCatches,NewPests = replace(NewPests, Location == "Arisaig" & dates == as.Date("2022-06-30") & TimeOfDay == "Evening", 3)) %>%
  mutate(NewPests = replace(NewPests, Location == "Arisaig" & dates == as.Date("2022-07-01") & TimeOfDay == "Night", 0)) %>%
  # Correct a double picture at Cheeseman's Park on 2022-07-12
  filter(!(Location == "Cheesemans Park" & dates == ymd_hms("2022-07-12 05:33:00"))) %>%
  mutate(NewPests = replace(NewPests,Location == "Cheesemans Park" & dates == ymd_hms("2022-07-12 07:01:00"),20))


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
    full_join(.,D.2022) %>%
    ungroup() %>%
    dplyr::select(any_of(c("Location","Code","Longitude","Latitude",as.character(selectdates)))) %>%
    arrange(Location)
  # Fill in dates that are not recorded on Trapview
  yesdates <- tmp1 %>% select(starts_with(as.character(Year))) %>% colnames() %>% as.Date()
  nodates <- selectdates[!(selectdates %in% yesdates)]
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

### Calculate new pests for the day. If a part of the day is NA then calculate the sum
### but add 0.1 to indicate that there is a problem

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
saveWorkbook(wb, "tmp/Trap Monitoring 2022.xlsx", overwrite = TRUE)

# Copy the Excel file to Google Drive 
#drive_upload(media="Trap Monitoring 2022.xlsx",
#             path = "Data/Trap Monitoring/2022/",
#             name = "Trap Monitoring 2022",
#             overwrite = TRUE)

drive_auth(email="jean.noel.candau@gmail.com")
drive_upload(media="tmp/Trap Monitoring 2022.xlsx",
             path = "SBWTeam/PheromoneTraps/Data/Raw/",
             name = "Trap Results 2022",
             overwrite = TRUE)
