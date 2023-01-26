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
## NOTES for 2018
##
## 1- 
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
drive_download("Trap Monitoring 2018",path="./tmp/tmp2.xlsx",overwrite=TRUE)
Traps <- readxl::read_excel("./tmp/tmp2.xlsx",)
Traps$Code <- sub("S","S0",Traps$Code)
Traps <- Traps[,c("Name","Code","Latitude","Longitude")] %>% filter(stringr::str_detect(Code,"S0"))

# Source the functions to access the API
source("Trapview API.R")

# Define some parameters
Year <- 2018
# Which provinces are we analysing
Prov.names <- c("QC","NB","ME","NS","NL")

# Get all the devices Devices
D <- getDevices()

# Select Eastern traps for 2021
D.2018 <- left_join(Traps,D$device,by=c("Code"="partNo")) %>%
  mutate(tz=timeZone$name) %>%
  dplyr::select(id,Code,Name,Longitude,Latitude,tz) %>%
  arrange(Longitude)


# Recuperate a layer of state/province names
state_prov <- rnaturalearth::ne_states(c("united states of america", "canada"))

# Intersect and recover code for Prov or State
D.2018 <- st_join(st_as_sf(D.2018,coords = c("Longitude",'Latitude'),crs=st_crs(state_prov),remove=FALSE)
                  ,st_as_sf(state_prov)) %>%
  dplyr::select(id,Code,Name,Longitude,Latitude,postal,tz) %>%
  rename(Prov=postal) %>%
  st_drop_geometry()

# Sort by name
D.2018 <- D.2018 %>% 
  arrange(Name) %>% 
  rename(Location=Name) %>%
  select(id,Code,Location,Prov,Longitude,Latitude)

# Fix a couple of Prov names
# and select only Eastern traps
D.2018 <- D.2018 %>% mutate(Prov = replace(Prov,Code == "S04255" , "QC")) %>%
  mutate(Location = replace(Location,Location == "NFLD" , "Zinc Mine Rd")) %>%
  filter(Prov %in% Prov.names)


################################################################################
#### Analysis 1: Calculate number of budworm caught every flight night
####             A flight night (FN) is the combination of afternoon and evening
####             of day 1 plus night of day 2. I keep the morning separate.
################################################################################

# Define the time window for the analysis
fromDate <- "2018-06-15T00:00:00"
toDate <- "2018-08-24T00:00:00"

drive_download("Eastern Traps 2018",path="./tmp/tmp.xlsx",overwrite=TRUE)
AllTraps.xls <- readxl::read_excel("./tmp/tmp.xlsx")

AllTraps <- do.call("rbind", lapply(1:ncol(AllTraps.xls),function(x) {
  tmp <- select(AllTraps.xls,all_of(x)) %>%
    slice(-c(1,2,3)) %>%
    separate(1,into=unlist(str_split(str_remove_all(as_vector(AllTraps.xls[3,1]),"\""),";")),sep=";") %>%
    mutate(Date=dmy_hms(Date),
           Total=as.double(str_remove_all(`Pests no.`,"\"")),
           NewPests=as.double(str_remove_all(`Pests difference`,"\"")),
           User=str_remove_all(`User reviewed`,"\"")) %>%
    select(Date,Total,NewPests,User) %>%
    mutate(Location=colnames(AllTraps.xls)[x])
}))

# Select records for newpests (eliminate records of paper tweaks not needed here)
# Change name and eliminate some pictures taken twice
AllTraps <- filter(AllTraps,User == "Yes") %>%
  mutate(Location = replace(Location,Location == "Newfoundland" , "Zinc Mine Rd")) %>%
  filter(Location != "Pikauba" & Date != dmy_hms("13.07.2018 10:18:00")) %>%
  filter(Location != "South Tetagouche" & Date != dmy_hms("14.08.2018 22:22:00")) %>%
  filter(Location != "Zinc Mine Rd" & Date != dmy_hms("19.07.2018 13:30:00"))
  
# Histograms of picture times to help setup the cut times in between pictures
tmp <- AllTraps %>%
  dplyr::mutate(Hour=as.integer(format(Date,"%H"))) %>%
  dplyr::select(Date,Hour,Location)
ggplot(data=tmp,aes(x=Hour)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(0, 24, by = 1))
# In 2022 we have breaks at 3,10,16,and 21
# In 2021 we have breaks at 1:30,9:30,16,and 21:30
# In 2020 we have breaks at 1:30,9:30,13:30,and 21:30
# In 2019 we have breaks at 1:30,9:30,13:30,and 21:30 except for NL for which 
# we have to use 4:00, 10:00, 20:00 and 23:30  
# In 2018 we have breaks at 2:30,10:30,14:35,and 21:30

# Create master list of start times and end times
# #### REMEMBER: We code a FlightDay from noon one day to noon the next day
Date1 <- seq(from=ymd_hms("2018-06-15 02:30:00"), to=ymd_hms("2018-08-24 02:30:00"), by ="day")
Date2 <- seq(from=ymd_hms("2018-06-15 10:30:00"), to=ymd_hms("2018-08-24 10:30:00"), by ="day")
Date3 <- seq(from=ymd_hms("2018-06-15 14:35:00"), to=ymd_hms("2018-08-24 14:35:00"), by ="day")
Date4 <- seq(from=ymd_hms("2018-06-15 21:30:00"), to=ymd_hms("2018-08-24 21:30:00"), by ="day")
tmp <- sort(c(Date1,Date2,Date3,Date4))
Dates <- tibble(StartDates=tmp[1:length(tmp)-1]) %>%
  mutate(EndDates=tmp[-1]) %>%
  mutate(TimeOfDay=rep_along(StartDates,c("Night","Morning","Afternoon","Evening"))) %>%
  mutate(FlightDay=if_else(TimeOfDay %in% c("Night","Morning"),lubridate::as_date(StartDates)-1,lubridate::as_date(StartDates)))

NewPests.f <- function(df,Dates) {
  # Create the Time of Day and the correct Flight day
  tmp1 <- fuzzy_left_join(Dates,
                          df,
                          by = c("StartDates"="Date", "EndDates"="Date"),
                          match_fun = list(`<=`, `>=`))
  tmp1 %>% mutate(Location=df$Location[1],NewPests=NewPests) %>%
    select(StartDates,EndDates,TimeOfDay,FlightDay,NewPests,Location)
}

TrapCatches <- AllTraps %>%
  group_by(Location) %>%
  group_map(~ NewPests.f(.x,Dates),.keep = TRUE) %>%
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

# Create the Excel workbook that will receive the results 
wb <- createWorkbook()

### Calculate new pests for each part of the day
purrr::map(list("Morning","Night","Afternoon","Evening"), function(x) {
  selectdates <- seq(as.Date(fromDate),as.Date(toDate),by="day") 
  tmp1 <- TrapCatches %>%
    group_by(Location) %>%
    filter(TimeOfDay == x) %>%
    dplyr::select(Location,FlightDay,NewPests) %>%
    pivot_wider(names_from = FlightDay,values_from = NewPests,names_sort = TRUE) %>%
    full_join(.,D.2018) %>%
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
saveWorkbook(wb, "Trap Monitoring 2018.xlsx", overwrite = TRUE)

# Copy the Excel file to Google Drive 
#drive_upload(media="Trap Monitoring 2021.xlsx",
#             path = "Data/Trap Monitoring/2021/",
#             name = "Trap Monitoring 2021",
#             overwrite = TRUE)

## Google sheet access
drive_auth(email="jean.noel.candau@gmail.com")
drive_upload(media="Trap Monitoring 2018.xlsx",
             path = "SBWTeam/PheromoneTraps/Data/Raw/",
             name = "Trap Results 2018",
             overwrite = TRUE)
