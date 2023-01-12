library(lubridate)
library(googledrive)
library(openxlsx)
library(sf)
library(fuzzyjoin)
library(tidyverse)


# Read spreadsheets: connect with internet browser and copy the link
Traps2020 <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1e4z0Pnn-hgro9KBW7d1RA4T_Ll1QXcfkidAqzm9Nsls/edit#gid=1787025890")
Traps2019 <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1n4H0fpVT8dKaOpZqvkkbY7Nx3-Kh_duwOGgAlJk3Hyk/edit#gid=1787025890")
Traps2018 <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1NLrHPCYcS5zey-yFF7ovRO_B_4lUsh3xekwH3hYRYnc/edit#gid=1787025890")

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

# Process sheets
# ss is the spreadsheet read from sbudworm google drive, i.e. the original spreadsheet that we created for the
# season's summaries
Process.R <- function(ss,Year) {
  tmp1 <- ss[!is.na(ss$Longitude) & ss$Longitude > -73,seq(1,ncol(ss)-2)]
  tmp1$Code <- sub("S","S0",tmp1$Code)
  First.Col <- min(which(grepl(as.character(Year),colnames(tmp1))))
  Last.Col <- max(which(grepl(as.character(Year),colnames(tmp1))))
  colnames(tmp1)[First.Col:Last.Col] <- as.character(as.Date(colnames(tmp1)[First.Col:Last.Col],format="%m-%d-%Y"))
  tmp2 <- abs(floor(tmp1[,First.Col:Last.Col]))
  tmp3 <- cbind(tmp1[,c("Name","Code","Longitude","Latitude")],tmp2)
  Daily.Captures <- tmp3 %>% rename(Location=Name) %>% 
    bind_rows(tibble::tibble_row(Location="Total",summarise(.,across(starts_with(as.character(Year)),function(x) {sum(floor(x),na.rm=TRUE)})))) %>%
    bind_rows(tibble::tibble_row(Location="N>0",summarise(slice_head(.,n=dim(.)[1]-1),across(starts_with(as.character(Year)),gt0)))) %>%
    rowwise() %>%
    mutate(Total=sumgt0(c_across(starts_with(as.character(Year))))) %>%
    mutate("N>0"=gt0(c_across(starts_with(as.character(Year)))))

  # Create the Excel workbook that will receive the results 
  wb <- createWorkbook()
  # Save the daily captures in a separate sheet of the workbook
  # and format the results to facilitate reading the results
  addWorksheet(wb,"Daily Captures")
  writeDataTable(wb, "Daily Captures", x = Daily.Captures,tableStyle = "TableStyleLight9")
  # Format the sheet
  Format.sheet(Daily.Captures,"Daily Captures")
  
  # Save the Excel file locally
  saveWorkbook(wb, paste0("Trap Monitoring ",Year,".xlsx"), overwrite = TRUE)
  
  drive_auth(email="jean.noel.candau@gmail.com")
  drive_upload(media=paste0("Trap Monitoring ",Year,".xlsx"),
               path = "SBWTeam/PheromoneTraps/Data/Raw/",
               name = paste0("Trap Monitoring ",Year),
               overwrite = TRUE)
}

# Running Process.R() doesn't seem to work if ss and Year are given as parameters
# I have to set ss <- ... and Year <- ... and run the function code manually for it to work!

