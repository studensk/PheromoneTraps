library(lubridate)
library(tidyverse)
library(chillR)

##### Clean Data #####
tempdata <- read.csv('data/TrapWeather_2018-2022.csv', sep = ';')
tempdata$humi <- as.numeric(chartr(',', '.', tempdata$humi))
tempdata$temp <- as.numeric(chartr(',', '.', tempdata$temp))

newtrap.lst <- lapply(2023:2024, function(y) {
  read.csv(paste0('data/TrapWeather_', y, '.csv'))
}) 
newtraps <- bind_rows(newtrap.lst)

tempdata <- bind_rows(tempdata, newtraps)

dates <- sapply(tempdata$DMeasure, function(x) {
  dtvec <- strsplit(x, 'T')[[1]]
  dt <- paste0(dtvec[1], ' ', dtvec[2], ' ', 'UTC')
  return(dt)
})
tempdata$Date <- as_datetime(dates)

td.cols <- c('Date', 'Code', 'humi', 'temp')
tempdata <- subset(tempdata, select = -DMeasure)[,td.cols]

tempdata$Date <- as.POSIXct(tempdata$Date)
tempdata$Year <- year(tempdata$Date)
tempdata$Index <- paste0(tempdata$Code, '_', tempdata$Year)

td.lst <- lapply(unique(tempdata$Index), function(ind) {
  print(ind)
  sub <- subset(tempdata, Index == ind)
  code <- unique(sub$Code)
  start <- sub$Date[1]
  if (month(start) > 8) {return(NULL)}
  end <- sub$Date[nrow(sub)]
  dates <- seq(start, end, by = 'hour')
  ddf <- data.frame('Date' = dates)
  all <- full_join(ddf, sub)
  all$Index <- ind
  all$Code <- code
  
  full.date.lst <- lapply(as.character(all$Date), function(x) {
    datetime.vec <- strsplit(x, split = ' ')[[1]]
    splits <- c('-', ':')
    att.vec <- list('Year' = year(x), 'Month' = month(x), 
                    'Day' = day(x), 'Hour' = hour(x))
    return(as.data.frame(att.vec))
  })
  full.date.df <- bind_rows(full.date.lst)
  full.date.df$Temp <- all$temp
  
  #### IMPUTE TEMPS ####
  if (any(is.na(all$temp))) {
    all$temp <- interpolate_gaps_hourly(full.date.df)$weather$Temp
  }
  return(all)
})
td.df <- bind_rows(td.lst)

write.csv(td.df, 'data/TrapWeather_clean.csv', row.names = FALSE)

