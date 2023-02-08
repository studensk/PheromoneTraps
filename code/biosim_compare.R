library(lubridate)
library(googledrive)
library(googlesheets4)
library(openxlsx)
library(sf)
library(tidyverse)
library(BioSIM)
library(reshape2)

for (yr in 2018:2022) {
  name <- paste0('Trap Results ', yr)
  path <- paste0('./data/traps', yr, '.xlsx')
  drive_download(name, path = path, overwrite = TRUE)
}

all.traps.lst <- lapply(2018:2022, function(yr) {
  data <- read.xlsx(paste0('data/traps', yr, '.xlsx'))
  data <- data[1:(nrow(data)-2),1:(ncol(data)-2)]
  return(data)
})
all.traps <- bind_rows(all.traps.lst)
loc.ord.df <- all.traps.lst[[1]]
loc.ord <- loc.ord.df[order(loc.ord.df$Latitude, decreasing = FALSE),]$Location
all.traps$Location <- factor(all.traps$Location, levels = loc.ord)

captures.lst <- lapply(1:length(all.traps.lst), function(i) {
  data <- all.traps.lst[[i]]
  nms <- names(data)[5:(ncol(data)-2)]
  obs <- data[,c('Location', nms)]
  m.obs <- melt(obs, id = 'Location')
  m.obs.nao <- as.data.frame(na.omit(m.obs))
  m.obs.nao$Year <- (2018:2022)[i]
  names(m.obs.nao)[2:3] <- c('Date', 'Captures')
  m.obs.nao$Date <- as.Date(m.obs.nao$Date)
  return(subset(m.obs.nao, Captures > 0))
})
captures.df <- bind_rows(captures.lst)
captures.df$Location <- factor(captures.df$Location, levels = loc.ord)

pheno.lst <- lapply(1:nrow(all.traps), function(r) {
  row <- all.traps[r,]
  gen.w <- generateWeather(modelNames = 'Spruce_Budworm_Biology',
                           fromYr = row$Year, toYr = row$Year, id = row$Location,
                           latDeg = row$Latitude, longDeg = row$Longitude)
  gw <- gen.w[[1]]
  dates <- sapply(1:nrow(gw), function(rw) {
    row2 <- gw[rw,]
    year <- row2$Year
    month <- str_pad(as.character(row2$Month), width = 2, pad = '0', side = 'left')
    day <- str_pad(as.character(row2$Day), width = 2, pad = '0', side = 'left')
    date <- paste(c(year, month, day), collapse = '-')
  })
  dates <- as.Date(dates)
  rg1 <- range(which(gw$MaleFlight > 0))
  rg2 <- range(which(gw$MaleAdult > 0))
  start1 <- dates[rg1[1]]
  end1 <- dates[rg1[2]]
  start2 <- dates[rg2[1]]
  end2 <- dates[rg2[2]]
  df <- data.frame('Location' = row$Location, 'Year' = row$Year, 
                   'f_sDate' = start1, 'f_eDate' = end1,
                   'a_sDate' = start2, 'a_eDate' = end2)
  return(df)
})
pheno.df <- bind_rows(pheno.lst)
pheno.df$Location <- factor(pheno.df$Location, levels = loc.ord)

ggplot(data = pheno.df) + 
  geom_segment(aes(x = a_sDate, xend = a_eDate, y = factor(Location), yend = factor(Location)),
               alpha = 0.25, col = 'red', size = 1) +
  geom_segment(aes(x = f_sDate, xend = f_eDate, y = factor(Location), yend = factor(Location)),
               alpha = 0.75, col = 'red', size = 1) +
  geom_point(data = captures.df,
             aes(x = Date, y = factor(Location), size = Captures)) +
  facet_wrap(vars(Year), scales = 'free_x') +
  theme_minimal()


