### To install BioSIM if necessary:
# require(remotes)
# install_github("RNCan/BioSimClient_R")

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

##### RUN TO HERE AND ENTER AUTH TOKEN #####

all.traps.lst <- lapply(2018:2022, function(yr) {
  data <- xlsx::read.xlsx(paste0('data/traps', yr, '.xlsx'), 
                                 sheetName = 'Daily Captures')
  data <- data[1:(nrow(data)-2),1:(ncol(data)-2)]
  return(data)
})
all.traps <- bind_rows(all.traps.lst)
loc.ord.df <- all.traps
loc.ord <- unique(loc.ord.df[order(loc.ord.df$Latitude,
                                   decreasing = FALSE),]$Location)
all.traps$Location <- factor(all.traps$Location, levels = loc.ord)

coords <- subset(all.traps, select = c(Location, Longitude, Latitude))
coords$Latitude <- round(coords$Latitude, 4)
coords$Longitude <- round(coords$Longitude, 4)
coords <- coords[!duplicated(coords),]
coords.ag <- aggregate(data = coords, 
                       cbind(Latitude, Longitude) ~ Location, mean)

captures.lst <- lapply(1:length(all.traps.lst), function(i) {
  data <- all.traps.lst[[i]]
  nms <- names(data)[5:(ncol(data)-2)]
  obs <- data[,c('Location', nms)]
  m.obs <- melt(obs, id = 'Location')
  m.obs.nao <- as.data.frame(na.omit(m.obs))
  m.obs.nao$Year <- (2018:2022)[i]
  names(m.obs.nao)[2:3] <- c('Date', 'Captures')
  m.obs.nao$Date <- sapply(as.character(m.obs.nao$Date), function(d) {
    s <- strsplit(d, split = 'X')[[1]][2]
    s.new <- chartr('.', '-', s)
    return(s.new)
  })
  m.obs.nao$Date <- as.Date(m.obs.nao$Date)
  return(subset(m.obs.nao, Captures > 0))
})
captures.df <- bind_rows(captures.lst)
captures.df$Location <- factor(captures.df$Location, levels = loc.ord)

pheno.yr.lst <- lapply(1:length(all.traps.lst), function(i) {
  traps <- all.traps.lst[[i]]
  yr <- (2018:2022)[i]
  pheno.lst <- lapply(1:nrow(traps), function(r) {
    row <- traps[r,]
    gen.w <- generateWeather(modelNames = 'Spruce_Budworm_Biology',
                             fromYr = yr, toYr = yr, id = row$Location,
                             latDeg = row$Latitude, longDeg = row$Longitude)
    gw <- gen.w[[1]]
    dates <- sapply(1:nrow(gw), function(rw) {
      row2 <- gw[rw,]
      year <- row2$Year
      month <- str_pad(as.character(row2$Month), 
                       width = 2, pad = '0', side = 'left')
      day <- str_pad(as.character(row2$Day), 
                     width = 2, pad = '0', side = 'left')
      date <- paste(c(year, month, day), collapse = '-')
    })
    dates <- as.Date(dates)
    rg1 <- range(which(gw$MaleFlight > 0))
    rg2 <- range(which(gw$MaleAdult > 0))
    start1 <- dates[rg1[1]]
    end1 <- dates[rg1[2]]
    start2 <- dates[rg2[1]]
    end2 <- dates[rg2[2]]
    pk <- dates[which.max(gw$MaleFlight)]
    df <- data.frame('Location' = row$Location, 'Year' = yr, 
                     'f_sDate' = start1, 'f_eDate' = end1, 'f_pDate' = pk,
                     'a_sDate' = start2, 'a_eDate' = end2)
    return(df)
  })
  pheno.df <- bind_rows(pheno.lst)
  pheno.df$Year <- yr
  return(pheno.df)
})
pheno.yr.df <- bind_rows(pheno.yr.lst)
pheno.yr.df$Location <- factor(pheno.yr.df$Location, levels = loc.ord)
pheno.df <- pheno.yr.df

#write.csv(pheno.df, 'data/pheno_results.csv', row.names = FALSE)

##### Plot Captures vs. Pheno Estimates #####
ggplot(data = pheno.df) + 
  geom_segment(aes(x = a_sDate, xend = a_eDate, 
                   y = factor(Location), yend = factor(Location)),
               alpha = 0.25, col = 'red', size = 1) +
  geom_segment(aes(x = f_sDate, xend = f_eDate,
                   y = factor(Location), yend = factor(Location)),
               alpha = 0.75, col = 'red', size = 1) +
  geom_point(data = captures.df,
             aes(x = Date, y = factor(Location), size = Captures)) +
  facet_wrap(vars(Year), scales = 'free_x') +
  theme_minimal()


##### Calculate Accuracy #####
pheno.df.orig <- pheno.df
date.cols <- c('f_sDate', 'f_eDate', 'f_pDate', 'a_sDate', 'a_eDate')
newcols <- as.data.frame(lapply(date.cols, function(nm) {
  col <- pheno.df[,nm]
  new <- as.Date(col)
  return(new)
}))
names(newcols) <- date.cols
pheno.df[,date.cols] <- newcols

accuracy.lst <- lapply(1:nrow(pheno.df), function(r) {
  row <- pheno.df[r,]
  loc <- row$Location
  yr <- row$Year
  start <- row$f_sDate
  end <- row$f_eDate
  sub <- subset(captures.df, Year == yr & Location == loc)
  sub$Date <- as.Date(sub$Date)
  #tot <- ifelse(nrow(sub) > 0, sum(sub$Captures), NA)
  tot <- sum(sub$Captures)
  sub2 <- subset(sub, between(Date, start, end))
  cap <- ifelse(nrow(sub2) > 0, sum(sub2$Captures), 0)
  return(data.frame('captures' = cap, 'total' = tot))
})
accuracy.df <- bind_rows(accuracy.lst)
pheno.df$accuracy <- accuracy.df$captures/accuracy.df$total
pheno.df$captures <- accuracy.df$captures
pheno.df$total <- accuracy.df$total
pheno.df$Longitude <- sapply(pheno.df$Location, function(loc) {
  coords.ag$Longitude[coords.ag$Location == loc]
})
pheno.df$Latitude <- sapply(pheno.df$Location, function(loc) {
  coords.ag$Latitude[coords.ag$Location == loc]
})

pheno.df <- subset(pheno.df, total > 0)

ggplot(data = pheno.df) +
  geom_bar(aes(x = accuracy, y = Location, fill = total), stat = 'identity') +
  scale_fill_gradient(trans = 'log') +
  facet_wrap(vars(Year), nrow = 1) +
  theme_minimal()

# ggplot(data = pheno.df) +
#   geom_point(aes(x = Longitude, y = Latitude, size = accuracy, col = accuracy
#                  ), alpha = 0.4) +
#   # geom_label(aes(x = Longitude, y = Latitude, label = Location), 
#   #            nudge_x = 1, nudge_y = 1) +
#   facet_wrap(vars(Year), scales = 'fixed') +
#   theme_minimal()

##### Take out sites with suspected events #####
full.df <- read.csv("data/all_obs.csv")
full.ll <- subset(full.df, select = c('Longitude', 'Latitude',
                                      'Year', 'Province', 'Location.orig'))
full.ll.nd <- full.ll[!duplicated(full.ll),]

bad.locs <- unique(subset(full.df, out.ind == 'outlier')$locIndex)
normal.df <- subset(full.df, !(locIndex %in% bad.locs))

normal.sub <- subset(full.df, select = c(Date, Captures, pred, Period, 
                                           Location.orig, Year))
normal.sub.ag <- aggregate(data = normal.sub,
                           cbind(Captures, pred) ~ Date + Location.orig + Year,
                           sum)
normal.sub.ag$index <- paste0(normal.sub.ag$Location.orig, '_', 
                              normal.sub.ag$Year)
normal.sub.ag$Date <- as.Date(normal.sub.ag$Date)
trap.peaks.lst <- lapply(unique(normal.sub.ag$index), function(ind) {
  sub <- subset(normal.sub.ag, index == ind)
  w <- which.max(sub$pred)
  return(sub[w,])
})
trap.peaks <- bind_rows(trap.peaks.lst)

pheno.peaks <- subset(pheno.df, select = c(Location, Year, f_pDate, accuracy))
names(pheno.peaks)[1] <- 'Location.orig'

compare.peaks <- merge(trap.peaks, pheno.peaks)
compare.peaks$diff <- as.numeric(compare.peaks$Date - compare.peaks$f_pDate)

compare.peaks.ll <- merge(compare.peaks, full.ll.nd)
#write.csv(compare.peaks.ll, 'data/compare_peaks.csv', row.names = FALSE)
