library(tidyverse)
library(splitr)
library(parallel)
library(lutz)
## 96 cores

imm.events <- read.csv('data/imm_events.csv')
o.points <- read.csv('data/origins_yearly.csv')

source('code/util_file.R')
source('code/trajectory_read.R')
source('code/hysplit_trajectory_new2.R')

tzs <- tz_lookup_coords(lat = imm.events$Latitude, lon = imm.events$Longitude, 
                        method = 'accurate')
imm.events$timezone <- tzs
imm.events$start.hr <- sapply(imm.events$Period, function(p) {
  ifelse(p == 'Afternoon', 12, 
         ifelse(p == 'Evening', 18,
                ifelse(p == 'Night', 24, 30)))
})

dt.lst <- lapply(1:nrow(imm.events), function(r) {
  row <- imm.events[r,]
  dt <- as.POSIXct(paste0(row$Date, ' 00:00'),
                   format = '%Y-%m-%d %H:%M',
                   tz = row$timezone)
})

dt.per <- sapply(1:length(dt.lst), function(i) {
  dt <- dt.lst[[i]]
  s.hr <- imm.events$start.hr[i]
  new <- dt + hours(s.hr)
  as.character(with_tz(new, 'GMT'))
})

dt.per.full <- as.POSIXct(dt.per, tz =)
imm.events$trajdate <- as.Date(dt.per)
imm.events$start.hr.gmt <- hour(dt.per.full)
imm.events.orig <- imm.events

imm.dates <- unique(as.Date(imm.events.orig$trajdate))

s.imm.dates <- sort(imm.dates)

date.vl <- function(alldates, int = 0) {
  date.lst <- lapply(alldates, function(x) {
    return(x + days(-1:5))
  })
  combine.lst.initial <- list()
  ind <- vector()
  for (i in 1:length(date.lst)) {
    ind.new <- c(ind, i)
    dates <- as.Date(unlist(date.lst[ind.new]))
    len <- length(unique(dates))
    y.len <- length(unique(year(dates)))
    if (len > 17 | y.len > 1 | diff(range(alldates[ind.new])) > 10) {
      combine.lst.initial <- append(combine.lst.initial, list(ind + int))
      ind <- i
    }
    else {ind <- ind.new}
  }
  combine.lst.orig <- append(combine.lst.initial, list(ind + int))
  return(combine.lst.orig)
}

combine.lst <- date.vl(s.imm.dates)
####

cl.ind <- unlist(lapply(1:length(combine.lst), function(x) {
  cl <- combine.lst[[x]]
  rep(x, length(cl))
}))

index.key <- data.frame('date' = s.imm.dates, 'index' = cl.ind)
index.key$Year <- year(index.key$date)
# write.csv(index.key, 'data/index_key.csv', row.names = FALSE)

## if download.file times out, run:
##   options(timeout = max(300, getOption("timeout")))

o.points <- o.points[order(o.points$Longitude),]
o.points <- o.points[order(o.points$Latitude),]

dirpath <- paste0(getwd(), '/meteorology')

max.ind <- 0
for (yr in 2018:2022) {
  print(yr)
  y.o.points <- subset(o.points, Year == yr)
  ind.df <- subset(index.key, Year == yr)
  ind.vec <- unique(ind.df$index)
  combine.lst.sub <- combine.lst[ind.vec]
  
  cl <- makeCluster(80)
  clusterEvalQ(cl, {
    library(tidyverse)
    library(splitr)

    source('code/hysplit_trajectory_new2.R')
    source('code/trajectory_read.R')
    source('code/util_file.R')
  })
  clusterExport(cl, c('y.o.points', 's.imm.dates', 'yr', 
                      'dirpath', 'combine.lst.sub', 'max.ind'))
  traj.lst <- parLapply(cl, 1:nrow(y.o.points), function(x) {
    for(i in 1:length(combine.lst.sub)) {
      inds <- combine.lst.sub[[i]]
      dates <- s.imm.dates[inds]
      e.path <- paste0(dirpath, '/ed', x, '_', yr)
      dir.create(path = e.path)
      cfg <- list(KMSL = 0,
                  tm_tpot = 1,
                  tm_tamb = 1,
                  tm_rain = 1,
                  tm_mixd = 1,
                  tm_relh = 1,
                  tm_terr = 1,
                  tm_dswf = 1,
                  vbug=2.5)
      traj1 <- hysplit_trajectory_new2(lat = y.o.points$Latitude[x],
                                      lon = y.o.points$Longitude[x],
                                      duration = 9,
                                      days = s.imm.dates[inds],
                                      height = c(300, 600, 900),
                                      daily_hours = c(1, 3),
                                      met_type = 'nams',
                                      extended_met = TRUE, 
                                      met_dir = dirpath,
                                      exec_dir = e.path,
                                      clean_up = TRUE,
                                      config = cfg)
      
      traj2 <- hysplit_trajectory_new2(lat = y.o.points$Latitude[x],
                                      lon = y.o.points$Longitude[x],
                                      duration = 9,
                                      days = s.imm.dates[inds] - 1,
                                      height = c(300, 600, 900),
                                      daily_hours = 23,
                                      met_type = 'nams',
                                      extended_met = TRUE, 
                                      met_dir = dirpath,
                                      exec_dir = e.path,
                                      clean_up = TRUE,
                                      config = cfg)
      traj1$run <- traj1$run + max(traj2$run)
      traj <- rbind(traj2, traj1)
      
      write.csv(traj, paste0('code/output/hysplit_output/traj',
                             i + max.ind, '_', x, '.csv'),
                row.names = FALSE)
      
    }
  })
  stopCluster(cl)
  max.ind <- max(ind.vec)
}
