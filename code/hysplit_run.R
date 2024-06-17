library(tidyverse)
library(splitr)
library(parallel)
library(lutz)

source('code/hysplit_trajectory_new2.R')
source('code/trajectory_read.R')
source('code/util_file.R')
## 96 cores

o.points <- read.csv('data/origins_yearly.csv')
o.points <- o.points[!duplicated(o.points),]
o.points$ll <- paste0(o.points$Longitude, '_', o.points$Latitude)

imm.events <- read.csv('data/imm_events.csv')
imm.dates <- as.Date(unique(imm.events$trapdate))

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

combine.lst.orig <- date.vl(s.imm.dates)

### Here, need to add in s.imm.dates that were missing before "realdate" ###
imm.events$realdate <- as.Date(mapply(function(trapdate, period) {
  ifelse(period %in% c('Afternoon', 'Evening'), trapdate - 1, trapdate)
}, as.Date(imm.events$trapdate), imm.events$Period))

imm.dates.new <- as.Date(unique(imm.events$realdate)) 
s.imm.dates.new <- sort(imm.dates.new)

leftover <- s.imm.dates.new[!(s.imm.dates.new %in% s.imm.dates)]
s.imm.dates.new2 <- c(s.imm.dates, leftover)

c.lst <- date.vl(leftover, int = length(s.imm.dates))
c.lst <- append(c.lst, list(162))
c.lst[[18]] <- 161
combine.lst <- append(combine.lst.orig, c.lst)
####

cl.ind <- unlist(lapply(1:length(combine.lst), function(x) {
  cl <- combine.lst[[x]]
  rep(x, length(cl))
}))

index.key <- data.frame('date' = s.imm.dates.new2, 'index' = cl.ind)
# write.csv(index.key, 'data/index_key.csv', row.names = FALSE)

## if download.file times out, run:
##   options(timeout = max(300, getOption("timeout")))

o.points <- o.points[order(o.points$Longitude),]
o.points <- o.points[order(o.points$Latitude),]

dirpath <- paste0(getwd(), '/meteorology')

cl <- makeCluster(80)
clusterEvalQ(cl, {
  library(tidyverse)
  library(splitr)
})
clusterExport(cl, c('o.points', 's.imm.dates.new2', 
                    'dirpath', 'combine.lst'))
traj.lst <- parLapply(cl, 1:nrow(o.points), function(x) {
  for(i in 1:length(combine.lst)) {
    inds <- combine.lst[[i]]
    dates <- s.imm.dates.new2[inds]
    yr <- unique(year(dates))
    y.o.points <- subset(o.points, Year == yr)
    e.path <- paste0(dirpath, '/ed', x)
    dir.create(path = e.path)
    traj <- hysplit_trajectory_new2(lat = o.points$Latitude[x],
                               lon = o.points$Longitude[x],
                               duration = 9,
                               days = s.imm.dates.new2[inds] + 1,
                               height = c(300, 600, 900),
                               daily_hours = c(1, 3),
                               met_type = 'nam12',
                               extended_met = TRUE, 
                               met_dir = dirpath,
                               exec_dir = e.path,
                               clean_up = TRUE,
                               config = list(vbug = 2.5))
    write.csv(traj, paste0('code/output/hysplit_output/traj', i, '_', x, '.csv'),
              row.names = FALSE)
  
  }
})
stopCluster(cl)


