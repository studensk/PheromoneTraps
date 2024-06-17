library(tidyverse)
library(sf)
library(sp)
library(parallel)
library(lutz)

imm.events <- read.csv('data/imm_events.csv')

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

index.key <- read.csv('data/index_key.csv')
names(index.key) <- c('date', 'dateIndex')
index.key$trajdate <- as.Date(index.key$date) + 1

imm.events <- merge(imm.events.orig, index.key)
  
o.points <- read.csv('data/origins.csv')
o.points <- o.points[!duplicated(o.points),]

##### Create sf Objects #####
imm.sf <- st_as_sf(x = imm.events,
                   coords = c('Longitude', 'Latitude'),
                   crs = 4326)
imm.sf.buffer <- st_buffer(imm.sf, dist = 10000)

write_rds(imm.sf, 'code/output/event_points.rds')
write_rds(imm.sf.buffer, 'code/output/event_buffers.rds')

path <- 'code/output/'
t.path <- paste0(path, 'hysplit_output/')
max.ind <- max(index.key$dateIndex)

for (i in 1:max.ind) {
  print(i)
  ptm.start <- proc.time()
  all.traj.files <- list.files(t.path, pattern = paste0('traj', i, '_'))
  
  traj.lst <- lapply(all.traj.files, function(file) {
    full <- paste0(t.path, file)
    read.csv(full)
  })
  traj.df <- bind_rows(traj.lst)
  rm(traj.lst)
  traj.df$traj_dt_i <- as.POSIXct(traj.df$traj_dt_i)
  traj.df$date_i <- as.Date(traj.df$traj_dt_i)
  traj.df$hour_i <- hour(traj.df$traj_dt_i)
  
  dayvec <- rep(0, nrow(traj.df)) 
  dayvec[traj.df$hour_i == 11] <- 1
  
  # traj.df$trapdate_i <- as.Date(mapply(function(date, hour) {
  #   ifelse(hour == 11, date + 1, date)
  # }, traj.df$date_i, traj.df$hour_i))
  
  traj.df$trapdate_i <- traj.df$date_i + dayvec
  
  bin.ind <- as.numeric(traj.df$hour_along == 0)
  cs.bin.ind <- cumsum(bin.ind)
  
  # tdf.ind <- paste0(traj.df$date_i, '_',
  #                   traj.df$hour_i, '_',
  #                   traj.df$lat_i, '_',
  #                   traj.df$lon_i, '_', 
  #                   traj.df$height_i)
  
  traj.df$index <- cs.bin.ind
  write.csv(traj.df, paste0(path, 'all_trajectories', i, '.csv'), 
            row.names = FALSE)
  
  ptm.end <- proc.time() - ptm.start
  print(ptm.end)
}

cols <- c('lat', 'lon', 'index')
at.lst <- lapply(1:max.ind, function(i) {
  path <- 'code/output/'
  file <- paste0('all_trajectories', i, '.csv')
  data <- read.csv(paste0(path, file))
  data[,cols]
})

max.ind <- 10

cl <- makeCluster(max.ind)
clusterEvalQ(cl, {
  library(tidyverse)
  library(sf)
})
clusterExport(cl, 'at.lst')
parLapply(cl, 1:max.ind, function(i) {
  #traj.df <- read.csv('code/output/all_trajectories', i, '.csv')
  traj.df <- at.lst[[i]]
  traj.df.st <-
    st_as_sf(x = traj.df, coords = c("lon", "lat"), crs = "epsg:4326") %>%
    group_by(index) %>%
    ## old
    #summarize() %>%
    ## new
    summarize(do_union = FALSE) %>%
    filter(st_geometry_type(.) == "MULTIPOINT") %>%
    st_cast("LINESTRING")
  
  path <- 'code/output/'

  write_rds(traj.df.st, paste0(path, 'trajectory_lines', i, '.rds'))
})
stopCluster(cl)


