library(tidyverse)
library(parallel)
library(lutz)
# devtools::install_github('studensk/sbwtraj')
library(sbwtraj)
library(sf)
## 96 cores

imm.events <- read.csv('data/imm_events.csv')
o.points <- read.csv('data/origins_yearly.csv')
# o.points.orig <- read.csv('data/origins_yearly.csv')
# u.points <- o.points.orig %>%
#   select(Longitude, Latitude) %>%
#   distinct() %>%
#   mutate(start.index = 1:nrow(.))
# o.points <- merge(o.points.orig, u.points)


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

dt.per.full <- as.POSIXct(dt.per)
imm.events$trajdate <- as.Date(dt.per)
imm.events$start.hr.gmt <- hour(dt.per.full)

imm.events.sf <- st_as_sf(imm.events,
                          coords = c('Longitude', 'Latitude'),
                          crs = "+proj=longlat +datum=WGS84")
imm.events.buf <- st_buffer(imm.events.sf, dist = 10000)
o.points.sf <- st_as_sf(o.points,
                          coords = c('Longitude', 'Latitude'),
                          crs = "+proj=longlat +datum=WGS84")

rdf.lst <- lapply(1:nrow(imm.events.sf), function(r) {
  row <- imm.events.sf[r,]
  year <- unique(row$Year)
  o.yr.sf <- subset(o.points.sf, Year == year)
  o.yr <- subset(o.points, Year == year)
  
  dists <- as.numeric(st_distance(row, o.yr.sf)[1,])
  w <- which(dists <= 1050*1000)
  origins <- o.yr[w,]
  
  datetime <- as.Date(row$trapdate) + hours(c(-1, 1, 3))
  fly.df <- expand.grid('datetime' = datetime, 'height' = c(300, 600, 900)) %>%
    mutate(hour = hour(datetime),
           date = date(datetime)) %>%
    merge(origins) %>%
    rename(lon = Longitude, lat = Latitude) %>%
    select(-datetime, -Year) 
  return(fly.df)
})

rdf.full <- bind_rows(rdf.lst) %>%
  distinct() %>%
  arrange(date)

# write.csv(rdf.full , 'data/full_rundf.csv', row.names = FALSE)

u.dates <- unique(rdf.full$date)

for (i in 1:length(u.dates)) {
  print(i)
  d <- u.dates[i]
  rdf.sub <- subset(rdf.full, date == d)
  e.dir <- file.path('code/output/hysplit_output')
  traj.df <- hysplit_trajectory(rdf.sub, 
                                met_dir = file.path(getwd(), 'meteorology'), 
                                exec_dir = file.path(getwd(), e.dir),
                                traj_name = paste0('date_', d))
}

out.path <- 'code/output/hysplit_output'
t.files.orig <- list.files(out.path, pattern = 'date')
file.date <- sapply(t.files.orig, function(x) {
  substr(x, 6, nchar(x) - 4)
}, USE.NAMES = FALSE)
ord <- order(as.Date(file.date))
t.files <- t.files.orig[ord]

traj.lst <- lapply(1:length(t.files), function(i) {
  file <- t.files[i]
  path <- file.path(out.path, file)
  traj <- read.csv(path) %>%
    mutate(receptor = paste0(i, '_', receptor),
           date_i = as.Date(traj_dt_i))
})
traj.df <- bind_rows(traj.lst)
# traj.df.orig <- traj.df
# traj.df <- traj.df.orig %>%
#   mutate(lon_i = round(lon_i, 2), 
#          lat_i = round(lat_i, 2))

rdf.join.lst <- lapply(1:length(rdf.lst), function(ind) {
  rdf <- rdf.lst[[ind]]
  traj.sub <- subset(traj.df, date_i %in% unique(rdf$date))
  
  rdf.new <- rdf %>%
    rename_with(~paste0(.x, '_i')) %>%
    mutate(lon_i = round(lon_i, 3),
           lat_i = round(lat_i, 3))
  rdf.mg <- merge(rdf.new, traj.sub) %>%
    arrange(hour_along)
  rdf.i <- rdf.mg %>%
    select(contains('_i'), receptor) %>%
    distinct()
  rdf.st <-
    st_as_sf(x = rdf.mg, coords = c("lon", "lat"), crs = "epsg:4326") %>%
    group_by(receptor) %>%
    summarize(do_union = FALSE) %>%
    filter(st_geometry_type(.) == "MULTIPOINT") %>%
    st_cast("LINESTRING") %>%
    merge(rdf.i)
  
  event <- imm.events.buf[ind,]
  intersection <- st_intersects(event, rdf.st)
  rdf.int <- rdf.st[intersection[[1]],]
  rdf.int$imm.event <- ind
  
  file.name <- paste0('intersectons', ind, '.rds')
  path <- file.path('code/output/hysplit_output/intersections', file.name)
  saveRDS(rdf.int, file = path)
  return(rdf.int)
})

rdf.join <- bind_rows(rdf.join.lst)
saveRDS(rdf.join, 'code/output/intersecting_trajectories_sbwtraj.rds')

