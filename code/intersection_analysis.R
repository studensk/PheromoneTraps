library(tidyverse)
library(sf)
library(geosphere)

#o.points <- read.csv('data/origins.csv')
y.origins <- read.csv('data/origins_yearly.csv')
y.origins <- y.origins[!duplicated(y.origins),]
names(y.origins) <- c('lon_i', 'lat_i', 'Year')

int.traj.orig <- read.csv('code/output/intersecting_trajectories.csv')
int.traj <- int.traj.orig
int.traj$Year <- sapply(int.traj$date_i, function(x) {year(x)})

int.traj <- int.traj[order(int.traj$hour_along),]
int.traj <- int.traj[order(int.traj$index),]
int.traj <- int.traj[order(int.traj$Year),]
int.traj$full.index <- paste0(int.traj$dateIndex, '_', int.traj$buffer)

write.csv(int.traj, 'data/intersecting_trajectories_yearly.csv',
          row.names = FALSE)

e.buf <- read_rds('code/output/event_points.rds')
#e.buf <- read_rds('code/output/event_buffers.rds')
buf.ind.lst <- lapply(unique(e.buf$dateIndex), function(ind) {
  sub <- subset(e.buf, dateIndex == ind)
  new.ind <- paste0(ind, '_', 1:nrow(sub))
  sub$full.index <- new.ind
  return(sub)
})
buf.ind.df <- bind_rows(buf.ind.lst)

index.key <- read.csv('data/index_key.csv')

it.cols <- names(int.traj)
i.cols <- c(it.cols[grep('_i', it.cols)], 'index')

summary.lst <- list()
for (i in sort(unique(index.key$index))) {
  it.sub <- subset(int.traj, dateIndex == i)
  ep.sub <- subset(e.buf, dateIndex == i)
  for (j in 1:nrow(ep.sub)) {
    print(paste0(i, '_', j))
    epj <- ep.sub[j,]
    start.hr <- epj$start.hr.gmt
    
    it.sub2 <- subset(it.sub, buffer == j)
    if (nrow(it.sub2) == 0) {next}
    u.ind <- unique(it.sub2$index)
    
    it.init <- subset(it.sub2, select = i.cols)
    it.init.nd <- it.init[!duplicated(it.init),]
    
    it.sub2$dist <- distGeo(st_coordinates(epj), 
                            as.matrix(it.sub2[,c('lon', 'lat')]))/1000
    ag <- aggregate(data = it.sub2, dist ~ index, which.min)
    names(ag)[2] <- 'int_hour'
    ag$int_hour <- ag$int_hour - 1
    
    traj.info.lst <- lapply(1:nrow(ag), function(r) {
      ind <- ag$index[r]
      hr <- ag$int_hour[r]
      sub <- subset(it.sub2, index == ind & hour_along == hr)
      return(sub[,c('air_temp', 'pressure', 'height', 'dist')])
    })
    traj.info.df.orig <- bind_rows(traj.info.lst)
    traj.info.df <- bind_cols(ag, traj.info.df.orig)
    all.info.orig <- merge(it.init.nd, traj.info.df)
    
    all.info <- all.info.orig
    tj.start.hr <- sapply(all.info$hour_i, function(x) {
      ifelse(x == 11, -1, x)
    })
    all.info$int_hour_gmt <- all.info$int_hour + tj.start.hr
    all.info <- subset(all.info, int_hour_gmt < start.hr + 6)
    if (nrow(all.info) == 0) {next}
    
    all.info$dateIndex <- i
    all.info$buffer <- j
    all.info$full.index <- paste0(i, '_', j)
    
    ntraj <- nrow(all.info)
    
    ll <- subset(all.info, select = c(lat_i, lon_i))
    ll.nd <- ll[!duplicated(ll),]
    u.origins <- nrow(ll.nd)
    
    all.info$n.origins <- u.origins
    all.info$n.traj <- ntraj
    
    all.info$dist.orig <- distGeo(st_coordinates(epj), 
                            as.matrix(all.info[,c('lon_i', 'lat_i')]))/1000
    
    summary.lst <- append(summary.lst, list(all.info))
  }
}
summary.df <- bind_rows(summary.lst)

write.csv(summary.df, 'code/output/intersection_summary.csv',
          row.names = FALSE)

#summary.df <- subset(summary.df.orig, dist < 50)

summary.ag.do <- aggregate(data = summary.df, dist.orig ~ full.index,
                           function(x) {c(min(x), mean(x), max(x))})
do.df <- as.data.frame(summary.ag.do$dist.orig)
names(do.df) <- paste0(c('min.', 'mean.', 'max.'), 'do')
do.df$full.index <- summary.ag.do$full.index

summary.sub <- subset(summary.df, select = c(full.index, n.origins, n.traj))
summary.sub.nd <- summary.sub[!duplicated(summary.sub),]
ss.mg <- merge(summary.sub.nd, do.df)

imm.pts.orig <- merge(buf.ind.df, ss.mg, all.x = TRUE)
imm.pts.orig$intersection <- sapply(imm.pts.orig$n.traj,
                               function(x) {ifelse(is.na(x), FALSE, TRUE)})

yo.points.sf <- st_as_sf(x = y.origins, coords = c('lon_i', 'lat_i'),
                         crs = 4326)
dist.lst <- lapply(2018:2022, function(yr) {
  imm.pts.orig.sub <- subset(imm.pts.orig, Year == yr)
  dists <- st_distance(subset(yo.points.sf, Year == yr), 
                       imm.pts.orig.sub)
  dists <- dists/1000
  nearest.dist <- apply(dists, 2, min)
  imm.pts.orig.sub$nearest.op <- nearest.dist
  return(imm.pts.orig.sub)
})
imm.pts.orig <- bind_rows(dist.lst)

# dists <- st_distance(o.points.sf, imm.pts.orig)
# dists <- dists/1000
# nearest.dist <- apply(dists, 2, min)
#imm.pts.orig$nearest.op <- nearest.dist

ip.ll.mat <- st_coordinates(imm.pts.orig)
ip.ll.df <- as.data.frame(ip.ll.mat)
names(ip.ll.df) <- c('Longitude', 'Latitude')
imm.pts <- bind_cols(imm.pts.orig, ip.ll.df)
imm.pts.df <- subset(as.data.frame(imm.pts), select = -geometry)

write.csv(imm.pts.df, 'data/all_imm_event_trajinfo.csv', 
          row.names = FALSE)


# 
# sub.int.orig <- subset(imm.pts, !is.na(n.traj))
# si.ll.mat <- st_coordinates(sub.int.orig)
# si.ll.df <- as.data.frame(si.ll.mat)
# names(si.ll.df) <- c('Longitude', 'Latitude')
# sub.int <- bind_cols(sub.int.orig, si.ll.df)
# 
# write.csv(sub.int, 'data/new_events.csv', row.names = FALSE)
