library(tidyverse)
library(sf)

index.key <- read.csv('data/index_key.csv')
names(index.key) <- c('date', 'dateIndex')
index.key$trajdate <- as.Date(index.key$date) + 1

y.origins <- read.csv('data/origins_yearly.csv')
names(y.origins)[1:2] <- c('lon_i', 'lat_i')

##### Find Intersections #####
e.pts <- read_rds('code/output/event_points.rds')
e.buf <- read_rds('code/output/event_buffers.rds')

index.key.orig <- index.key

total.traj <- vector()
int.count <- list()
for (ind in 1:max(index.key$dateIndex)) {
  print(ind)
  path <- 'code/output/'
  file1 <- paste0(path, 'trajectory_lines', ind, '.rds')
  t.lines <- read_rds(file1)
  file2 <- paste0(path, 'all_trajectories', ind, '.csv')
  traj.df <- read.csv(file2)
  traj.df$trapdate_i <- as.Date(traj.df$trapdate_i)
  yr <- year(traj.df$trapdate_i)[1]
  yo.sub <- subset(y.origins, Year == yr)
  m <- merge(traj.df, yo.sub)
  inds <- sort(unique(m$index))
  
  all.dates <- unique(traj.df$trapdate_i)
  
  buffers <- e.buf
  
  intersections <- st_intersects(buffers, subset(t.lines, index %in% inds))
  
  pts.lst <- sapply(1:nrow(buffers), function(r) {
    row <- buffers[r,]
    w2 <- intersections[[r]]
    traj.sub <- subset(traj.df, index %in% w2)
    return(length(unique(traj.sub$trapdate_i)))
  })
  int.count <- append(int.count, list(pts.lst))
  total.traj <- c(total.traj, length(all.dates))
}

c.mat <- do.call('cbind', int.count)
prop.days <- apply(c.mat, 1, sum)/sum(total.traj)

e.pts$prop.days <- prop.days
e.pts <- subset(as.data.frame(e.pts), select = -geometry)


write.csv(e.pts, 'data/imm_pts_proptraj.csv', row.names = FALSE)
