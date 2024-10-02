library(tidyverse)
library(sf)

index.key <- read.csv('data/index_key.csv')
names(index.key) <- c('date', 'dateIndex', 'Year')

##### Find Intersections #####
e.pts <- read_rds('code/output/event_points.rds')
e.buf <- read_rds('code/output/event_buffers.rds')

index.key.orig <- index.key
#index.key <- subset(index.key, dateIndex <= 21)

traj.lst <- list()
for (ind in 1:max(index.key$dateIndex)) {
  print(ind)
  path <- 'code/output/'
  file1 <- paste0(path, 'trajectory_lines', ind, '.rds')
  t.lines <- read_rds(file1)
  file2 <- paste0(path, 'all_trajectories', ind, '.csv')
  traj.df <- read.csv(file2)
  traj.df$trapdate_i <- as.Date(traj.df$trapdate_i)
  
  buffers <- subset(e.buf, dateIndex == ind)
  if (nrow(buffers) == 0) {next}
  
  intersections <- st_intersects(buffers, t.lines)
  
  pts.lst <- lapply(1:nrow(buffers), function(r) {
    row <- buffers[r,]
    date <- row$trajdate 
    
    w2 <- intersections[[r]]
    
    traj.sub <- subset(traj.df, (index %in% w2) & trapdate_i == date)
    if (nrow(traj.sub) > 0) {
      traj.sub$buffer <- r
    }
    return(traj.sub)
  })
  pts.df <- bind_rows(pts.lst)
  if (nrow(pts.df) > 0) {
    pts.df$dateIndex <- ind
  }
  traj.lst <- append(traj.lst, list(pts.df))
}
int.traj.df <- bind_rows(traj.lst)
# write.csv(int.traj.df, 'code/output/intersecting_trajectories.csv',
#           row.names = FALSE)
