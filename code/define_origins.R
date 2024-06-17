library(terra)
library(tidyverse)
library(sp)
library(raster)
library(sf)
library(lutz)

projcode <- '+proj=aea +lat_0=44 +lon_0=-68.5 +lat_1=60 +lat_2=46 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

filename <- 'data/defoliation/TBE_2014_2023.shp'
shape <- vect(filename)

qc.new <- terra::project(shape, crs(projcode))

qc.new.sf <- st_as_sf(qc.new)

grid <- qc.new.sf %>%
  st_make_grid(cellsize = 10000, what = "centers") %>% # grid of points
  st_intersection(qc.new.sf)

ll.grid <- terra::project(vect(grid), crs(shape))

points <- as.data.frame(crds(ll.grid))
names(points) <- c('Longitude', 'Latitude')

write.csv(points, 'data/origins.csv', row.names = FALSE)