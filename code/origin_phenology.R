### To install BioSIM if necessary:
# require(remotes)
# install_github("RNCan/BioSimClient_R")

library(tidyverse)
library(BioSIM)
library(stringr)

imm.traj <- read.csv('data/all_imm_event_trajinfo.csv')
imm.traj$trajdate <- as.Date(imm.traj$trajdate)
r.dates <- range(imm.traj$trajdate)

all.dates <- seq(r.dates[1], r.dates[2], by = 'day')
yo.points <- read.csv('data/origins_yearly.csv')
yo.points$index <- 1:nrow(yo.points)

stages <- c('L2o', 'L2', 'L3', 'L4', 'L5', 'L6', 
            'Pupae', 'Adults', 'DeathAdult')

year.lst <- list()
for (y in 2018:2022) {
  print(y)
  points <- subset(yo.points, Year == y)
  pheno.lst <- list()
  for (r in 1:nrow(points)) {
    print(r)
    gw <- generateWeather("Spruce_Budworm_Biology", y, y, 
                          latDeg = points$Latitude[r], 
                          longDeg = points$Longitude[r],
                          id = paste0('origin', points$index[r]))
    pheno.lst <- append(pheno.lst, list(gw[[1]]))
  }
  pheno.df <- bind_rows(pheno.lst)
  pheno.df.summer <- subset(pheno.df, Month %in% 6:8)
  pheno.df.summer$Date <- as.Date(paste0(y, '-', 
                                 str_pad(pheno.df.summer$Month, 2, pad = '0'), 
                                 '-',
                                 str_pad(pheno.df.summer$Day, 2, pad = '0')))
  pheno.df.summer[,stages] <- pheno.df.summer[,stages]/
    apply(pheno.df.summer[,stages], 1, sum)
  year.lst <- append(year.lst, list(pheno.df.summer))
}
year.df <- bind_rows(year.lst)
write.csv(year.df, 'data/origin_phenology.csv', row.names = FALSE)


overlap.yr.lst <- lapply(2018:2022, function(y) {
  data <- subset(year.df, Year == y)
  origins <- unique(data$KeyID)
  combos.full <- expand.grid(origins, origins)
  v1 <- as.numeric(combos.full[,1])
  v2 <- as.numeric(combos.full[,2])
  w <- which(v1 < v2)
  combos <- combos.full[w,]
  date.lst <- lapply(sort(unique(data$Date)), function(d) {
    sub <- subset(data, Date == d)
    
    o.mat <- matrix(rep(NA, n^2), nr = n)
    rownames(o.mat) <- origins
    colnames(o.mat) <- origins
    for (i in 1:length(origins)) {
      for (j in 1:length(origins)) {
        o.mat[i,j] <- sum(sapply(stages, function(s) {
          p.i <- sub[sub$KeyID == origins[i],s]
          p.j <- sub[sub$KeyID == origins[j],s]
          return(min(p.i, p.j))
        }))
      }
    }
    return(o.mat)
  })
  return(date.lst)
})

overlap.yr.lst <- lapply(2018:2022, function(y) {
  data <- subset(year.df, Year == y)
  origins <- unique(data$KeyID)
  combos.full <- expand.grid(origins, origins)
  v1 <- as.numeric(combos.full[,1])
  v2 <- as.numeric(combos.full[,2])
  w <- which(v1 < v2)
  combos <- combos.full[w,]
  n <- length(origins)
  date.lst <- lapply(unique(data$Date), function(d) {
    sub <- subset(data, Date == d)
    props <- sapply(1:nrow(combos), function(i) {
      cb <- combos[i,]
      p.i <- sub[sub$KeyID == cb[1], stages]
      p.j <- sub[sub$KeyID == cb[2], stages]
      mat <- rbind(p.i, p.j)
      prop <- sum(apply(mat, 2, min))
      return(prop)
    })
   
    return(o.mat)
  })
  return(date.lst)
})

