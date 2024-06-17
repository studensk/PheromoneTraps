library(tidyverse)
library(abind)
library(BioSIM)
library(stringr)

imm.df.orig <- read.csv('data/all_imm_event_trajinfo.csv')
imm.df.int <- subset(imm.df.orig, intersection)
e.pts <- read.csv('data/imm_pts_proptraj.csv')
imm.df <- merge(imm.df.int, e.pts)

traj.df <- read.csv('code/output/intersection_summary.csv')
pheno.df <- read.csv('data/origin_phenology.csv')

int.traj.orig <- read.csv('code/output/intersecting_trajectories.csv')
int.traj.orig$full.index <- paste0(int.traj.orig$dateIndex, '_', 
                                   int.traj.orig$buffer)

immdate1 <- names(sort(table(imm.df$trajdate), decreasing = TRUE))[2]

## Suspected imm events on date immdate1
imm.df1 <- subset(imm.df, trajdate == immdate1)

## Summaries of trajectories intersecting events in imm.df1
traj.df1 <- subset(traj.df, full.index %in% unique(imm.df1$full.index))

## Full trajectories corresponding to rows in traj.df1 
traj.df.int <- subset(int.traj.orig, full.index %in% traj.df1$full.index & 
                        index %in% traj.df1$index)

## Phenology at all origins on date immdate1
pheno.df1 <- subset(pheno.df, Date == immdate1)
stages <- c('L2o', 'L2', 'L3', 'L4', 'L5', 'L6', 
            'Pupae', 'Adults', 'DeathAdult')
pheno.df1.sub <- subset(pheno.df1, select = c(stages, 'KeyID', 
                                              'Longitude', 'Latitude',
                                              'Year', 'Date', 
                                              'AverageInstar'))

origin.lst <- lapply(unique(imm.df1$full.index), function(x) {
  sub <- subset(traj.df1, full.index == x, select = c(lon_i, lat_i))
  sub.nd <- sub[!duplicated(sub),]
  names(sub.nd) <- c('Longitude', 'Latitude')
  
  
  mg <- merge(sub.nd, pheno.df1.sub, all.y = FALSE, all.x = TRUE)
  return(unique(mg$KeyID))
})
main.origins <- origin.lst[[3]]

origin.int.lst <- lapply(c(1:2, 4:length(origin.lst)), function(ind) {
  intersect(main.origins, origin.lst[[ind]])
})


origins.orig <- subset(traj.df1, select = c('lon_i', 'lat_i'))
names(origins.orig) <- c('Longitude', 'Latitude')
origins <- origins.orig[!duplicated(origins.orig),]
origin.pheno <- merge(origins, pheno.df1, all.y = FALSE)


origins <- unique(origin.pheno$KeyID)
combos.full <- expand.grid(origins, origins)
v1 <- as.numeric(combos.full[,1])
v2 <- as.numeric(combos.full[,2])
w <- which(v1 < v2)
combos <- combos.full[w,]
sub <- pheno.df1
props <- sapply(1:nrow(combos), function(i) {
  cb <- combos[i,]
  p.i <- sub[sub$KeyID == cb$Var1, stages]
  p.j <- sub[sub$KeyID == cb$Var2, stages]
  mat <- rbind(p.i, p.j)
  prop <- sum(apply(mat, 2, min))
  return(prop)
})
combos$prop <- props

##### Hybrid method; phenology comparison for traps #####
###    vs actual similarity in captures
datefun <- function(year, month, day) {
  dchar <- paste0(year, '-', str_pad(month, 2, pad = '0'),
                  '-', str_pad(day, 2, pad = '0'))
  return(dchar)
}

trap.info.full.orig <- read.csv('data/all_obs.csv')
trap.info.full <- subset(trap.info.full.orig, out.ind == 'normal')

ti.ll <- subset(trap.info.full, select = c(locIndex, Longitude,
                                           Latitude, Year))
ti.ll <- ti.ll[!duplicated(ti.ll),]
all.lis.orig <- unique(ti.ll$locIndex)

ti.ag <- aggregate(data = trap.info.full, Captures ~ locIndex, sum)
null.lis <- subset(ti.ag, Captures == 0)$locIndex
all.lis <- all.lis.orig[!(all.lis.orig %in% null.lis)]

pheno.full.lst <- lapply(all.lis, function(id) {
  sub <- subset(ti.ll, locIndex == id)
  yr <- as.numeric(strsplit(id, split = '_')[[1]][2])
  pheno <- generateWeather(modelNames = 'Spruce_Budworm_Biology',
                           fromYr = yr, toYr = yr, id = id,
                           latDeg = unique(sub$Latitude), 
                           longDeg = unique(sub$Longitude))[[1]]
})
pheno.full.df <- bind_rows(pheno.full.lst)
pheno.full.df$Date <- as.Date(datefun(pheno.full.df$Year, 
                                      pheno.full.df$Month, 
                                      pheno.full.df$Day))
#write.csv(pheno.full.df, 'data/biosim_trap_pheno.csv', row.names = FALSE)


combo.lst <- lapply(2018:2022, function(y) {
  g <- grep(y, all.lis)
  ali <- all.lis[g]
  
  combos.full <- expand.grid(ali, ali)
  v1 <- as.numeric(combos.full[,1])
  v2 <- as.numeric(combos.full[,2])
  w.dup <- which(v1 < v2)
  combos.df <- combos.full[w.dup,]
  return(combos.df)
})
combos <- bind_rows(combo.lst)


get.stage.props <- function(id, window) {
  pheno <- subset(pheno.full.df, KeyID == id)
  pheno.sub <- subset(pheno, between(Date, window[1], window[2]))
  pheno.sub.prop <- t(apply(pheno.sub[,stages], 1, function(x) {x/sum(x)}))
  return(pheno.sub.prop)
}

get.ai <- function(id, window) {
  pheno <- subset(pheno.full.df, KeyID == id)
  pheno.sub <- subset(pheno, between(Date, window[1], window[2]))
  return(pheno.sub$AverageInstar)
}

av.age.vec <- vector()
overlap.vec <- vector()
diff.cap.int.vec <- vector()
diff.pred.int.vec <- vector()
diff.peak.vec <- vector()
for (i in 1:nrow(combos)) {
  ids <- c(combos$Var1[i], combos$Var2[i])
  trap.info <- subset(trap.info.full, locIndex %in% ids)
  w.caps <- which(trap.info$Captures > 0)
  if (length(w.caps) == 0) {
    av.age.vec <- c(av.age.vec, NA)
    overlap.vec <- c(overlap.vec, NA)
    diff.cap.int.vec <- c(diff.cap.int.vec, NA)
    diff.pred.int.vec <- c(diff.pred.int.vec, NA)
    diff.peak.vec <- c(diff.peak.vec, NA)
    next
  }
  dates <- as.Date(range(trap.info$Date[w.caps]))
  dates.full <- data.frame('trapdate' = seq(dates[1], dates[2], by = 'day'))
  
  psp.lst <- lapply(1:2, function(j) {
    get.stage.props(id = ids[j], window = dates)
  })
  psp.arr <- abind(psp.lst, along = 3)
  min.mat <- apply(psp.arr, 1:2, min)
  overlap <- mean(apply(min.mat, 1, sum))
  overlap.vec <- c(overlap.vec, overlap)
  
  ai.lst <- lapply(1:2, function(j) {
    get.ai(id = ids[j], window = dates)
  })
  av.age <- abs(mean(ai.lst[[1]] - ai.lst[[2]]))
  av.age.vec <- c(av.age.vec, av.age)
  
  ag1 <- aggregate(data = trap.info, cbind(Captures, pred) ~ 
                     trapdate + locIndex, sum)
  ag1$trapdate <- as.Date(ag1$trapdate)
  maxlen <- max(table(ag1$locIndex))
  
  cs.lst <- lapply(unique(ag1$locIndex), function(li) {
    sub <- subset(ag1, locIndex == li)
    sub <- sub[order(sub$trapdate),]
    mg <- merge(dates.full, sub, all.x = TRUE)
    w.na <- which(is.na(mg$locIndex))
    mg$locIndex[w.na] <- unique(sub$locIndex)
    mg$Captures[w.na] <- 0
    mg$pred[w.na] <- 0
    
    mg$csCaptures <- cumsum(mg$Captures)
    mg$pCaptures <- mg$csCaptures/sum(mg$Captures)
    
    mg$csPred <- cumsum(mg$pred)
    mg$pPred <- mg$csPred/sum(mg$pred)
    mg$max.ind <- rep(0, nrow(mg))
    mg$max.ind[which.min(abs(mg$pPred - 0.5))] <- 1
    
    return(mg)
  })
  cs.df <- bind_rows(cs.lst)
  diff.cap.int <- sum(abs(cs.lst[[1]]$pCaptures - 
                        cs.lst[[2]]$pCaptures))/nrow(dates.full)
  diff.cap.int.vec <- c(diff.cap.int.vec, diff.cap.int)
  
  diff.pred.int <- sum(abs(cs.lst[[1]]$pCaptures - 
                        cs.lst[[2]]$pCaptures))/nrow(dates.full)
  diff.pred.int.vec <- c(diff.pred.int.vec, diff.pred.int)
  
  diff.peak <- abs(as.numeric(diff(subset(cs.df, max.ind == 1)$trapdate)))
  diff.peak.vec <- c(diff.peak.vec, diff.peak)
}

comp.df <- data.frame('overlap' = overlap.vec,
                      'av.age' = av.age.vec,
                      'cap.int' = diff.cap.int.vec,
                      'pred.int' = diff.pred.int.vec,
                      'diff.peak' = diff.peak.vec)

comp.df$loc1 <- combos$Var1
comp.df$loc2 <- combos$Var2

#write.csv(comp.df, 'data/pheno_capture_compare.csv', row.names = FALSE)

pairs(comp.df[,1:4], pch = 20, gap = 0)
