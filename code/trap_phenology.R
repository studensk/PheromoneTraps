library(BioSIM)
library(tidyverse)


trap21 <- read.csv('data/traps2021.csv')
names(trap21)[1] <- 'Location'
trap22 <- read.csv('data/traps2022.csv')

trap21$Year <- 2021
trap22$Year <- 2022

colnms <- c('Location', 'Code', 'Longitude', 'Latitude', 'Year')

all.traps <- bind_rows(trap21[,colnms], trap22[,colnms])
all.traps <- as.data.frame(na.omit(all.traps))

pheno.lst <- lapply(1:nrow(all.traps), function(r) {
  row <- all.traps[r,]
  gen.w <- generateWeather(modelNames = 'Spruce_Budworm_Biology',
                           fromYr = row$Year, toYr = row$Year, id = row$Location,
                           latDeg = row$Latitude, longDeg = row$Longitude)
  gw <- gen.w[[1]]
  sub <- subset(gw, L2 > 0 | L3 > 0 | L4 > 0 | L5 > 0 | L6 > 0 | Pupae > 0 | Adults > 0)
  return(sub)
})

pheno.df <- bind_rows(pheno.lst)
write.csv(pheno.df, 'biosim_pheno.csv')
