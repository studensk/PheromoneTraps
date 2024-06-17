library(xlsx)
library(reshape2)
library(tmbstan)
library(TMB)
library(tidyverse)
library(lme4)
library(boot)
library(chillR)
library(BioSIM)
library(lubridate)
library(BioSIM)

##### Prepare Data #####
pheno.df <- read.csv('data/pheno_results.csv')
tempdata <- read.csv('data/TrapWeather_clean.csv')

### Capture Data ###
years <- 2018:2022
sheet_names <- c('Afternoon', 'Evening', 'Night', 'Morning')
trap.lst <- lapply(years, function(year) {
  time.lst <- lapply(sheet_names, function(snm) {
    dat <- xlsx::read.xlsx(paste0('data/traps', year, '.xlsx'), sheetName = snm)
    dat.raw <- dat[1:(nrow(dat)-2),1:(ncol(dat)-2)]
    return(dat.raw)
  })
  names(time.lst) <- sheet_names
  return(time.lst)
})
names(trap.lst) <- as.character(years)

loc.ids <- c('Location', 'Code', 'Longitude', 'Latitude')
all.locs <- bind_rows(lapply(1:length(trap.lst), function(i) {
  x <- trap.lst[[i]]
  dat <- x[[1]][,loc.ids]
  dat$Year <- as.numeric(names(trap.lst)[i])
  return(dat)
}))
gw.lst <- lapply(1:nrow(all.locs), function(r) {
  row <- all.locs[r,]
  gw <- with(row, {generateWeather(modelNames = 'TminTairTmax_Daily', 
                                   fromYr = Year, toYr = Year, id = Location,
                                   latDeg = Latitude, longDeg = Longitude)})[[1]]  
  return(gw)
})
all.locs$DD <- sapply(1:nrow(all.locs), function(r) {
  row <- all.locs[r,]
  gw <- with(row, {generateWeather(modelNames = 'TminTairTmax_Daily', fromYr = Year, toYr = Year, id = Location, latDeg = Latitude, longDeg = Longitude)})[[1]]
  rg <- c(which(gw$Month == 6 & gw$Day == 15), which(gw$Month == 8 & gw$Day == 22))
  dd <- sum(gw[rg[1]:rg[2],'Tair'])
  return(dd)
})

capture.lst <- lapply(as.character(years), function(yr) {
  period.lst <- lapply(sheet_names, function(snm) {
    trap <- trap.lst[[yr]][[snm]]
    trap$Code <- sapply(trap$Location, function(loc) {
      all.locs$Code[all.locs$Location == loc & all.locs$Year == yr]})
    capture.inds <- which(!(names(trap) %in% loc.ids)) 
    capture.dates <- names(trap)[capture.inds]
    dates <- sapply(capture.dates, function(nm) {
      dt <- substring(nm, 2, nchar(nm))
      dt.char <- paste0(strsplit(dt, split = '[.]')[[1]], collapse = '-')
      return(dt.char)
    })
    names(trap)[capture.inds] <- dates
    caps <- subset(trap, select = -c(Location, Longitude, Latitude))
    m.caps <- melt(caps, id.vars = 'Code', value.name = 'Captures',
                   variable.name = 'Date')
    m.caps$Period <- snm
    return(m.caps)
  })
  period.df <- bind_rows(period.lst)
  period.df$Year <- as.numeric(yr)
  period.df$Juldate <- julian(as.Date(period.df$Date), 
                              origin = as.Date(paste0(yr, '-03-01')))
  return(period.df)
})
cap.df <- bind_rows(capture.lst)

cap.df$Captures <- sapply(cap.df$Captures, function(x) {
  ifelse(x < 0, NA, x)
})

#nao.cap.df <- as.data.frame(na.omit(cap.df))
nao.cap.df <- cap.df
na.df <- subset(cap.df, is.na(Captures))

nao.cap.df$nPeriod <- as.numeric(factor(nao.cap.df$Period, 
                                        levels = c('Afternoon', 'Evening', 
                                                   'Night', 'Morning'))) - 1
nao.cap.df$Day <- nao.cap.df$Juldate*4 + nao.cap.df$nPeriod - 1
nao.cap.df$Index <- paste0(nao.cap.df$Code, '_', nao.cap.df$Year)
nao.cap.df$corJuldate <- nao.cap.df$Juldate - min(nao.cap.df$Juldate)

nao.cap.df.orig <- nao.cap.df
newlst <- lapply(unique(nao.cap.df$Index), function(ind) {
  sub <- subset(nao.cap.df, Index == ind)
  eves <- length(which(sub$Period == 'Evening'))
  if (eves > 2) {return(sub)}
  else {return(NULL)}
})
nao.cap.df <- bind_rows(newlst)
nao.cap.df$nIndex <- as.numeric(factor(nao.cap.df$Index)) - 1

index2.dp <- subset(nao.cap.df, select = c(Index, nIndex))
index2 <- index2.dp[!duplicated(index2.dp),]

### Temp Data ###
tempdata$Period <- sapply(tempdata$Date, function(x) {
  hr <- hour(x)
  ifelse(hr < 6, 'Night',
         ifelse(hr < 12, 'Morning', 
                ifelse(hr < 18, 'Afternoon', 'Evening')))
})
p.ind.vec <- sapply(tempdata$Period, function(x) {
  ifelse(x %in% c('Night', 'Morning'), 1, 0)
})
tempdata$trapdate <- as.Date(tempdata$Date) - days(p.ind.vec)

tempdata.orig <- tempdata
tempdata.indlst <- lapply(unique(tempdata$Index), function(x) {
  sub <- subset(tempdata, Index == x)
  sub$cDD <- cumsum(sub$temp/24)
  return(sub)
})
tempdata <- bind_rows(tempdata.indlst)
tempdata.ag <- aggregate(data = tempdata, cDD ~ trapdate + Period + Year + Code, max)


td.ag.orig <- aggregate(data = tempdata, temp ~ Period + trapdate + Code, 
                        function(x) {c('min' = min(x, na.rm = TRUE), 'mean' = mean(x), 'max' = max(x))})
nms <- paste0('temp.', colnames(td.ag.orig$temp))
tmp.df <- as.data.frame(td.ag.orig$temp)
names(tmp.df) <- nms

td.ag <- bind_cols(td.ag.orig[,1:3], tmp.df)
td.ag$Year <- year(td.ag$trapdate)
td.ag$Juldate <- sapply(1:nrow(td.ag), function(r) {
  row <- td.ag[r,]
  jd <- julian.Date(row$trapdate, origin = as.Date(paste0(row$Year, '-03-01')))
})
jd.range <- range(nao.cap.df$Juldate)
td.ag <- subset(td.ag, between(Juldate, jd.range[1], jd.range[2]))
td.ag$Index <- paste0(td.ag$Code, '_', td.ag$Year)
td.ag$nIndex <- sapply(td.ag$Index, function(ind) {
  index2$nIndex[which(index2$Index == ind)[1]]
})
all.dat <- merge(nao.cap.df, td.ag)

caps.ag <- aggregate(data = all.dat, Captures ~ nIndex, function(x) {
  first <- ifelse(max(x > 0), which(x > 0)[1], NA)
  peak <- which.max(x)[1]
  return(c('first' = first, 'peak' = peak))
})
caps.df <- as.data.frame(caps.ag$Captures)
names(caps.df) <- c('first', 'peak')
caps.ag.df <- cbind('nIndex' = caps.ag[,1], caps.df)

cdate.lst <- lapply(1:nrow(caps.ag.df), function(r) {
  row <- caps.ag.df[r,]
  sub <- subset(all.dat, nIndex == row$nIndex)
  d.first <- as.Date(ifelse(is.na(row$first), NA, as.character(sub$Date[row$first])))
  d.peak <- as.Date(ifelse(is.na(row$peak), NA, as.character(sub$Date[row$peak])))
  return(c('d.first' = d.first, 'd.peak' = d.peak))
})
caps.ag.df <- bind_cols('nIndex' = caps.ag.df$nIndex, as.data.frame(bind_rows(cdate.lst)))
  
### Pheno Data ###
pheno.df$Code <- sapply(1:nrow(pheno.df), function(x) {
  loc <- pheno.df$Location[x]
  yr <- pheno.df$Year[x]
  all.locs$Code[which(all.locs$Location == loc & all.locs$Year == yr)]
})
pheno.df$Index <- paste0(pheno.df$Code, '_', pheno.df$Year)
pheno.df$nIndex <- sapply(pheno.df$Index, function(x) {
  nao.cap.df$nIndex[which(nao.cap.df$Index == x)][1]
})
ph.info <- subset(pheno.df, select = c(nIndex, f_sDate, f_pDate, f_eDate))

biosim.comp <- merge(caps.ag.df, ph.info)
biosim.comp$Year <- year(biosim.comp$f_sDate)
biosim.comp$startdiff <- as.numeric(biosim.comp$d.first - as.Date(biosim.comp$f_sDate))
biosim.comp$peakdiff <- as.numeric(biosim.comp$d.peak - as.Date(biosim.comp$f_pDate))
biosim.comp$Code <- sapply(biosim.comp$nIndex, function(x) {
  subset(all.dat, nIndex == x)$Code[1]
})
bc.full <- merge(biosim.comp, all.locs)

ggplot(data = biosim.comp) +
  geom_boxplot(aes(x = factor(Year), y = startdiff)) +
  theme_minimal()

ggplot(data = biosim.comp) +
  geom_boxplot(aes(x = factor(Year), y = peakdiff)) +
  theme_minimal()

ggplot(data = bc.full) +
  geom_point(aes(x = Longitude, y = Latitude, size = startdiff, col = peakdiff)) +
  scale_colour_gradient2(low = 'blue', mid = 'grey', high = 'red') +
  facet_wrap(vars(Year)) +
  theme_minimal()

##### Compile TMB Model #####
nList <- lme4:::namedList
basename <- "capture_model"
basename_loc <- paste0('code/', basename)
cc <- compile(paste0(basename_loc, '.cpp'))
try(dyn.unload(dynlib(basename_loc)),silent=TRUE)
dyn.load(dynlib(basename_loc))

inds <- length(unique(nao.cap.df$nIndex))
starts <- list('mu' = rep(140, inds),
               'sigma' = rep(10, inds),
               'N' = rep(60, inds))

ncd <- subset(nao.cap.df, Period == 'Evening', 
              select = c(Juldate, Captures, nIndex))

ff <- MakeADFun(data=as.list(ncd),
                parameters=starts,
                DLL=basename,
                silent=TRUE)

opt <- optim(unlist(starts), ff$fn, ff$gr, control = list('maxit' = 100000))

##### Poisson GLM #####
times <- c('Evening', 'Night', 'Morning', 'Afternoon')
modres.lst <- lapply(sort(unique(nao.cap.df$nIndex)), function(ind) {
  print(ind)
  dat0 <- subset(all.dat, nIndex == ind)
  dat0 <- as.data.frame(na.omit(dat0))
  p.tab <- table(dat0$Period)
  new.times <- names(p.tab)[p.tab > 1]
  dat0 <- subset(dat0, Period %in% new.times)
  dat0$Period <- factor(dat0$Period, levels = times[times %in% unique(dat0$Period)])
  if (length(unique(dat0$Period)) == 1 | nrow(dat0) == 0) {return(NULL)}
  mod0 <- glm(data = dat0, Captures ~ corJuldate + I(corJuldate^2) + Period + temp.min, 
              family = poisson)
  cv <- cv.glm(dat0, mod0)
  if (mod0$converged & !(length(mod0$coefficients) < mod0$rank)) {
    df <- as.data.frame(as.list(coef(mod0)))
    names(df)[1:3] <- c('Intercept', 'jdate', 'jdate2')
    df$index <- ind
    df$Year <- unique(dat0$Year)
    df$Code <- unique(dat0$Code)
    df$PeriodMorningNA <- (length(which(dat0$Period == 'Morning')) == 0)
    
    lls <- sapply(sort(unique(nao.cap.df$Juldate)), function(i) {
      sub.wo <- subset(dat0, Juldate != i)
      newsub <- subset(dat0, Juldate == i)
      if (nrow(sub.wo) > 1 & nrow(newsub) > 1) {
        mod <- glm(data = sub.wo, 
                   Captures ~ corJuldate + I(corJuldate^2) + Period + temp.min, family = poisson)
        pred <- exp(predict(mod, newsub))
        if (mod$converged & !(length(mod$coefficients) < mod$rank)) {
          return(mean(dpois(newsub$Captures, pred, log = TRUE)))}
        else {return(NA)}
      }
     else {return(NA)}
    })
    worst <- sort(unique(nao.cap.df$Juldate))[which.min(lls)]
    
    pred <- exp(predict(mod0, subset(dat0, select = c(corJuldate, Period, temp.min))))
    caps <- dat0$Captures
    m.ll <- min(dpois(caps, pred, log = TRUE))
    
    df$minmodLL <- m.ll
    df$cv.delta <- cv$delta[2]
    df$minlooLL <- min(lls, na.rm = TRUE)
    df$worstJD <- worst
    df.lls <- as.data.frame(as.list(lls))
    names(df.lls) <- paste0('jd', seq(min(nao.cap.df$Juldate), 
                                      max(nao.cap.df$Juldate), by = 1))
    dfnew <- cbind(df, df.lls)
    
    return(dfnew)
  }
  else {return(NULL)}
})
mod.df <- bind_rows(modres.lst)

full.df <- subset(mod.df, !(is.na(PeriodNight) | is.na(PeriodMorning) | 
                              is.na(PeriodAfternoon) | PeriodMorningNA))
# write.csv(full.df, 'code/glm_results.csv', row.names = FALSE)

jd.grep <- grep('jd1', names(full.df))
fd.jd <- cbind('nIndex' = full.df[,'index'], full.df[,jd.grep])

fd.jd.ni <- subset(fd.jd, select = -nIndex)
ap <- lapply(1:nrow(fd.jd.ni), function(x) {
  all.vals <- fd.jd.ni[x,]
  w.vals <- which(all.vals < -15)
  jds <- as.numeric(substr(names(fd.jd.ni)[w.vals], 3, 5))
  vals <- unlist(all.vals[w.vals])
  ind <- fd.jd$nIndex[x]
  if (length(jds) > 0) {
    df <- data.frame('Juldate' = jds, 'll' = vals)
    df$nIndex <- ind
  }
  else {df <- NULL}
  return(df)
})
ap.df <- bind_rows(ap)
bad.pts.full <- left_join(ap.df, all.dat)
bad.pts0 <- subset(bad.pts.full, select = c(Code, Period, Year, Date, Captures, ll))
bad.pts <- merge(bad.pts0, all.locs)

fd.info <- full.df[,-jd.grep]
mu <- -fd.info$jdate/(2*fd.info$jdate2)

fd.info$mu <- mu + min(nao.cap.df$Juldate)
fd.info$sigma <- sqrt(-1/(2*fd.info$jdate2))

fd.ord <- order(fd.info$minlooLL)

mod.lst <- lapply(sort(unique(fd.info$index)), function(x) {
  print(x)
  sub <- subset(all.dat, nIndex == x)
  if (dim(sub)[1] == 0) {return(NULL)}
  mod <- glm(data = sub, family = poisson,
             Captures ~ corJuldate + I(corJuldate^2) + Period + temp.min)
})
names(mod.lst) <- sort(unique(fd.info$index))

##### Detect early season outliers #####
gauscore <- lapply(1:nrow(all.dat), function(r) {
  row <- all.dat[r,]
  ind <- row$nIndex
  w <- which(fd.info$index == ind)
  if (length(w) == 0) {return(data.frame('density' = NA, 'score' = NA))}
  else {
    mod <- mod.lst[[as.character(ind)]]
    dens <- exp(predict(mod, row))
    #dens <- dnorm(row$Juldate, fd.info$mu[w], fd.info$sigma[w])
    caps <- row$Captures
    #score <- (sqrt(caps)/dens)*ifelse(row$Juldate < fd.info$mu[w], 1, 0)
    score <- (pmin(caps, 1)/dens)*ifelse(row$Juldate < fd.info$mu[w], 1, 0)
    return(data.frame('density' = dens, 'score' = score))
  }
})
gs <- bind_rows(gauscore)
ncd <- na.omit(cbind(all.dat, gs))

ncd <- ncd[order(ncd$score, decreasing = TRUE),]
ncd.pos <- subset(ncd, score > 1)

traps <- subset(ncd.pos, select = c(Code, Year, nIndex))
traps.nd <- traps[!duplicated(traps),]

##### Relative abundace #####
pord <- c('Morning', 'Afternoon', 'Evening', 'Night')
abundance.lst <- lapply(mod.lst, function(mod) {
  data <- mod$data
  yr <- unique(data$Year)
  jdates <- 0:70
  full <- expand.grid(jdates, pord)
  names(full) <- c('corJuldate', 'Period')
  full$trapdate <- as.Date(paste0(yr, '-06-15')) + full$corJuldate
  full$Year <- yr
  full$Code <- unique(data$Code)
  full$temp.min <- left_join(full, td.ag)$temp.min
  full.nao <- na.omit(full)
  pred <- exp(predict(mod, full.nao))
  code <- unique(data$Code)
  index <- paste0(code, '_', yr)
  df <- data.frame('Code' = code, 'Year' = yr, 
                   'Index' = index,
                   'nIndex' = unique(subset(cap.df, Index == index)$nIndex),
                   'Abundance' = sum(pred))
  return(df)
})
rab.df <- bind_rows(abundance.lst)


##### Plotting #####
par(mfrow = c(3, 3))
trap.plot <- function(ind) {
  dat0 <- subset(all.dat, nIndex == ind)
  dat0$Period <- factor(dat0$Period, levels = pord)
  mod0 <- glm(data = dat0, Captures ~ corJuldate + I(corJuldate^2) + Period + 
                temp.min, family = poisson)
  tdat <- subset(td.ag, nIndex == ind)
  for (i in 1:4) {
    per <- pord[i]
    sub <- subset(dat0, Period == per)
    cl <- c('black', 'blue', 'red', 'green')[i]
    pred <- exp(predict(mod0, subset(sub, select = c(corJuldate, Period, temp.max, temp.min))))
    if (i == 1) {
      plot(sub$Juldate, sqrt(pred), type = 'l', col = cl, 
           xlim = c(106, 176),
           #ylim = c(0, sqrt(max(dat0$Captures) + 2)),
           ylim = c(0, 15),
           main = unique(dat0$Index))
      abline(v = subset(fd.info, index == ind)$worstJD, lty = 2, lwd = 0.25)
    }
    else {
      lines(sub$Juldate, sqrt(pred), col = cl)
    }
    points(sub$Juldate, sqrt(sub$Captures), col = cl, pch = 20)
  }
}
for (ind in inds) {
  trap.plot(ind)
}

