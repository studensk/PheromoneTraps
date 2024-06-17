library(BioSIM)
library(xlsx)
library(tidyverse)

sheet_names <- c('Morning', 'Night', 'Afternoon', 'Evening')

trap21.lst <- lapply(sheet_names, function(nm) {
  dat <- read.xlsx('data/traps2021.xlsx', sheetName = nm)
  dat <- dat[1:(nrow(dat)-2),]
  return(dat)
})

trap22.lst <- lapply(sheet_names, function(nm) {
  dat <- read.xlsx('data/traps2022.xlsx', sheetName = nm)
  dat <- dat[1:(nrow(dat)-2),]
  return(dat)
})

trap21 <- sapply(trap21.lst, function(x) {
  dat <- x[,c('Location', 'Total')]
  totals <- sum(dat$Total)
  return(totals)
})

trap22 <- sapply(trap22.lst, function(x) {
  dat <- x[,c('Location', 'Total')]
  totals <- sum(dat$Total)
  return(totals)
})

trap.lst <- lapply(2018:2022, function(year) {
  dat <- read.xlsx(paste0('data/traps', year, '.xlsx'), sheetName = 'Daily Captures')
  dat.raw <- dat[1:(nrow(dat)-2),1:(ncol(dat)-2)]
  return(dat.raw)
})

df <- data.frame('Year' = c(rep(2021, length(sheet_names)), rep(2022, length(sheet_names))),
                 'Time' = rep(sheet_names, 2),
                 'Count' = c(trap21, trap22))
df$Time <- factor(df$Time, levels = c('Morning', 'Afternoon', 'Evening', 'Night'))

ggplot(data = df) +
  geom_bar(aes(x = Time, y = Count), stat = 'identity') +
  labs(x = 'Capture Period') +
  facet_wrap(vars(Year)) +
  theme_minimal()
