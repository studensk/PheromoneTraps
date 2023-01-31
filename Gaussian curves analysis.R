################################################################################
##
## This code fits the flight activity curves with Gaussian curves following
## Edwards and Crone (2021)
##
################################################################################
library(lme4)
library(MASS)
library(here)
library(tidyverse)
library(purrr)

source("Functions.R")

# Download the full dataset
AllTraps <- GetAllTrapData()

# Calculate sum of counts per day
AllTraps.perDay <- SumCountsPerDay(AllTraps)

# Date to DOY function
Date2DOY <- function(x) {
  as.integer(x - as.Date(paste0(year(x),"-01-01")))
}

# Prepare the datafile
# 1- Removes Location*Year if the number of days with (counts>0) is < 3
# 2- Calculate DOY
# 3- Adds an identifier for overdispersed Poisson
AllTraps.remove <- AllTraps.perDay %>%
  mutate(Year=year(FlightDay)) %>%
  group_by(Location,Year) %>%
  filter(!is.na(DayCount)) %>%
  mutate(Count.gt.1 = if_else(DayCount>0,1,0)) %>%
  summarise(nDays=sum(Count.gt.1)) %>%
  ungroup() %>%
  filter(nDays<11)

AllTraps.dat <- anti_join(AllTraps.perDay,AllTraps.remove) %>%
  mutate(DOY=Date2DOY(FlightDay)) %>%
  rownames_to_column(var="ID") %>%
  mutate(Year=year(FlightDay),
         LocxYear = paste(Location,Year,sep="."),
         DOYsc = scale(DOY,scale=T)[,1])

m0 <- glmer(DayCount ~ DOYsc + I(DOYsc*DOYsc) + (DOYsc + I(DOYsc*DOYsc)|LocxYear), family = poisson, data=AllTraps.dat )
summary(m0)

m1 <- glmer(DayCount ~ DOYsc + I(DOYsc*DOYsc) + (DOYsc + I(DOYsc*DOYsc)|LocxYear) + (1|ID), family = poisson, data = AllTraps.dat)
ss <- getME(m1,c("theta","fixef"))
m2 <- update(m1,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
m3 <- update(m1,start=ss,control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5)))
# Rescale the parameters
rescale_coeff <- function(mx) {
  mx.coef <- coefficients(mx)$LocxYear
  mx.coef$LocxYear <- rownames(mx.coef)
  mx.coef.rescaled <- do.call("rbind",lapply(1:dim(mx.coef)[1], function(x){
    m <- mean(AllTraps.dat[AllTraps.dat$LocxYear == mx.coef[x,4],"DOY"]$DOY)
    s <- sd(AllTraps.dat[AllTraps.dat$LocxYear == mx.coef[x,4],"DOY"]$DOY)
    B0 <- mx.coef[x,1] - mx.coef[x,2] * m / s + mx.coef[x,3] * m^2 / s^2
    B1 <- mx.coef[x,2] / s - 2 * mx.coef[x,3] * m / s^2
    B2 <- mx.coef[x,3] / s^2
    data.frame(LocxYear=mx.coef[x,4],B0,B1,B2)
}))
  mx.coef.rescaled
}

m0.coef.rescaled <- rescale_coeff(m0)
m1.coef.rescaled <- rescale_coeff(m1)
mx.coef.rescaled <- full_join(m0.coef.rescaled,m1.coef.rescaled,by=c("LocxYear"),suffix=c(".m0",".m1"))

# Prediction function
pred.f <- function(DOY,B0,B1,B2) {
  exp(B0 + B1*DOY + B2*DOY^2)
}

# Calculate predictions for each LocxYear from min to max observed DOY
mx.pred <- mx.coef.rescaled %>%
  group_by(LocxYear) %>%
  summarise(DOY = seq(min(AllTraps.dat[AllTraps.dat$LocxYear == LocxYear,"DOY"]),
                      max(AllTraps.dat[AllTraps.dat$LocxYear == LocxYear,"DOY"]),0.1), 
            Count.pred.m0 = pred.f(DOY,B0.m0,B1.m0,B2.m0),
            Count.pred.m1 = pred.f(DOY,B0.m1,B1.m1,B2.m1))


# Join observations and predictions
# There will be 10x more predictions than observations because the predictins will be plotted as curves
res <- left_join(mx.pred,AllTraps.dat)

# Plot for each year
Selected.Year <- 2022
ggplot(filter(res,Year==Selected.Year),aes(x=DOY)) +
  geom_point(aes(y=DayCount)) +
  geom_line(aes(y=Count.pred.m0),color="red") +
  geom_line(aes(y=Count.pred.m1),color="blue") +
  facet_wrap(~LocxYear)

# Calculate summary statistics
Summaries <- m0.coef.rescaled %>%
  mutate(PeakDay=-1*B1/(2*B2),
         Duration80=2*1.285*sqrt(-1*1/(2*B2)),
         Abundance=sqrt(-1*(2*pi)/(2*B2))*exp(B0-B1^2/(4*B2))) %>%
  separate(LocxYear,into = c("Location","Year"),sep = "[.]")

ggplot(Summaries,aes(x=Duration80)) +
  geom_histogram()

ggplot(Summaries,aes(x=Abundance)) +
  geom_histogram()

ggplot(Summaries,aes(x=PeakDay)) +
  geom_histogram() 


