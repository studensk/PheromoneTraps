################################################################################
##
## This code reads the capture data from the Trapview site 
## does a few checks, saves the capture data in an Excel workbook
## and uploads it to a Google sheet workbook
##
################################################################################
library(lubridate)
library(googledrive)
library(googlesheets4)
library(openxlsx)
library(sf)
library(tidyverse)

source("Functions.R")

# Download the full dataset
AllTraps <- GetAllTrapData()

# Calculate the total moth count per year for the traps that were setup every year
# Compare with defoliation
PermanentTraps <- AllTraps %>% group_by(Location) %>% 
  summarise(yearcount = length(unique(year(FlightDay)))) %>%
  filter(yearcount==5) %>%
  select(Location)
AllTraps %>% filter(Location %in% PermanentTraps$Location) %>%
  group_by(Year=year(FlightDay)) %>%
  summarise(TotalMoths = sum(MothCount,na.rm=TRUE)) %>%
  mutate(Defoliation=c(8180770,9608488,13537152,12229847,9159154)) %>%
  ggplot(aes(x=Year)) +
  geom_line(aes(y=TotalMoths),color="blue") +
  geom_line(aes(y=Defoliation/1000),color="red") +
  scale_y_continuous(
    name = "Total moths count",
    sec.axis = sec_axis(~.*1000,name="Defoliation (ha)")) +
  theme(
        axis.title.y = element_text(color = "blue"),
        axis.title.y.right = element_text(color = "red")
      )

# Distribution of flight season length
AllTraps %>% group_by(Location,year(FlightDay)) %>%
  filter(!is.na(MothCount) & MothCount > 0) %>%
  summarise(StartDate=min(FlightDay),EndDate=max(FlightDay),Duration=(EndDate-StartDate)) %>%
  ggplot(aes(x=Duration)) + 
  geom_histogram(binwidth=4)


