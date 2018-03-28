## install dplyr and hflights packages
install.packages("dplyr")
install.packages("hflights")
install.packages("tidyverse")
install.packages("nycflights13")

# Load Library
library(readxl)

## Load the dplyr package
library(dplyr)
library(tidyverse)

## Load the hflights package
library(nycflights13)

## make hfights a tibble
flights <- as.tbl(flights)


################### (3) Cancelled flights #################

flights %>%
  arrange(arr_delay)

################### (4) Worst delays #################
# I use departure delay
(flights %>%
      group_by(carrier) %>%
      filter(dep_delay >= 0) %>%
      summarise(n_planes = n()) %>%
      arrange(n_planes))
# UA has the worst delay

# Disentangle effect of airport and carrier
(flights %>%
      group_by(carrier, dest) %>%
      filter(dep_delay >= 0) %>%
      summarise(n_planes = n()) %>%
      arrange(desc(n_planes)))

################### (5) Count the number of flight delay #################



################### (6) Worst on-time record #################
(flights %>%
  group_by(tailnum) %>%
  filter(dep_delay > 0) %>%
  summarise(nplanes = n()) %>%
  arrange(desc(nplanes)))


################### (7) Time of day #################
flights %>%
  mutate(Date = paste(year, month, day, sep = '-')) %>%
  group_by(Date) %>%
  filter(dep_delay >= 0) %>%
  summarise(nplanes = n()) %>%
  arrange(nplanes)


################### (8) Total minutes of delay #################
flights %>%
  filter(arr_delay >= 0) %>%
  group_by(dest) %>%
  summarize(totalarrDelay = sum(arr_delay, na.rm = TRUE))

## proportion of the total delay for its destination
flights %>%
  filter(arr_delay >= 0) %>%
  group_by(dest, flight) %>%
  summarize(arrD_ef = sum(arr_delay, na.rm = TRUE)) %>%
  mutate(prop_f = as.vector(arrD_ef) / as.vector(by(arrD_ef, dest, sum)))


################### (9) Suspiciously fast #################
flights %>%
  mutate(realtime = hour * 60 + minute, timediff=(realtime - air_time))%>%
  arrange(desc(timediff))

### most delayed
flights %>%
  mutate(realtime = hour * 60 + minute) %>%
  arrange(realtime) %>%
  group_by(dest) %>%
  mutate(diff = (realtime - by(realtime, dest, min))) %>%
  arrange(desc(diff))
