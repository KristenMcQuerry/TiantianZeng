## install dplyr and hflights packages
install.packages("dplyr")
install.packages("hflights")
install.packages("nycflights13")

# Load Library
library(readxl)

## Load the dplyr package
library(dplyr)

## Load the hflights package
library(nycflights13)

## make hfights a tibble
flights <- as.tbl(flights)


################### (3) Cancelled flights #################
flights %>%
  group_by(day) %>%
  filter(is.na(dep_delay)) %>%
  summarise(n_plane=n())

flights %>%
  group_by(day) %>%
  filter(!is.na(dep_delay)) %>%
  summarise(ave=mean(dep_delay))

# By comparing the two filtered tibble, we could see that the cancelled flights is somewhat related to the average delay.


################### (4) Worst delays #################
# I use departure delay
flights %>%
      group_by(carrier) %>%
      filter(dep_delay > 0) %>%
      summarise(n_plane = n()) %>%
      arrange(desc(n_plane)) # Show in descending order
# UA has the worst delay

# Disentangle effect of airport and carrier
flights %>%
      group_by(carrier, dest) %>%
      filter(dep_delay > 0) %>%
      summarise(n_plane = n()) %>%
      arrange(desc(n_plane))
# It is hard to disentangle the effects of bad airports versus bad carriers using this method. 
# We might need to do Anova test to disentangle the effect.


################### (5) Count the number of first delay #################



################### (6) Worst on-time record #################
flights %>%
  group_by(tailnum) %>%
  filter(dep_delay > 0) %>%
  summarise(n_plane = n()) %>%
  arrange(desc(n_plane))
# Filtered by departure delay, we could see that N258JB has the worst on-time record.


################### (7) Time of day #################
flights %>%
  mutate(Date = paste(year, month, day, sep = '-')) %>%
  group_by(Date) %>%
  filter(dep_delay > 0) %>%
  summarise(nplanes = n()) %>%
  arrange(nplanes)


################### (8) Total minutes of delay #################
flights %>%
  group_by(dest) %>%
  filter(arr_delay > 0) %>%
  summarise(TotalDelayTime = sum(arr_delay, na.rm = TRUE))

# Comparing arrival time versus scheduled arrival time, we could have arr_delay. 
# I think it is the delay time that we want to calculate.
# Thus, instead of using dep_delay, I use arr_delay for this question.


## proportion of the total delay for its destination
flights %>%
  group_by(dest, flight) %>%
  filter(arr_delay > 0) %>%
  summarise(TotalDelay = n()) %>%   # Count
  mutate(prop = as.vector(TotalDelay) / as.vector(by(TotalDelay, dest, sum)))


################### (9) Suspiciously fast #################
flights %>%
  select(flight, arr_time, sched_arr_time) %>%
  filter(arr_time < sched_arr_time) %>%
  mutate(timeahead = abs(arr_time-sched_arr_time)) %>%
  arrange(desc(timeahead))

# Air time
flights %>%
  mutate(realtime = hour * 60 + minute, timediff=(realtime - air_time))%>%
  arrange(desc(timediff))

# Most delayed 
flights %>%
  mutate(realtime = hour * 60 + minute) %>%
  arrange(realtime) %>%
  group_by(dest) %>%
  mutate(diff = (realtime - by(realtime, dest, min))) %>%
  arrange(desc(diff))
