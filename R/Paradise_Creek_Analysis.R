# Gerrit Bass Winter 2022/23
# Homebase for analyzing cleaned Paradise Creek data that has been been exported from WISKI 
# and complied by the "Paradise_from_WISKI" script.
# Also have 7 day moving average temp analysis for now from Paradise_movingavg_fromWISKI" script

library(plyr)
library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)


# DATA UPLOAD ----------------------------------------------------------------- 

#set directory where files are located
setwd("R:/_04_Project_Data/from_WISKI/Paradise_Creek")

# read in Paradise Creek Water Temp (degC) data
paradise_data <- read_csv("paradiseWT_full.csv") %>% 
  mutate(station = as_factor(station))

paradise_data <- drop_na(paradise_data)

# BASIC STATS ----------------------------------------------------------------

station_max <- paradise_data %>% 
  group_by(station) %>% 
  dplyr::summarize(station_maxi = max(WT),
                   station_min = min(WT),
                   station_mean = mean(WT)) 

station_max

ggplot(data = station_max, aes(x = station, y = station_max, group = station))+
  geom_point( aes(color = station))

#BASIC GRAPHS ---------------------------------------------------------------------

#basic time series graph of all stations
ggplot(data = paradise_data,aes(x = datetime, y = WT, group = station )) +
  geom_line(aes(color = station))+
  geom_hline(yintercept = 17.5, color = "red")

#basic boxplot
ggplot(data = paradise_data,aes(x = station, y = WT )) +
  geom_boxplot(aes(color = station))
 

#-----------------------------------------------------------------------------------

## ANALYSIS OF 7-DAY MOVING AVERAGE DATA

# read in Paradise Creek Water Temp (degC) data
paradise_avg <- read_csv("./moving_avg/paradise_7dayavg_WT_full.csv")

paradise_avg <- drop_na(paradise_avg)

#avg time series graph of all stations
ggplot(data = paradise_avg,aes(x = datetime, y = WT, group = station )) +
  geom_line(aes(color = station))+
  geom_hline(yintercept = 17.5, color = "red")

#basic boxplot
ggplot(data = paradise_data,aes(x = station, y = WT )) +
  geom_boxplot(aes(color = station))


#------------------------------------------------------------------
# 7-DAD Max

# daily max
daily_max_temp <- paradise_data %>%
  mutate(date = date(datetime)) %>% #create at date column from datetime
  group_by(date, station) %>% 
  dplyr::summarise(daily_max = max(WT))

# daily max graph
ggplot(data = daily_max_temp,aes(x = date, y = daily_max, group = station )) +
  geom_line(aes(color = station))+
  geom_hline(yintercept = 17.5, color = "red")

# rolling 7 day average of daily max
seven_day_moving_max <- daily_max_temp %>%
  group_by(station) %>%
  mutate(seven_DADMax = rollmean(daily_max,7,fill = NA))

# 7DADMax graph
ggplot(data = seven_day_moving_max,aes(x = date, y = seven_DADMax, group = station )) +
  geom_line(aes(color = station))+
  geom_hline(yintercept = 17.5, color = "red")

# calculates the number of days the 17.5 deg threshold is exeeded
exceed_sum <- seven_day_moving_max %>% 
  drop_na() %>% 
  group_by(station) %>% 
  summarise(exeed_days = sum(seven_DADMax > 17.5), days = sum(seven_DADMax > 0), ratio = exeed_days/days )
exceed_sum

# graph of number of eceedance days ratio for each site
ggplot(data = exceed_sum,aes(x = station, y = ratio )) +
  geom_bar(stat= "identity", aes(fill = station))+
  coord_cartesian(ylim = c(0.5, 0.7))

# Monthly Stats (min, max, and range) ------------------------------------------------------------------------------------------

june_stats <- paradise_data %>%
  filter(month(datetime) %in% 6) %>% 
  group_by(station) %>% 
  dplyr::summarize(station_maxi = max(WT),
                   station_min = min(WT),
                   station_range = station_maxi - station_min ) %>% 
  mutate(month = 'JUNE')

july_stats <- paradise_data %>%
  filter(month(datetime) %in% 7) %>% 
  group_by(station) %>% 
  dplyr::summarize(station_maxi = max(WT),
                   station_min = min(WT),
                   station_range = station_maxi - station_min ) %>% 
  mutate(month = 'JULY')

aug_stats <- paradise_data %>%
  filter(month(datetime) %in% 8) %>% 
  group_by(station) %>% 
  dplyr::summarize(station_maxi = max(WT),
                   station_min = min(WT),
                   station_range = station_maxi - station_min ) %>% 
  mutate(month = 'AUG')

sept_stats <- paradise_data %>%
  filter(month(datetime) %in% 9) %>% 
  group_by(station) %>% 
  dplyr::summarize(station_maxi = max(WT),
                   station_min = min(WT),
                   station_range = station_maxi - station_min ) %>% 
  mutate(month = 'SEPT')

oct_stats <- paradise_data %>%
  filter(month(datetime) %in% 10) %>% 
  group_by(station) %>% 
  dplyr::summarize(station_maxi = max(WT),
                   station_min = min(WT),
                   station_range = station_maxi - station_min ) %>% 
  mutate(month = 'OCT')

  
