# Gerrit Bass Winter 2022/23
# Homebase for analyzing cleaned Paradise Creek data that has been been exported from WISKI 
# and complied by the "Paradise_from_WISKI" script.
# Also have 7 day moving average temp analysis for now from Paradise_movingavg_fromWISKI" script

library(tidyverse)
library(plyr)
library(lubridate)
library(zoo)

## ANALYSIS OF FULL DATASET

#set directory where files are located
setwd("R:/_04_Project_Data/from_WISKI/Paradise_Creek")

# read in Paradise Creek Water Temp (degC) data
paradise_data <- read_csv("paradiseWT_full.csv")

paradise_data <- drop_na(paradise_data)

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
# trying to get a 7 day avearge daily max

# daily max works
daily_max_temp <- paradise_data %>%
  mutate(date = date(datetime)) %>% #create at date column from datetime
  group_by(date, station) %>% 
  dplyr::summarise(daily_max = max(WT))

# rolling 7 day average of daily max is a little tougher... doesn't aveage for each site
seven_day_moving_max <- daily_max_temp %>%
  group_by(station) %>%
  mutate(seven_DADMax = rollmean(daily_max, k = 7,fill = NA, align = "right"))


  
