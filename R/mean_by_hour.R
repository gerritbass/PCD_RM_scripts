library(tidyverse)
library(readxl)
library(lubridate)
temp <- read_excel('C:/Users/gerritb/Documents/paradise_temp_3B.xlsx')
temp$date = as.Date(temp$datetime) 
temp$hour = format(as.POSIXct(temp$datetime), format = '%H')


temp_hour <- temp %>% 
  group_by(date, hour) %>% 
  summarize(mean = mean(value, na.rm=TRUE))

temp_hour$datetime = as.POSIXct(paste(temp_hour$date, temp_hour$hour), format ="%Y-%m-%d %H")

write.csv(temp_hour,"C:/Users/gerritb/Documents/hour_temp3B.csv", row.names = FALSE)
