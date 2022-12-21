# Gerrit Bass Winter 2022/23
# SCRIPT TO FORMAT AND COMBINE PARADISE CREEK 7 day avg DATA FILES EXPORTED FROM WISKI
# Turns out this isn't the kind of average we wanted though, but will keep script for now

library(tidyverse)
library(plyr)
library(lubridate)

#set directory where files are located
setwd("R:/_04_Project_Data/from_WISKI/Paradise_Creek/moving_avg")

# make a list of all your files
data_files <- grep(list.files(), pattern='.log', invert=TRUE, value=TRUE)


# clean data and create a file to append the rst of the files too

# set the first equal to a variable "file1
file1 <- data_files[1]

#extract station name from file name
station<- sapply(strsplit(file1,"_"), `[`, 1) 

paradise_file1 <- read.csv(file1, skip = 15)

paradise1_cleaned <- paradise_file1 %>% 
  mutate(datetime = mdy_hms(paste(Date, Time))) %>%
  mutate(station = station) %>% 
  dplyr::rename(WT = 3) %>% 
  select(station,datetime,WT)

write_csv(paradise1_cleaned, file = "appended_paradise_7dayavg.csv", append = FALSE)

# remove the first file from the list of files
data_files_new <- data_files[-1]

#function to clean and append Paradise Creek data to one .CSV file

Paradise_cleanANDcompile <- function(file){
  
  paradise_file <- read.csv(file, skip = 15)
  
  station<- sapply(strsplit(file,"_"), `[`, 1) #grab station name from file name
  
  paradise_cleaned <- paradise_file %>% 
    mutate(datetime = mdy_hms(paste(Date, Time))) %>%
    mutate(station = station) %>% 
    dplyr::rename(WT = 3) %>% 
    select(station,datetime,WT)
  
  write_csv(paradise_cleaned, file = "appended_paradise_7dayavg.csv", append = TRUE)
} 

# clean and append the rest of the data to appended_paradise
lapply(data_files_new, Paradise_cleanANDcompile)

#__________________________________________________________________________________

## Now that we have a file with_te all the data in it we can change it from long to wide format to make it easier to work with and graph

#read in our new file and identify NA values
all_paradise <- read_csv("appended_paradise_7dayavg.csv", na = "---")

#remove duplicates and pivot wider
long_paradise <- all_paradise %>%
  distinct() #%>% # get rid of duplicates
  #pivot_wider(names_from = station, values_from = 'water_tempC')

#create a new file to work with
write_csv(long_paradise, file = "paradise_7dayavg_WT_full.csv")








  

  
 

