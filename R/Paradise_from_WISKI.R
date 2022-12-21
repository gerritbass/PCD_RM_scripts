# GERRIT BASS WINTER 2022/23
# SCRIPT TO FORMAT AND COMBINE PARADISE CREEK DATA FILES EXPORTED FROM WISKI


library(tidyverse)
library(plyr)
library(lubridate)

#set directory where files are located
setwd("R:/_04_Project_Data/from_WISKI/Paradise_Creek")

# make a list of all your files
data_files <- grep(list.files(), pattern='.log', invert=TRUE, value=TRUE)


# clean data and create a file to append the rEst of the files too

# set the first file equal to a variable "file1"
file1 <- data_files[1]

#extract station name from file name
station<- sapply(strsplit(file1,"_"), `[`, 1) 

# read in data
paradise_file1 <- read.csv(file1, skip = 15)

# clean data
paradise1_cleaned <- paradise_file1 %>% 
  mutate(datetime = mdy_hms(paste(Date, Time))) %>%
  mutate(station = station) %>% 
  dplyr::rename(water_tempC = 3) %>% # needed to specify the rename command from dplyr to work right
  select(station,datetime,water_tempC)

write_csv(paradise1_cleaned, file = "appended_paradise.csv", append = FALSE)

# remove the first file from the list of files before appending the rest of the files
data_files_new <- data_files[-1]

#function to clean and append Paradise Creek data to one .CSV file

Paradise_cleanANDcompile <- function(file){
  
  paradise_file <- read.csv(file, skip = 15)
  
  station<- sapply(strsplit(file,"_"), `[`, 1) #grab station name from file name
  
  paradise_cleaned <- paradise_file %>% 
    mutate(datetime = mdy_hms(paste(Date, Time))) %>%
    mutate(station = station) %>% 
    dplyr::rename(water_tempC = 3) %>% 
    select(station,datetime,water_tempC)
  
  write_csv(paradise_cleaned, file = "appended_paradise.csv", append = TRUE)
} 

# clean and append the rest of the data to appended_paradise
lapply(data_files_new, Paradise_cleanANDcompile)

#__________________________________________________________________________________

## Now that we have a file with all the data in it we can change it from long to wide format to make it easier to work with and graph
## EDIT: turns out long is actually better so wider commands have been commented out and name changed

#read in our new file and identify NA values
all_paradise <- read_csv("appended_paradise.csv", na = "---")

#remove duplicates and pivot wider
long_paradise <- all_paradise %>%
  distinct()# %>% # get rid of duplicates
  #pivot_wider(names_from = station, values_from = 'water_tempC')

#create a new file to work with
write_csv(wide_paradise, file = "paradiseWT_full.csv")








  

  
 

