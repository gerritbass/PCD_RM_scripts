# Gerrit Bass Summer 2022 
# Script to format Paradise Creek Water Temp HOBO logger data to go into WISKI


library(tidyverse)
library(plyr)

#set directory where files are located
setwd("R:/_04_Project_Data/Water_Quality/Paradise_Creek_Temp_Study/water_temp_data")
# make a list of all your files
csv_files <- list.files(pattern = "*.csv")
xls_files <- list.files(pattern = "*.xls")


# PARADISE WATER TEMP___________________________________________________________________________

#CSV FILES
#for loop to iterate through files in list and edit them and save them in a new folder
for (file in csv_files){
  data <- read.csv(file) # read in data
  data <- subset(data, select = c(1:3)) #only keep first three columns
  colnames(data) <- c("site_name","datetime","WT") #rename columns
  data$site_name <- word(file,1) #fill the site name column with the fist word of the file name (which is the site name)
  
  new_file = paste("to_WISKI/",file, ".csv", sep="") # create the new file name
  write.csv(data, new_file, row.names = F, na = "") # write the new file
}
   

#XLS FILES  
for (file in xls_files){
  data <- read_exel(file) # read in data
  data <- subset(data, select = c(1:3)) #only keep first three columns
  colnames(data) <- c("site_name","datetime","WT") #rename columns
  data$site_name <- word(file,1) #fill the site name column with the fist word of the file name (which is the site name)
  
  new_file = paste("to_WISKI/",file, ".csv", sep="") # create the new file name
  write.csv(data, new_file, row.names = F, na = "") # write the new file
}
  

#PARADISE AIR TEMP_______________________________________________________________________

#set directory where files are located
setwd("R:/_04_Project_Data/Water_Quality/Paradise_Creek_Temp_Study/air_temp_data")
# make a list of all your files
files <- list.files(pattern = "*.csv")


#for loop to iterate through files in list and edit them and save them in a new folder
for (file in files){
  data <- read.csv(file, skip = 1) # read in data
  colnames(data) <- c("site_name","datetime","AirTempC") #rename columns
  data$site_name <- sapply(strsplit(file,"_"), `[`, 1) #fill the site name column with the fist word of the file name (which is the site name)
  
  new_file = paste("to_WISKI/",file, ".csv", sep="") # create the new file name
  write_csv(data, new_file, na = "") # write the new file
}








