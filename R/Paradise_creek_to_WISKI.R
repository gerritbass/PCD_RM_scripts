library(tidyverse)
library(plyr)

#set directory where files are located
setwd("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Paradise_Creek_Temp_Study/water_temp_data")
# make a list of all your files
files <- list.files(pattern = "*.csv")


#for loop to iterate through files in list and edit them and save them in a new folder
for (file in files){
  data <- read.csv(file) # read in data
  data <- subset(data, select = c(1:3)) #only keep first three columns
  colnames(data) <- c("site_name","datetime","WT") #rename columns
  data$site_name <- word(file,1) #fill the site name column with the fist word of the file name (which is the site name)
  
  new_file = paste("to_WISKI/",file, ".csv", sep="") # create the new file name
  write.csv(data, new_file, row.names = F, na = "") # write the new file
}
   
  
 
  









