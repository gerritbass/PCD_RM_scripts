#Gerrit Bass Fall 2022
# Export of SCC And Turb data from WISKI files


library(tidyverse)
library(plyr)
library(lubridate)



#set directory where files are located
setwd("R:/_04_Project_Data/from_WISKI/Turb_and_SSC")
# make a list of all your files
files <- list.files() 
files <- files%>% 
  str_detect(pattern = ".log",negate = TRUE) %>% 
  keep(files, .)

#for loop to iterate through files in list and edit them and save them in a new folder
for (file in files){
  # read in data
  data <- read_csv(file, skip = 15)
  
  station<- sapply(strsplit(file,"_"), `[`, 1) #grab station name from file name
  
  # create a "Parameter" column based on the units used in the file
  val_column <- colnames(data[3]) # grab parameter name
  if(val_column == "Value[mg/l]"){ 
    Parameter <- "SSC"
    Unit <- "mg/L"
  } else if ( val_column == "Value[NTU]"){
    Parameter <- "TURB"
    Unit <- "NTU"
  } else{ Parameter <- "ERROR"
  Unit <- "ERROR"
  } #if statements to name parameter based on units
  
  data$parameter <- Parameter 
  data$unit <- Unit
  data$datetime <- mdy_hms(paste(data$Date, data$Time))
  data$station <- station
  data <- subset(data,select = c(7,6,3:5))
  names(data)[3] <- "Value"
  
  new_file = paste("P:/Research_and_Monitoring/_04_Project_Data/From_WISKI/Joined_File/",file, ".csv", sep="") # create the new file name
  write.csv(data, new_file, row.names = F, na = "")
}


data_join <- list.files(path = "P:/Research_and_Monitoring/_04_Project_Data/from_WISKI/Joined_File", 
                        pattern = "*.csv", full.names = TRUE) %>% # Identify all CSV files
  lapply(read_csv) %>%                              # Store all files in list
  reduce(full_join)                      # Full-join data sets into one data set 

write.csv(data_join, "P:/Research_and_Monitoring/_04_Project_Data/from_WISKI/Joined_File/Joined_File.csv")







