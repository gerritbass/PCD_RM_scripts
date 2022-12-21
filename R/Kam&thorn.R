library(dplyr)
library(xlsx)
library(readxl)
library(lubridate)
library(stringr)
library(data.table)
library(taskscheduleR)
library(tidyr)
library(readr)


setwd("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/_3_Incoming_Data/Historic_Kam&Thorn")

files <- list.files(pattern = "*.NEW")

for (file in files){
  station <- sapply(strsplit(file,"_"), `[`, 1)
  data <- read.table(file, row.names = NULL, header = F) # read in data
  colnames(data) = c("DATE", "TIME", "S", "WT", "AirTemp", "TURB", "Variance", "MedTurb", "MinTurb", "MaxTurb", " FTSTemp", "PRECIP", "REMOVEME1", "REMOVEME2", "REMOVEME3", "REMOVEME4", "REMOVEME5", "REMOVEME6", "Batt")
  data <- subset(data,select = c(1:6,12))
  data$datetime <- mdy_hms(paste(data$DATE, data$TIME))
  if(station == "KA"){ data$site_name <- "KC02.8"}
  if(station == "TH"){ data$site_name <- "TC00.6"}
  data = data[,c(9,8, 3:7)]

  new_file = paste("to_WISKI/",file, ".csv", sep="") # create the new file name
  write.csv(data, new_file, row.names = F, na = "") # write the new file
}
#-----------------------------------

temp_file = read.table(list.filenames[1], row.names = NULL, header = F)
archive_file = read.table(list.filenames[1], row.names = NULL, header = F)
colnames(temp_file) = c("DATE", "TIME", "Stage", "H2OTemp", "AirTemp", "MeanTurb", "Variance", "MedTurb", "MinTurb", "MaxTurb", " FTSTemp", "Rainage", "REMOVEME1", "REMOVEME2", "REMOVEME3", "REMOVEME4", "REMOVEME5", "REMOVEME6", "Batt")
temp_file = temp_file[,c(1:6,12)]
temp_file$Datetime = paste(temp_file$DATE, temp_file$TIME)
temp_file = temp_file[,c(8, 3:7)]
if(str_detect(list.filenames[1], "TC0.06") | str_detect(list.filenames[1], "TC00.6")){
  cat("    Detected Thorn Creek (SJ)...")
  temp_file$Station = "TC00.6"
  temp_file = temp_file[,c(7, 1:6)]
  start_date = mdy_hms(temp_file$Datetime[1])
  start_date = strtrim(start_date, 10)
  end_date = mdy_hms(temp_file$Datetime[nrow(temp_file)])
  end_date = strtrim(end_date, 10)
  my.file.rename(from = paste("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/_3_Incoming_Data/", list.filenames[i], sep=""),
                 to = paste("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/File Drop Archive/", list.filenames[i], ".new", sep=""))
  myname = paste("_To_WISKI/Continuous Station/TC0.06_", start_date, "_to_", end_date, ".csv", sep="")
  write.csv(temp_file, myname, row.names = F)
  cat(paste("successfully exported & archived", "\n"))