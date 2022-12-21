# Gerrit Bass Winter 2022/23
# workspace to test and develop individual aspects of a function before adding them to the main script

library(tidyverse)

file1 <- "proDSSdata_20221115.csv"


filename <- sapply(strsplit(file1,".csv"),"[",1) #removing ".csv" from file name for use in file naming

proDSS_data <- read.csv(file1, skip = 5)

cleaned_DSS <- proDSS_data %>%
  mutate(datetime = as.POSIXct(paste(DATE, TIME), format="%m/%d/%Y %I:%M:%S %p"))
  select(1:3,5:7,11,15,18:20) %>% 
  




# write csv file to "To_WISKI" field data folder
write_csv(cleaned_hobo,paste("R:\\_04_Project_Data\\Water_Quality\\_3_Incoming_Data\\_To_WISKI\\HOBO\\",filename,".csv",sep = ""))

# move file from file drop to file drop archive
move_to_archive(from = paste("R:/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/", filename,".csv", sep=""),
                to = paste("R:/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/File Drop Archive/", filename, ".csv", sep=""))
