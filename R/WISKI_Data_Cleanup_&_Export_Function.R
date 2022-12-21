### WISKI Data Cleanup and Export Function ###
### Tal Atkins ###
### 11/7/22 ###

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(tools)

### SET WORKING DIRECTORY ###
setwd("R:/_04_Project_Data/from_WISKI/Sites/NFPR12") # <- User must manually change Sites sub-folder 
getwd()


### GETTING COLUMN NAMES FROM FILE NAMES ###
files <- list.files(pattern = "*.csv")
files<- files%>% 
  str_detect(pattern = ".log",negate = TRUE) %>% 
  keep(files, .) 

filename <- sapply(strsplit(files[1],"[_]"),`[`, 1)

### FUNCTION ###
data.import<-function(file){ #Cleaning function for PCD data
  data<- read_csv(file, skip = 15) #imports file
  data$datetime<-paste(data$Date, data$Time) #creates a datetime ID for Joining data sets
  data$datetime<-mdy_hms(data$datetime) #changes date time to posixct format
  data[data <(-100)] <- NA
  if (filename == "COW0.07" || filename =="DC0.51"||filename =="PAL112.4"|| filename =="SFC0.35"||filename == "STEPTOE0.70"|| filename =="STEPTOE5.23") {
    Parameter <- sapply(strsplit(file, "[[:punct:]]"), "[", 3)
  } else {
    Parameter <- sapply(strsplit(file, "[[:punct:]]"), "[", 2)
  }
  colnames(data)[3] = Parameter
  if (ncol(data) == 5){
    data <- data[,c(5,3)]# Cleans up data frame
  } else {
    data <- data[,c(4,3)]
  }
  return(data) #assigns the data frame
}

### Data cleanup ###
cleandatalist <- lapply(files, data.import)
cleandata <- cleandatalist %>% reduce(full_join, by="datetime")
cleandata <- cleandata %>% arrange(cleandata$datetime)
cleandata <- distinct(cleandata, .keep_all = FALSE)

### Add site name ###
cleandata$Sitename <- sapply(strsplit(files[1],"[_]"),`[`, 1)
alldata <- cleandata %>%
  select("Sitename", everything())

### Data export ###
if (filename == "COW0.07"){
  path_out <-  'R:\\_04_Project_Data\\from_WISKI\\WQ_Projects\\CowThorn'
} else if (filename == "DC0.51" || filename == "NFPR3" || filename == "NFPR5A" || filename == "NFPR6" || filename == "NFPR6P" || filename == "NFPR8" || filename == "NFPR8A" || filename == "NFPR9" || filename == "NFPR9B" || filename == "NFPR11" || filename == "NFPR12" ||  filename == "PAL112.4" || filename == "SFC0.35") {
  path_out <- 'R:\\_04_Project_Data\\from_WISKI\\WQ_Projects\\PRtribs'
} else if (filename == "STEPTOE0.70" || filename == "STEPTOE5.23"){
  path_out <- "R:\\_04_Project_Data\\from_WISKI\\WQ_Projects\\Steptoe"
}

setwd(path_out)
getwd()

begindate <- head(alldata$datetime, n=1) 
begindate <- sub(" .*", "", begindate)
  
enddate <- tail(alldata$datetime, n=1)
enddate <- sub(" .*", "", enddate)

write.csv(alldata, paste(filename, begindate, "to", enddate, ".csv"), row.names = FALSE)






