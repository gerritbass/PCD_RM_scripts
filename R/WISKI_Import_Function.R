library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

setwd("R:/_04_Project_Data/from_WISKI/Sites/COW007")

### CLEANING DATA AND CREATING DATA FRAMES FOR EACH SITE ###

data.import<-function(file){ #Cleaning function for PCD turbidity data
  data<-read_csv(file) #imports file
  data$datetime<-mdy_hm(data$datetime) #changes date time to posixct format
  turb$Turbidity<-as.numeric(turb$Turbidity)#changes turbidity to a numeric value
  turb$NEWID<-paste(turb$Site,turb$datetime) #creates a new ID for Joining data sets
  turb = turb[,c(1,2,4,3)]# Cleans up data frame 
  return(turb) #assigns the data frame
}














csv_list = dir()

files = lapply(csv_list, read.csv, skip = 15)

Cow_Data <- do.call(rbind, files)




files <- list.files() 
files <- files%>% 
  str_detect(pattern = ".csv",negate = FALSE) %>% 
  keep(files, .)
