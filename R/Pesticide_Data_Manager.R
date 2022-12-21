library(tidyverse)
library(readxl)
library(lubridate)

#read in data
pest_data <- read_excel('P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Kamiache_and_Thorn_Creeks/Kamiache_Creek/Stream_Flow_and_Water_Quality/External_Lab_Data/Manchester Lab/Lab Data/_1_Raw_KT_Combined/2021PALField_NUT.xlsx')

# replace site names
pest_data$SiteCode[pest_data$SiteCode == "TH-1"] <- "TC00.6" 
pest_data$SiteCode[pest_data$SiteCode == 'KA-1'] <- "KC02.8" 
pest_data$SiteCode[pest_data$SiteCode == "DR-1"] <- "DC0.51"

# grab only site name,date, and parameters
pest_data_clean <-  pest_data[,c(2,5,8:13)]

# rename column names
colnames(pest_data_clean) = c("Site ID", "Datetime", "WT", "SPC", "pH", "DOPerc","DO","Q" )

pest_data_clean$Datetime <- as_datetime(pest_data_clean$Datetime)


write.csv(pest_data_clean,'P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/_3_Incoming_Data/_To_WISKI/Field/pest_data.csv', row.names = F, na = "")
