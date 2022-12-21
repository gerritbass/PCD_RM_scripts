# Script created by Anthony Hatcher and Nick Harris
# This version edited by Gerrit Bass Winter 2022/23

# This script will consolidate Vegitation monitoring data from excel sheets

#Check library for "readxl" and "tidyr" in order for R to read Excel sheets and obtain tools to properly arrange/sort data
# and Check library for Packages that enable R to export data to a new or current .xlsx file

#load in libraries needed
library(readxl)
library(tidyr)
library(dplyr)
library(openxlsx)
library(rio)

#Set working directory to "Combined_Datasheets" to access both 2019 and 2020 veg. monitoring datasheets
setwd("P:/Vegetation Monitoring/Mastersheet/Veg_Monitoring_Data/Combined_Datasheets")
rm(list = ls())

#Create first dataframe, "my_files", so that R reads Excel files within the working directory. All files should be read and processed
my_files <- list.files(pattern = "*.xls")
my_files

#Create another datatframe, "vegmon", where lapply will produce a list of the data from the "my_files" dataframe
#Vegmon will access "my_files" to retrieve and list all excel data present from sheet 10. The list of data will be located in "vegmon"
vegmon <-  lapply(my_files, function(i){x = read_excel(i, sheet = 10)
  x$file = i
  x
})

#"vegmon" is set to only read sheet 10 for each Excel file, being the "To Export" sheet
#Testing various Excel files in "vegmon" dataframe to ensure R is only reading sheet 10
vegmon[[1]]
vegmon[[2]]
vegmon[[9]]

#do.call will sort all the Excel data from sheet 10 in "vegmon" and bind the data to produce one main mastersheet containing all variables and values
#"vegmon" will produce a final mastersheet table for viewing
vegmon = do.call("rbind.data.frame", vegmon)

#This function will export the "vegmon" data table to a new Excel file named "Mastersheet_20192020"
export(vegmon,"Mastersheet_20192020.xlsx")
