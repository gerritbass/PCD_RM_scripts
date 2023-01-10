## Script to formatting Field and Lab data for input into WISKI
## Written by Gerrit Bass Fall/Winter 2022/2023 


#Run the "SETUP" & "FUNCTIONS (CURRENT)" sections and then run each lapply command that you need based on what data
# you need to change. 

# If you have log files or other files that or old/ not used as much, Run the functions in
# "FUNCTIONS (ARCHIVED) but make sure formatting and sitenames are properly set.


## SETUP____________________________________________________________________________________________________
# load in libraries
library(tidyverse)
library(readxl)
library(lubridate)# doesn't load with the rest of the tidyverse

#set working directory
setwd("R:\\_04_Project_Data\\Water_Quality\\_3_Incoming_Data\\FILE DROP")

# make lists of all file types
csv_files <- list.files(pattern = "*.csv")
xls_files <- list.files(pattern = "*.xls")
new_files <- list.files(pattern = "*.NEW")

#divide those lists further into data type based on strings in filename
anatek_files <-grep("General", xls_files, value = TRUE)
FC_files <- grep("FecalColi", xls_files, value = TRUE)
tablet_files <- grep("Field",xls_files, value = TRUE)

# Dividing and subdividing HOBO files
hobo_files <- grep("Data",csv_files, value = TRUE) # select HOBO files from file drop
hobo_air <- grep("Air",hobo_files, value = TRUE) # select air temp only files from HOBO files
hobo_water <- grep("Water",hobo_files, value = TRUE) # select water temp only files from HOBO files
air_and_water <- c(hobo_air,hobo_water) # combine air and water file name
hobo_SandWT <- setdiff(hobo_files,air_and_water) # remove air and water only files from HOBO files to get a vector with the rest of the normal HOBO files

calsheets <- grep("CalSheet",xls_files, value = TRUE) # subsets calibration sheets from xls files

# log files from Cow & Thorn or Fourmile downloaded from sations with SatLink
log_files <- grep("log",csv_files, value = TRUE)

# function used to move files from FILE DROP to FILE DROP archive
move_to_archive <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = to)
}

##FUNCTIONS (CURRENT) _______________________________________________________________________

# fUNCTIONS for each type of data

# ANATEK CLEANER
anatek_cleaner <- function(file){
  
  filename <- sapply(strsplit(file,".xls"),"[",1) #removing ".xls" from file name for use in file naming
  anatek_data <- read_excel(file) # reading in data
  
  #cleans data
  cleaned_anatek <- anatek_data %>%
    mutate(datetime = as.POSIXct(paste(CollectDate, CollectTime), format="%m/%d/%Y %H:%M")) %>% #joins date and time
    mutate(remark = ifelse(grepl("<",Result) == TRUE,"U","")) %>% #adds a remark column for qualifiers and puts a U in for non-detects 
    mutate(Result = str_replace(Result, "<0.0500","0.025")) %>% # replaces non-detect values with value equal to 1/2 their reporting limit
    mutate(Result = str_replace(Result, "<0.100","0.05")) %>%
    mutate(Result = str_replace(Result, "<1.00","0.5")) %>% 
    mutate(Result = as.numeric(Result)) %>% 
    select(datetime,CustomerSampleNumber, Param,Result,Units,remark,DetectionLimit,RepLimit) %>%  #chooses columns from original sheet to be in new sheet
    mutate(Param = str_replace(Param,fixed("Nitrate/N + Nitrite/N"),"Nitrate-Nitrite_as_N_F")) %>%
    mutate(Param = str_replace(Param,fixed("Nitrate/N"),"Nitrate-Nitrite_as_N_F")) %>%
    mutate(Param = str_replace(Param, "Total P","Total_P"))
    #mutate(replace(Param,c("Nitrate/N"),c("Nitrate-Nitrite_as_N_F")))
  # write csv file
  write_csv(cleaned_anatek,paste("R:\\_04_Project_Data\\Water_Quality\\_3_Incoming_Data\\_To_WISKI\\ANATEK\\",filename,".csv",sep = ""))
  
  # move file from file drop to file drop archive
  move_to_archive(from = paste("R:/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/", filename,".xls", sep=""),
                 to = paste("R:/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/File Drop Archive/", filename, ".xls", sep=""))
}

# FECAL COLIFORM CLEANER
FC_cleaner <- function(file){
  
  filename <- sapply(strsplit(file,".xls"),"[",1) #removing ".xls" from file name for use in file naming
  FC_data <- read_excel(file, 2) # reading in data from 2nd sheet of excel file
  
  #cleans data
  cleaned_FC <- FC_data %>%
    drop_na %>% #remove rows with NA's
    rename(FC = VALUE) %>% 
    mutate(DATE =as_date(DATE)) %>% # make date column a date
    select(2,1,3) #reorder
  
  # write csv file to "To_WISKI" field data folder
  write_csv(cleaned_FC,paste("R:\\_04_Project_Data\\Water_Quality\\_3_Incoming_Data\\_To_WISKI\\Field\\",filename,".csv",sep = ""))
  
  # move file from file drop to file drop archive
  move_to_archive(from = paste("R:/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/", filename,".xlsm", sep=""),
                  to = paste("R:/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/File Drop Archive/", filename, ".xls", sep=""))
}

#HOBO CLEANER

hobo_cleaner <-  function(file){
  
  filename <- sapply(strsplit(file,".csv"),"[",1) #removing ".csv" from file name for use in file naming
  hobo_data <- read_csv(file)
  
  site_name <- word(filename,1) # takes the first word of filname and sets is as "site_name" varible
  
  #cleans data
  cleaned_hobo <- hobo_data %>%
    mutate("Site_ID" = site_name) %>% #creates Site ID column and uses the extracted site name to fill it
    rename(Datetime = 2) %>% 
    select(Site_ID, Datetime , 5,6 ) %>% 
    rename( Site = 1, WT = 3, S = 4) %>%  # renames columns based on index
    mutate(S = S*3.281)
  
  
  # write csv file to "To_WISKI" field data folder
  write_csv(cleaned_hobo,paste("R:\\_04_Project_Data\\Water_Quality\\_3_Incoming_Data\\_To_WISKI\\HOBO\\",filename,".csv",sep = ""))
  
  # move file from file drop to file drop archive
  move_to_archive(from = paste("R:/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/", filename,".csv", sep=""),
                  to = paste("R:/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/File Drop Archive/", filename, ".csv", sep=""))
}

# HOBO AIR TEMP CLEANER

hobo_AIR_cleaner <-  function(file){
  
  filename <- sapply(strsplit(file,".csv"),"[",1) #removing ".csv" from file name for use in file naming
  hobo_data <- read.csv(file)
  
  site_name <- word(filename,1) # takes the first word of filname and sets is as "site_name" varible
  
  #cleans data
  cleaned_hobo <- hobo_data %>%
    rename( Site = 1, Datetime = 2, AirTemp = 3) %>%  # renames columns based on index
    mutate(Site = "DC0.51")
  
  
  # write csv file to "To_WISKI" field data folder
  write_csv(cleaned_hobo,paste("R:\\_04_Project_Data\\Water_Quality\\_3_Incoming_Data\\_To_WISKI\\HOBO\\",filename,".csv",sep = ""))
  
  # move file from file drop to file drop archive
  move_to_archive(from = paste("R:/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/", filename,".csv", sep=""),
                  to = paste("R:/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/File Drop Archive/", filename, ".csv", sep=""))
  
}

# HOBO WATER TEMP CLEANER

hobo_WATER_cleaner <-  function(file){
  
  filename <- sapply(strsplit(file,".csv"),"[",1) #removing ".csv" from file name for use in file naming
  hobo_data <- read.csv(file)
  
  site_name <- word(filename,1) # takes the first word of filname and sets is as "site_name" varible
  
  #cleans data
  cleaned_hobo <- hobo_data %>%
    select(1:3) %>% 
    rename( Site = 1, Datetime = 2, WT = 3) %>%  # renames columns based on index
    mutate(Site = "DC0.51")
  
  
  # write csv file to "To_WISKI" field data folder
  write_csv(cleaned_hobo,paste("R:\\_04_Project_Data\\Water_Quality\\_3_Incoming_Data\\_To_WISKI\\HOBO\\",filename,".csv",sep = ""))
  
  # move file from file drop to file drop archive
  move_to_archive(from = paste("R:/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/", filename,".csv", sep=""),
                  to = paste("R:/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/File Drop Archive/", filename, ".csv", sep=""))
  
}

#TABLET FIELD DATA CLEANER

tablet_cleaner <- function(file){
  
  filename <- sapply(strsplit(file,".xlsm"),"[",1) #removing ".xlsm" from file name for use in file naming
  tablet_data <- read_excel(file, sheet = 6)
  
  #cleans data
  cleaned_tablet <- tablet_data %>%
    select(1:3,6:12,14,17) %>% 
    rename(AirTemp = 3, WT = 4, BarPress = 5, DOPerc = 6, DO = 7, SPC = 8, pH = 9, TURB = 10, Q = 11, S = 12)
  
  # write csv file to "To_WISKI" field data folder
  write_csv(cleaned_tablet,paste("R:\\_04_Project_Data\\Water_Quality\\_3_Incoming_Data\\_To_WISKI\\Field\\",filename,".csv",sep = ""))
  
  # move file from file drop to file drop archive
  move_to_archive(from = paste("R:/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/", filename,".xlsm", sep=""),
                  to = paste("R:/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/File Drop Archive/", filename, ".xlsm", sep=""))
}

#KAM & THORN CONTINUOUS STATION DATA CLEANER
# double check "Site" is working correct
kamthorn_cleaner <- function(file){
  
  filename <- sapply(strsplit(file,"_"),"[",1)
  Site <- if_else(filename == '34R050',"30003D16","30002E60") #30003D16 is Kamiache GOES address and 30002E60 is the THorn GOES address 
  
  kamthorn_data <- read_table(file,col_names = FALSE)
  
  colnames(kamthorn_data) = c("DATE", "TIME", "S", "WT", "AirTemp", "TURB", "Variance", "MedTurb", "MinTurb", "MaxTurb", " FTSTemp", "PRECIP", "REMOVEME1", "REMOVEME2", "REMOVEME3", "REMOVEME4", "REMOVEME5", "REMOVEME6", "Batt")
  
  clean_kamthorn <- kamthorn_data %>%
    mutate(datetime = as.POSIXct(paste(DATE, TIME), format="%m/%d/%y %H:%M:%S")) %>%
    mutate(Site = Site) %>% 
    select(Site, datetime, S, WT, AirTemp, TURB, PRECIP)
  
  end_date <- tail(kamthorn_data$DATE, n =1) %>% 
    str_replace_all("/","")
  
  
  # write csv file to "To_WISKI" field data folder
  write_csv(clean_kamthorn,paste("R:\\_04_Project_Data\\Water_Quality\\_3_Incoming_Data\\_To_WISKI\\Continuous Station\\",filename,"_",end_date,".csv",sep = ""))
  
  # move file from file drop to file drop archive
  move_to_archive(from = paste("R:/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/", filename,".NEW", sep=""),
                  to = paste("R:/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/File Drop Archive/", filename,"_", end_date, ".NEW", sep=""))
}

#CALIBRATION SHEET ARCHIVER
  # Doesn't change file, just moves it 

calsheet_mover <- function(file){
  
  # get filename
  filename <- sapply(strsplit(file,".xlsm"),"[",1)
  
  #copy to new archive directory
  file.copy(from = paste("R:/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/", filename,".xlsm", sep = ""),
            to   = paste("R:/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/File Drop Archive/CalSheets/", filename,".xlsm", sep = ""))
  
  #remove file from file drop
  file.remove(paste("R:/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/", filename,".xlsm", sep = ""))
  
}

##FUNCTIONS (Archived) _______________________________________________________________________

#LOG FILE CLEANER
  # STILL NEEDS SOME WORK, make sure to change "log_file$Site =" for the file you are cleaning

log_file_cleaner <- function(file){
  
  filename <- sapply(strsplit(file,".csv"),"[",1)
  
  log_file <- read_csv(file, col_names = FALSE)
  
  names(log_file) = c("date","time","param","value","unit", "tag")
  
  log_file <-  subset(log_file, select = -c(5,6))
  
  log_file= log_file %>% 
    pivot_wider(names_from = param, values_from = value )
  
  log_file$Datetime = as_datetime(paste(log_file$date, log_file$time), format = "%m/%d/%Y %H:%M:%S")
  log_file$Site = "B9D000AC"
  log_file$REMARK = ""
  col_order <- c("Site","Datetime","REMARK","Stage temp","SpCond uscm","pH","ODO %sat","ODO mg/L","Turbid NTU","Stage")
  log_file = log_file[, col_order]
  log_file <-  rename(log_file,
                      WT = `Stage temp`,
                      SPC = `SpCond uscm`,
                      DOPerc = `ODO %sat`,
                      DO = `ODO mg/L`,
                      TURB = `Turbid NTU`,
                      S = `Stage`)
  
  # write csv file to "To_WISKI" field data folder
  write_csv(log_file,paste("R:\\_04_Project_Data\\Water_Quality\\_3_Incoming_Data\\_To_WISKI\\Continuous Station\\",filename,".csv",sep = ""))
  
  # move file from file drop to file drop archive
  move_to_archive(from = paste("R:/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/", filename,".csv", sep=""),
                  to = paste("R:/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/File Drop Archive/", filename,".csv", sep=""))

  }

##RUN FUNCTIONS MADE ABOVE ON DATA _________________________________________________________________________

#Anatek
lapply(anatek_files, anatek_cleaner)

#Fecal Coliform
lapply(FC_files, FC_cleaner)

#HOBO S & T
lapply(hobo_SandWT, hobo_cleaner)

#HOBO AIR TEMP ONLY (DRY CREEK)
lapply(hobo_air, hobo_AIR_cleaner)

#HOBO WATER TEMP ONLY (DRY CREEK)
lapply(hobo_water, hobo_WATER_cleaner)

#TABLET
lapply(tablet_files, tablet_cleaner)

#KAM AND THORN CONT.
lapply(new_files, kamthorn_cleaner)

#Calibration Sheets
lapply(calsheets,calsheet_mover)

#log file cleaner (CHECK FUNCTION BEFORE RUNNING!!)
lapply(log_files,log_file_cleaner)


