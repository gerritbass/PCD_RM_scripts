#libraries
library(dplyr)
library(xlsx)
library(readxl)
library(lubridate)
library(stringr)
library(data.table)
library(taskscheduleR)


# Setting Working Directory
  setwd("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/_3_Incoming_Data")

# Function to Run Entire Code
  Runall = function(){
    
  # Main Checking Function to check for all data types from the FILE DROP folder
    data_checker = function(input_files, num_length_run, type){
      
      # Used to move files
        my.file.rename <- function(from, to) {
            todir <- dirname(to)
            if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
            file.rename(from = from,  to = to)
          }
          
        if(type == "xlsm"){ 
            cat(paste("Running .xlsm files - Field/Calibration/Fecal Coliform", "\n"))
          for(i in 1:d1f){
            cat("  Processing file ", as.numeric(i), " of ", as.numeric(d1f), "\n", sep='')
            
        # Fecal Coliform Check
          if(grepl("FecalColi", list.filenames[i])){
            cat("    Fecal Coliform Sheet...")
            myname = substring(list.filenames[i], 11)
            myname = substr(myname, 1, nchar(myname)-5)
            temp_file = read_excel(list.filenames[i], sheet=2)
            colnames(temp_file) = c("DATE", "SITE", "FC")
            temp_file = temp_file[,c(2,1,3)]
            temp_file$DATE = ymd(temp_file$DATE)
            temp_file = temp_file[!is.na(temp_file$FC),]  
            write.csv(temp_file, paste("_PCD_Archive/Field/Fecal_Coliform/", myname, ".csv",sep=""), row.names = F)
            write.csv(temp_file, paste("_To_WISKI/Field/", myname, ".csv",sep=""), row.names = F)
            my.file.rename(from = paste("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/_3_Incoming_Data/", list.filenames[i], sep=""),
                           to = paste("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/File Drop Archive/", myname, ".xlsm", sep=""))
            cat(paste("Successfully Exported & Archived.", "\n"))}
            
        # Calibration Sheet Check
          else if(grepl("CalSheet", list.filenames[i])){
            cat("    Calibration Sheet...")
            myname = substring(list.filenames[i], 11)
            myname = substr(myname, 1, nchar(myname)-5)
            temp_file = read_excel(list.filenames[i], sheet=3)
            running_cal = read.csv("_PCD_Archive/Calibration Data/Running_ProDSS_Calibration.csv")
            colnames(running_cal)[1] = "Datetime"
            
          # Compiling New Calibration Sheets
            final_cal = rbind(temp_file, running_cal)
            final_cal = final_cal[!duplicated(final_cal),]
            
          # Writing New Calibration Sheet - Compiled  
            write.csv(temp_file, paste("_PCD_Archive/Calibration Data/Archive/", myname, ".csv", sep=""))
            write.csv(final_cal, "_PCD_Archive/Calibration Data/Running_ProDSS_Calibration.csv", row.names = F)
            my.file.rename(from = paste("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/_3_Incoming_Data/", list.filenames[i], sep=""),
                           to = paste("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/File Drop Archive/", myname, ".xlsm", sep=""))
            cat(paste("Successfully Exported & Archived.", "\n"))}
            
        # Field Data Check
          else{
              temp_file = read_excel(list.filenames[i], sheet=5)
              myname = substring(list.filenames[i], 11)
              myname = substr(myname, 1, nchar(myname)-5)
              
              # Initial Data Check
                demerits = 0
                  if(nrow(temp_file) != 1){demerits = demerits+1}       # Checking row structure
                  if(is.na(temp_file$`Site ID`)){demerits = demerits+1} # Checking site ID presence
                  if(is.na(temp_file$Datetime)){demerits = demerits+1}  # Checking datetime presence
                  if(demerits > 0){
                  message = paste("COULD NOT TRANSFER ", myname, ".xlsm. CHECK FOR DATA/FORMATTING INCONSISTENCIES", sep="")
                  stop(message)}
                  else{
                if(ncol(temp_file) == 18){
                  temp_file = temp_file[,c(1:3, 6:14, 17)]
                  colnames(temp_file) = c('Site ID', 'Datetime', 'AirTemp', 'WT',
                                          'BarPress', 'DOPerc', 'DO', 'SPC',
                                          'pH', 'TURB', 'FC', 'Q', 'S')
                }
                else if(ncol(temp_file) == 17){
                      temp_file = temp_file[,c(1:3, 6:12, 14, 17)]
                      colnames(temp_file) = c('Site ID', 'Datetime', 'AirTemp', 'WT',
                                              'BarPress', 'DOPerc', 'DO', 'SPC',
                                              'pH', 'TURB', 'Q', 'S')
                      }
                temp_file$FC <- NULL
                filename = paste("_To_WISKI/Field/", myname, ".csv", sep="")
                write.csv(temp_file, file=filename, row.names = F, na = "")
                cat(paste('    Transfer of Field Data Complete with No Recorded Errors', '\n'))
                  }}
            # Appending new data to Field-Running file
              field_temp = read.csv("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/_3_Incoming_Data/_PCD_Archive/Field/Field-Running.csv")
              field_temp = field_temp[rowSums(is.na(field_temp)) != ncol(field_temp),] # Removing empty rows
              colnames(field_temp)[1] = "Site ID"
              field_final = rbind(field_temp, temp_file) # Appending temp_file (field sheet) to running field sheet
              write.csv(field_final, "P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/_3_Incoming_Data/_PCD_Archive/Field/Field-Running.csv", row.names=F) # Writing field sheet
            # Moves file from FILE DROP to the archive
              my.file.rename(from = paste("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/_3_Incoming_Data/", list.filenames[i], sep=""),
                         to = paste("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/File Drop Archive/", myname, ".xlsm", sep=""))
          }
        }
        
        if(type == "csv"){
          i=1
          cat(paste("Running .csv files - HydroSphere, HOBO, KorDSS & MEL", "\n"))
          for(i in 1:d2f){
            cat("  Processing file ", as.numeric(i), " of ", as.numeric(d2f), "\n", sep='')
            if(grepl("PAL", list.filenames[i]) & grepl("WaterChem", list.filenames[i])){
              cat("    Assuming WSDA Field Data...", "\n")
              temp_file = read.csv(list.filenames[i])
              
              
            }
            
            else if(grepl("KorDSS", list.filenames[i])){
              cat("    Assuming KorDSS Export...", "\n")
              
              # Have to read it again - skipping 5 lines
                temp_file = read.csv(list.filenames[i], skip = 5)
                temp_file = temp_file[,c(1:3,5,7,11,15,18:20)]
                unique(temp_file$SITE)
                temp_file$SITE = gsub("COW 266A", "COW2.66A", temp_file$SITE) 
                temp_file$SITE = gsub("COW 266", "COW2.66", temp_file$SITE) 
                temp_file$SITE = gsub("WC001", "WC0.01", temp_file$SITE) 
                temp_file$SITE = gsub("stoe07", "STEPTOE0.70", temp_file$SITE) 
                temp_file$SITE = gsub("SUF2731", "UFC27.31", temp_file$SITE) 
                temp_file$SITE = gsub("NFPR5a", "NFPR5A", temp_file$SITE) 
                temp_file$SITE = gsub("SFC035", "SFC0.35", temp_file$SITE) 
                temp_file$SITE = gsub("COW266", "COW2.66", temp_file$SITE) 
                temp_file$SITE = gsub("COW007", "COW0.07", temp_file$SITE) 
                temp_file$SITE = gsub("COW 266", "COW2.66A", temp_file$SITE) 
                temp_file$SITE = gsub("COW 266", "COW2.66A", temp_file$SITE) 
                temp_file$SITE = gsub("COW 266", "COW2.66A", temp_file$SITE) 
                temp_file$SITE = gsub("COW 266", "COW2.66A", temp_file$SITE) 
                temp_file$SITE = gsub("COW 266", "COW2.66A", temp_file$SITE) 
                temp_file$SITE = gsub("COW 266", "COW2.66A", temp_file$SITE)}
            
            else{
              temp_file = read.csv(list.filenames[i])
              if(ncol(temp_file) > 17){
                cat("    Assuming MEL Data...", "\n")
                mydate = temp_file[1,9]
                mydate = mdy(mydate)
                myfilename = paste("_To_WISKI/MEL/MEL", mydate, ".csv", sep="")
                myfilename2 = paste("_PCD_Archive/MEL/MEL", mydate, ".csv", sep="")
                write.csv(temp_file, myfilename, row.names = F, na = "")
                write.csv(temp_file, myfilename2, row.names = F, na = "")}
              
              else if(colnames(temp_file[1])=="UnixTimestamp"){
                cat("    Assuming HydroSphere Data...", "\n")
                if(grepl("Cow",list.filenames[i])){
                  cat("      Assuming Site is Cow Creek...", "\n")
                  temp_file$Site = "COW0.07"
                  temp_file$Datetime = ymd_hms(paste(temp_file$Date, temp_file$Time))
                  temp_file$REMARK = NA
                  temp_file = temp_file[,c(13, 14, 15, 4:9, 11)]
                  colnames(temp_file) = c("Site", "Datetime", "REMARK", "WT", "SPC", "pH", "DOPerc", "DO", "TURB", "S")
                  cat("        Quality Checking...", "\n")
                  
                  demerits = 0
                  
                  if(is.na(temp_file$Datetime[1])){demerits=demerits+1} # Check Date
                    suppressWarnings({temp_file$WT = as.numeric(temp_file$WT)})
                    suppressWarnings({temp_file$SPC = as.numeric(temp_file$SPC)})
                    suppressWarnings({temp_file$pH = as.numeric(temp_file$pH)})
                    suppressWarnings({temp_file$DOPerc = as.numeric(temp_file$DOPerc)})
                    suppressWarnings({temp_file$DO = as.numeric(temp_file$DO)})
                    suppressWarnings({temp_file$TURB = as.numeric(temp_file$TURB)})
                    suppressWarnings({temp_file$S = as.numeric(temp_file$S)})
                    temp_file[is.na(temp_file)] = 0
                    temp_file$REMARK = NA
                  
                  for(i in 1:nrow(temp_file)){
                    if(temp_file$WT[i] == 0 && temp_file$SPC[i] == 0 && temp_file$pH[i] == 0){
                      temp_file$WT[i] = NA
                      temp_file$SPC[i] = NA
                      temp_file$pH[i] = NA
                      temp_file$DOPerc[i] = NA
                      temp_file$DO[i] = NA
                      temp_file$TURB[i] = NA
                      temp_file$REMARK[i] = "EXO missing/pulled"}
                    
                    if(temp_file$S[i] == 0 || is.na(temp_file$S[i])){
                      temp_file$REMARK[i] = paste(temp_file$REMARK[i], "Campbell Scientific missing/pulled")}}
                    
                  myname = "COW"
                  mydate1 = temp_file[2,2]
                  mydate1 = ymd_hms(mydate1)
                  mydate1 = substring(mydate1, 0, 10)
                  mydate2 = temp_file[nrow(temp_file),2]
                  mydate2 = ymd_hms(mydate2)
                  mydate2 = substring(mydate2, 0, 10)
                  myfilename1 = paste("_To_WISKI/Continuous Station/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                  myfilename2 = paste("_PCD_Archive/Continuous Station/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                  write.csv(temp_file, myfilename1, row.names = F, na = "")
                  write.csv(temp_file, myfilename2, row.names = F, na = "")}
                
                if(grepl("Kam",list.filenames[i])){
                  cat("      Assuming Site is Kamiache Creek...", "\n")
                  temp_file$Site = "KC02.8"
                  temp_file$Datetime = ymd_hms(paste(temp_file$Date, temp_file$Time))
                  temp_file$REMARK = NA
                  temp_file = temp_file[,c(16, 17, 18, 4:7,9)]
                  colnames(temp_file) = c("Site", "Datetime", "REMARK", "S", "WT", "AirTemp", "TURB", "PRECIP")
                  cat("        Quality Checking...", "\n")
                  
                  demerits = 0
                  
                  if(is.na(temp_file$Datetime[1])){demerits=demerits+1} # Check Date
                    suppressWarnings({temp_file$WT = as.numeric(temp_file$WT)})
                    suppressWarnings({temp_file$TURB = as.numeric(temp_file$TURB)})
                    suppressWarnings({temp_file$S = as.numeric(temp_file$S)})
                    suppressWarnings({temp_file$AirTemp = as.numeric(temp_file$AirTemp)})
                    suppressWarnings({temp_file$PRECIP = as.numeric(temp_file$PRECIP)})
                    temp_file[is.na(temp_file)] = 0
                    temp_file$REMARK = NA
                  
                  for(i in 1:nrow(temp_file)){
                     if(temp_file$S[i] == 0 || is.na(temp_file$S[i])){
                      temp_file$REMARK[i] = paste(temp_file$REMARK[i], "Bubbler missing/pulled")}}                  
                
                  myname = "KAM"
                  mydate1 = temp_file[2,2]
                  mydate1 = ymd_hms(mydate1)
                  mydate1 = substring(mydate1, 0, 10)
                  mydate2 = temp_file[nrow(temp_file),2]
                  mydate2 = ymd_hms(mydate2)
                  mydate2 = substring(mydate2, 0, 10)
                  myfilename1 = paste("_To_WISKI/Continuous Station/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                  myfilename2 = paste("_PCD_Archive/Continuous Station/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                  write.csv(temp_file, myfilename1, row.names = F, na = "")
                  write.csv(temp_file, myfilename2, row.names = F, na = "")}                
                
                if(grepl("Thorn Creek_",list.filenames[i])){
                  cat("      Assuming Site is Thorn Creek - Saint John...", "\n")
                  temp_file$Site = "TC00.6"
                  temp_file$Datetime = ymd_hms(paste(temp_file$Date, temp_file$Time))
                  temp_file$REMARK = NA
                  temp_file = temp_file[,c(16,17,18,4:7,9)]
                  colnames(temp_file) = c("Site",
                                          "Datetime",
                                          "REMARK",
                                          "S",
                                          "WT", 
                                          "AirTemp",
                                          "TURB", 
                                          "PRECIP")
                  cat("        Quality Checking...", "\n")
                  demerits = 0
                  if(is.na(temp_file$Datetime[1])){demerits=demerits+1} # Check Date
                  suppressWarnings({temp_file$WT = as.numeric(temp_file$WT)})
                  suppressWarnings({temp_file$TURB = as.numeric(temp_file$TURB)})
                  suppressWarnings({temp_file$S = as.numeric(temp_file$S)})
                  suppressWarnings({temp_file$AirTemp = as.numeric(temp_file$AirTemp)})
                  suppressWarnings({temp_file$PRECIP = as.numeric(temp_file$PRECIP)})
                  temp_file[is.na(temp_file)] = 0
                  temp_file$REMARK = NA
                  
                  for(i in 1:nrow(temp_file)){
                    if(temp_file$S[i] == 0 || is.na(temp_file$S[i])){
                      temp_file$REMARK[i] = paste(temp_file$REMARK[i], "Bubbler missing/pulled")
                    }
                  }
                  myname = "Thorn_SJ"
                  mydate1 = temp_file[2,2]
                  mydate1 = ymd_hms(mydate1)
                  mydate1 = substring(mydate1, 0, 10)
                  mydate2 = temp_file[nrow(temp_file),2]
                  mydate2 = ymd_hms(mydate2)
                  mydate2 = substring(mydate2, 0, 10)
                  myfilename1 = paste("_To_WISKI/Continuous Station/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                  myfilename2 = paste("_PCD_Archive/Continuous Station/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                  write.csv(temp_file, myfilename1, row.names = F, na = "")
                  write.csv(temp_file, myfilename2, row.names = F, na = "")
                }
                if(grepl("Thorn Creek Union",list.filenames[i])){
                  cat("      Assuming Site is Thorn Creek - Uniontown...", "\n")
                  temp_file$Site = "TH0.12"
                  temp_file$Datetime = ymd_hms(paste(temp_file$Date, temp_file$Time))
                  temp_file$REMARK = NA
                  temp_file = temp_file[,c(14, 15, 16, 4:10, 12)]
                  colnames(temp_file) = c("Site",
                                          "Datetime",
                                          "REMARK",
                                          "PRECIP",
                                          "WT",
                                          "SPC", 
                                          "pH",
                                          "DOPerc", 
                                          "DO",
                                          "TURB",
                                          "S")
                  cat("        Quality Checking...", "\n")
                  demerits = 0
                  if(is.na(temp_file$Datetime[1])){demerits=demerits+1} # Check Date
                  suppressWarnings({temp_file$WT = as.numeric(temp_file$WT)})
                  suppressWarnings({temp_file$SPC = as.numeric(temp_file$SPC)})
                  suppressWarnings({temp_file$pH = as.numeric(temp_file$pH)})
                  suppressWarnings({temp_file$DOPerc = as.numeric(temp_file$DOPerc)})
                  suppressWarnings({temp_file$DO = as.numeric(temp_file$DO)})
                  suppressWarnings({temp_file$TURB = as.numeric(temp_file$TURB)})
                  suppressWarnings({temp_file$S = as.numeric(temp_file$S)})
                  
                  temp_file[is.na(temp_file)] = 0
                  temp_file$REMARK = NA
                  
                  for(i in 1:nrow(temp_file)){
                    if(temp_file$WT[i] == 0 && temp_file$SPC[i] == 0 && temp_file$pH[i] == 0){
                      temp_file$WT[i] = NA
                      temp_file$SPC[i] = NA
                      temp_file$pH[i] = NA
                      temp_file$DOPerc[i] = NA
                      temp_file$DO[i] = NA
                      temp_file$TURB[i] = NA
                      temp_file$REMARK[i] = "EXO missing/pulled"
                    }
                    if(temp_file$S[i] == 0 || is.na(temp_file$S[i])){
                      temp_file$REMARK[i] = paste(temp_file$REMARK[i], "Campbell Scientific missing/pulled")
                    }
                   }
                  myname = "Thorn_U"
                  mydate1 = temp_file[2,2]
                  mydate1 = ymd_hms(mydate1)
                  mydate1 = substring(mydate1, 0, 10)
                  mydate2 = temp_file[nrow(temp_file),2]
                  mydate2 = ymd_hms(mydate2)
                  mydate2 = substring(mydate2, 0, 10)
                  myfilename1 = paste("_To_WISKI/Continuous Station/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                  myfilename2 = paste("_PCD_Archive/Continuous Station/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                  write.csv(temp_file, myfilename1, row.names = F, na = "")
                  write.csv(temp_file, myfilename2, row.names = F, na = "")
                }
              }
                     
              else if(grepl("PST", list.filenames[i])){
                cat(paste("    Assuming HOBO File...", "\n"))
                myname = substring(list.filenames[i], 11)
                # Check NFPR
                if(str_detect(myname, "NFPR")){
                  myname = substring(myname, 0, 6)
                  cat(paste("    Assuming Site from NFPR...", myname, "\n",sep=''))
                  myname = str_trim(myname, side="right")
                  temp_file = temp_file[,c(1,2,5,6)]
                  colnames(temp_file) = c("Site ID", "Datetime", "WT", "S")  
                  temp_file$`Site ID` = myname
                  mydate1 = temp_file[1,2]
                  mydate1 = mdy_hms(mydate1)
                  mydate1 = substring(mydate1, 0, 10)
                  mydate2 = temp_file[nrow(temp_file),2]
                  mydate2 = mdy_hms(mydate2)
                  mydate2 = substring(mydate2, 0, 10)
                  temp_file$S = as.numeric(temp_file$S) * 3.28024
                  myfilename1 = paste("_To_WISKI/HOBO/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                  myfilename2 = paste("_PCD_Archive/HOBO/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                  write.csv(temp_file, myfilename1, row.names = F, na = "")
                  write.csv(temp_file, myfilename2, row.names = F, na = "")
                }
                # Check DRY
                if(str_detect(myname, "Dry")){
                  cat("    Assuming Site is Dry Creek...")
                  if(str_detect(myname, "Water")){
                    cat(paste("Water Temperature", "\n")) 
                    temp_file = temp_file[,c(1:3)]
                    colnames(temp_file) = c("Site ID", "Datetime", "WT")
                    temp_file$`Site ID` = "DC0.51"  
                    myname = "DC0.51_WT"
                    mydate1 = temp_file[1,2]
                    mydate1 = mdy_hms(mydate1)
                    mydate1 = substring(mydate1, 0, 10)
                    mydate2 = temp_file[nrow(temp_file),2]
                    mydate2 = mdy_hms(mydate2)
                    mydate2 = substring(mydate2, 0, 10)
                    myfilename1 = paste("_To_WISKI/HOBO/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                    myfilename2 = paste("_PCD_Archive/HOBO/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                    write.csv(temp_file, myfilename1, row.names = F, na = "")
                    write.csv(temp_file, myfilename2, row.names = F, na = "")
                  }
                  else if(str_detect(myname, "Air")){
                    cat(paste("Air Temperature", "\n")) 
                    temp_file = temp_file[,c(1:3)]
                    colnames(temp_file) = c("Site ID", "Datetime", "AirTemp")
                    temp_file$`Site ID` = "DC0.51"  
                    myname = "DC0.51_AIR"
                    mydate1 = temp_file[1,2]
                    mydate1 = mdy_hms(mydate1)
                    mydate1 = substring(mydate1, 0, 10)
                    mydate2 = temp_file[nrow(temp_file),2]
                    mydate2 = mdy_hms(mydate2)
                    mydate2 = substring(mydate2, 0, 10)
                    myfilename1 = paste("_To_WISKI/HOBO/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                    myfilename2 = paste("_PCD_Archive/HOBO/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                    write.csv(temp_file, myfilename1, row.names = F, na = "")
                    write.csv(temp_file, myfilename2, row.names = F, na = "")
                  }
                  else{
                    cat(paste("Stage", "\n")) 
                    temp_file = temp_file[,c(1,2,5,6)]
                    colnames(temp_file) = c("Site ID", "Datetime", "WT", "S")  
                    temp_file$`Site ID` = "DC0.51"
                    myname = "DC0.51_Stage"
                    mydate1 = temp_file[1,2]
                    mydate1 = mdy_hms(mydate1)
                    mydate1 = substring(mydate1, 0, 10)
                    mydate2 = temp_file[nrow(temp_file),2]
                    mydate2 = mdy_hms(mydate2)
                    mydate2 = substring(mydate2, 0, 10)
                    temp_file$S = as.numeric(temp_file$S) * 3.28024
                    myfilename1 = paste("_To_WISKI/HOBO/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                    myfilename2 = paste("_PCD_Archive/HOBO/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                    write.csv(temp_file, myfilename1, row.names = F, na = "")
                    write.csv(temp_file, myfilename2, row.names = F, na = "")
                    
                  }
                  
                }
                # Check 4MILE
                if(str_detect(myname, "Fourmile")){
                  cat(paste("    Assuming Site is Fourmile...", "\n"))
                  temp_file = temp_file[,c(1,2,5,6)]
                  colnames(temp_file) = c("Site ID", "Datetime", "WT", "S")  
                  temp_file$`Site ID` = "FMC0.28"
                  myname = "FMC0.28"
                  mydate1 = temp_file[1,2]
                  mydate1 = mdy_hms(mydate1)
                  mydate1 = substring(mydate1, 0, 10)
                  mydate2 = temp_file[nrow(temp_file),2]
                  mydate2 = mdy_hms(mydate2)
                  mydate2 = substring(mydate2, 0, 10)
                  temp_file$S = as.numeric(temp_file$S) * 3.28024
                  myfilename1 = paste("_To_WISKI/HOBO/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                  myfilename2 = paste("_PCD_Archive/HOBO/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                  write.csv(temp_file, myfilename1, row.names = F, na = "")
                  write.csv(temp_file, myfilename2, row.names = F, na = "")
                }
                # Check STEPTOE
                if(str_detect(myname, "Steptoe")){
                  myname = substring(list.filenames[i], 11)
                  myname = substring(myname, 8,8)
                  if(str_detect(myname, "0")){
                    cat(paste("    Assuming Site is Steptoe0.70...", "\n"))
                    temp_file = temp_file[,c(1,2,5,6)]
                    colnames(temp_file) = c("Site ID", "Datetime", "WT", "S")  
                    temp_file$`Site ID` = "STEPTOE0.70"
                    myname = "STEPTOE0.70"
                    mydate1 = temp_file[1,2]
                    mydate1 = mdy_hms(mydate1)
                    mydate1 = substring(mydate1, 0, 10)
                    mydate2 = temp_file[nrow(temp_file),2]
                    mydate2 = mdy_hms(mydate2)
                    mydate2 = substring(mydate2, 0, 10)
                    temp_file$S = as.numeric(temp_file$S)* 3.28024
                    temp_file$WT = as.numeric(temp_file$WT)
                    myfilename1 = paste("_To_WISKI/HOBO/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                    myfilename2 = paste("_PCD_Archive/HOBO/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                    write.csv(temp_file, myfilename1, row.names = F, na = "")
                    write.csv(temp_file, myfilename2, row.names = F, na = "")
                  }
                  else{
                    cat(paste("  Assuming Site is Steptoe5.23...", "\n"))
                    temp_file = temp_file[,c(1,2,5,6)]
                    colnames(temp_file) = c("Site ID", "Datetime", "WT", "S")  
                    temp_file$`Site ID` = "STEPTOE5.23"
                    myname = "STEPTOE5.23"
                    mydate1 = temp_file[1,2]
                    mydate1 = mdy_hm(mydate1)
                    mydate1 = substring(mydate1, 0, 10)
                    mydate2 = temp_file[nrow(temp_file),2]
                    mydate2 = mdy_hm(mydate2)
                    mydate2 = substring(mydate2, 0, 10)
                    temp_file$S = as.numeric(temp_file$S)* 3.28024
                    temp_file$WT = as.numeric(temp_file$WT)
                    myfilename1 = paste("_To_WISKI/HOBO/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                    myfilename2 = paste("_PCD_Archive/HOBO/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                    write.csv(temp_file, myfilename1, row.names = F, na = "")
                    write.csv(temp_file, myfilename2, row.names = F, na = "")
                  }
                }
                # Check WILBUR
                if(str_detect(myname, "WC")){
                  cat(paste("    Assuming Site is Wilbur Creek...", "\n",sep=''))
                  temp_file = temp_file[,c(1,2,5,6)]
                  colnames(temp_file) = c("Site ID", "Datetime", "WT", "S")  
                  temp_file$`Site ID` = "WC0.01"
                  myname = "WC0.01"
                  mydate1 = temp_file[1,2]
                  mydate1 = mdy_hms(mydate1)
                  mydate1 = substring(mydate1, 0, 10)
                  mydate2 = temp_file[nrow(temp_file),2]
                  mydate2 = mdy_hms(mydate2)
                  mydate2 = substring(mydate2, 0, 10)
                  temp_file$S = as.numeric(temp_file$S) * 3.28024
                  myfilename1 = paste("_To_WISKI/HOBO/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                  myfilename2 = paste("_PCD_Archive/HOBO/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                  write.csv(temp_file, myfilename1, row.names = F, na = "")
                  write.csv(temp_file, myfilename2, row.names = F, na = "")
                }
                # Check SPRING FLAT CREEK
                if(str_detect(myname, "SFC")){
                  cat(paste("    Assuming Site is Spring Flat Creek...", "\n",sep=''))
                  temp_file = temp_file[,c(1,2,5,6)]
                  colnames(temp_file) = c("Site ID", "Datetime", "WT", "S")  
                  temp_file$`Site ID` = "SFC0.35"
                  myname = "SFC0.35"
                  mydate1 = temp_file[1,2]
                  mydate1 = mdy_hms(mydate1)
                  mydate1 = substring(mydate1, 0, 10)
                  mydate2 = temp_file[nrow(temp_file),2]
                  mydate2 = mdy_hms(mydate2)
                  mydate2 = substring(mydate2, 0, 10)
                  temp_file$S = as.numeric(temp_file$S) * 3.28024
                  myfilename1 = paste("_To_WISKI/HOBO/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                  myfilename2 = paste("_PCD_Archive/HOBO/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                  write.csv(temp_file, myfilename1, row.names = F, na = "")
                  write.csv(temp_file, myfilename2, row.names = F, na = "")
                }
                # Check UNION FLAT CREEK
                if(str_detect(myname, "UFC")){
                  cat(paste("    Assuming Site is Union Flat Creek...", "\n",sep=''))
                  temp_file = temp_file[,c(1,2,5,6)]
                  colnames(temp_file) = c("Site ID", "Datetime", "WT", "S")  
                  temp_file$`Site ID` = "UFC27.31"
                  myname = "UFC27.31"
                  mydate1 = temp_file[1,2]
                  mydate1 = mdy_hms(mydate1)
                  mydate1 = substring(mydate1, 0, 10)
                  mydate2 = temp_file[nrow(temp_file),2]
                  mydate2 = mdy_hms(mydate2)
                  mydate2 = substring(mydate2, 0, 10)
                  temp_file$S = as.numeric(temp_file$S)
                  temp_file$S = as.numeric(temp_file$S) * 3.28024
                  myfilename1 = paste("_To_WISKI/HOBO/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                  myfilename2 = paste("_PCD_Archive/HOBO/", myname, "_From_", mydate1, "_to_", mydate2, ".csv", sep="") 
                  write.csv(temp_file, myfilename1, row.names = F, na = "")
                  write.csv(temp_file, myfilename2, row.names = F, na = "")
                  }
                }
              else{
                cat(paste("File: ", list.filenames[i], " could not be read and archived correctly. Please check.", sep=""))
              }
            }
            myname = substring(list.filenames[i], 11)
            myname = substr(myname, 1, nchar(myname)-4)
            my.file.rename(from = paste("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/_3_Incoming_Data/", list.filenames[i], sep=""),
                           to = paste("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/_3_Incoming_Data/FILE DROP/File Drop Archive/", myname, ".csv", sep=""))
          }
          cat(paste('  Transfer of HydroSphere/HOBO/MEL Data Complete with No Recorded Errors', '\n'))
        }
        if(type == "tsv"){
          cat(paste("Running .tsv files - MF PRO", "\n"))
          for(i in 1:d2f){
            cat("  Processing file ", as.numeric(i), " of ", as.numeric(d2f), "\n", sep='')
            profile_file = read.delim2(list.filenames[1]) # CHANGE ME TO [i]
            profile_file = profile_file[c(1,2,19),]
            temp_file = read.delim2(list.filenames[1], skip=29)
            mydate = substring(profile_file[2], 11, nchar(profile_file[2]))
            mydate = gsub(".", "/", mydate, fixed = TRUE)
            temp_file = temp_file[,c(1:4,6,8,31:33)]
            temp_file$Date = mydate
            temp_file$Station = profile_file[1]
              
          }  
        }
        if(type == "new"){
          cat(paste("Running .NEW files - Kamiache & Thorn Stations", "\n"))
          for(i in 1:d2f){
            cat("  Processing file ", as.numeric(i), " of ", as.numeric(d2f), "\n", sep='')
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
            }      
           }
          }
        cat(paste('Data Manager Task Ended', "\n"))
      } #run data checker
  
  # Field/Tablet Files
    input_dir = "FILE DROP"
    list.filenames<-list.files(input_dir, ".xlsm", full.names = TRUE)
    d1f = as.numeric(length(list.filenames))
    if(d1f>0){data_checker(list.filenames, d1f, "xlsm")}
  # HydroSphere/HOBO/MEL/KorDSS Files
    input_dir = "FILE DROP"
    list.filenames<-list.files(input_dir, ".csv", full.names = TRUE)
    d2f = as.numeric(length(list.filenames))
    if(d2f>0){data_checker(list.filenames, d2f, "csv")}
  # MF PRO Files
    input_dir = "FILE DROP"
    list.filenames<-list.files(input_dir, ".tsv", full.names = TRUE)
    d2f = as.numeric(length(list.filenames))
    if(d2f>0){data_checker(list.filenames, d2f, "tsv")}
  # Continuous Station Files 
    input_dir = "FILE DROP"
    list.filenames<-list.files(input_dir, ".new", full.names = TRUE)
    d2f = as.numeric(length(list.filenames))
    if(d2f>0){data_checker(list.filenames, d2f, "new")}
  }

# Run me
  Runall()
