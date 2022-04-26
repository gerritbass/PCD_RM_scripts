### EIM Creator
# The following functions will pull all Manchester Lab data, compile it, and spit it out in the correct columns for the EIM
# **Note, you should still double check the resulting .csv to make sure all the data transferred over

library(lattice)
library(latticeExtra)
library(MASS)
library(ggplot2)
library(visreg)
library(plyr)
library(gridExtra)
library(reshape2)
library(scales)
library(lubridate)
library(Hmisc)
library(Kendall)
library(tidyr)
library(dplyr)
library(readxl)
library(tidyverse)

EIM_clean<- function(df){
  rdat = df[,c("PR_STATION_NM","FA_START_DT","FA_START_TM", "SM_IDENT_NR", "SM_COMPOSITE_FL", "SM_MATRIX_DS",
               "SM_SOURCE_DS", "RS_CHAR_NM", "RS_ANALYSIS_DT", "RS_REPORTED_VALUE_NR",
               "RS_UOM_NM", "RS_METHOD_CD", "RS_LAB_NM", "RS_REPORTING_LIMIT", "RS_REPORTING_LIMIT_TYPE", 
               "RS_DETECTION_LIMIT", "RS_DETECTION_LIMIT_TYPE", "RS_QUAL_CD", "RS_SAMPLE_FRAC")]
  colnames(rdat)<-c("Study_Specific_Location_ID","Field_Collection_Start_Date",
                    "Field_Collection_Start_Time", "Sample_ID",
                    "Sample_Composite_Flag", "Sample_Matrix", "Sample_Source", "Result_Parameter_Name", "Lab_Analysis_Date",
                    "Result_Value", "Result_Value_Units", "Result_Method", "Result_Lab_Name", "Result_Reporting_Limit", 
                    "Result_Reporting_Limit_Type","Result_Detection_Limit", "Result_Detection_Limit_Type", "Result_Data_Qualifier", 
                    "Fraction_Analyzed")
  

  return(rdat)
}

#### import multiple csv files with the clean function (above) and removes all the QAQC data  #####

import.multiple.csv<-function(Input, pattern, Output, study_ID){ #sets up function with var. mypath and pattern
  cat(paste("Initiating EIM Cleaner function", "\n"))
  cat("\n")
  cat(paste("Importing files...", "\n"))
  list.filenames<-list.files(Input, pattern, full.names = TRUE) #lists all the files in the working directory in the list called "list.filenames"
  import.list<-llply(list.filenames,read.csv) # Reads in all the files in the list with the read csv function

  rdat<-ldply(import.list,EIM_clean)# Utilized the Clean function created above specifically for the Manchester lab data
  rdat = rdat[!rdat$Study_Specific_Location_ID == "",]
  cat(paste("Files imported", "\n"))
  cat(paste("Importing template...", "\n"))
  EIM_temp = read_excel("P:/Research_and_Monitoring/_05_Deliverables/_1_Draft/EIM/EIM_Result_Template.xlsx")
  my_num = nrow(rdat)
  
  for(i in 1:nrow(rdat)){
    EIM_temp[nrow(EIM_temp)+1,] <- NA
  }
  
  cat(paste("Setting template...", "\n"))
  EIM_temp[,1] = study_ID
  EIM_temp$Study_Specific_Location_ID = rdat$Study_Specific_Location_ID
  EIM_temp$Field_Collection_Type = "Sample"
  EIM_temp$Field_Collector = "ConsDistrict"
  EIM_temp$Field_Collection_Start_Date = rdat[,2]
  EIM_temp$Field_Collection_Start_Time = rdat[,3]
  EIM_temp$Sample_ID = rdat$Sample_ID
  EIM_temp$Sample_Composite_Flag = rdat$Sample_Composite_Flag
  EIM_temp$Sample_Matrix = rdat$Sample_Matrix
  EIM_temp$Sample_Source = rdat$Sample_Source
  EIM_temp$Result_Parameter_Name = rdat$Result_Parameter_Name
  EIM_temp$Lab_Analysis_Date = rdat$Lab_Analysis_Date
  EIM_temp$Result_Value = rdat$Result_Value
  EIM_temp$Result_Value_Units = rdat$Result_Value_Units
  EIM_temp$Result_Reporting_Limit = rdat$Result_Reporting_Limit
  EIM_temp$Result_Reporting_Limit_Type = rdat$Result_Reporting_Limit_Type
  EIM_temp$Result_Detection_Limit = rdat$Result_Detection_Limit
  EIM_temp$Result_Detection_Limit_Type = rdat$Result_Detection_Limit_Type
  EIM_temp$Result_Data_Qualifier = rdat$Result_Data_Qualifier
  EIM_temp$Result_Method = rdat$Result_Method
  EIM_temp$Result_Lab_Name = rdat$Result_Lab_Name
  EIM_temp$Fraction_Analyzed = rdat$Fraction_Analyzed
  EIM_temp$Sample_Composite_Flag = "N"
  EIM_temp$Field_Filtered_Flag = "N"
  
  for(i in 1:nrow(EIM_temp)){
    if(EIM_temp$Result_Parameter_Name[i] == "Ortho-Phosphate"){
      EIM_temp$Field_Filtered_Flag[i] = "Y"
    }
  }
  
# Cleaning KT
   if(grepl("Kamiache", Input)){
    # Check Site Names
      for(i in 1:nrow(EIM_temp)){
        if(EIM_temp$Study_Specific_Location_ID[i] == "TC13.9"){
          EIM_temp$Study_Specific_Location_ID[i] == "TC13.6"
        }
        
        if(EIM_temp$Study_Specific_Location_ID[i] == "TC013.6"){
          EIM_temp$Study_Specific_Location_ID[i] == "TC13.6"
        }
        if(EIM_temp$Study_Specific_Location_ID[i] == "TC9.8"){
          EIM_temp$Study_Specific_Location_ID[i] == "TC09.8"
        }
        if(EIM_temp$Study_Specific_Location_ID[i] == "TC6.3"){
          EIM_temp$Study_Specific_Location_ID[i] == "TC06.3"
        }
        if(EIM_temp$Study_Specific_Location_ID[i] == "TC2.6"){
          EIM_temp$Study_Specific_Location_ID[i] == "TC02.6"
        }
        if(EIM_temp$Study_Specific_Location_ID[i] == "TC0.4.6"){
          EIM_temp$Study_Specific_Location_ID[i] == "TC04.6"
        }
        if(EIM_temp$Study_Specific_Location_ID[i] == "TC0.06"){
          EIM_temp$Study_Specific_Location_ID[i] == "TC00.6"
        }
      }
  # Setting Location IDs
    EIM_temp$Location_ID = paste("EFF46600-", EIM_temp$Study_Specific_Location_ID, sep="")  
    }
  
  cat(paste("Template filled", "\n"))
  cat(paste("Removing duplicates...", "\n"))
  start_num = nrow(EIM_temp)
  EIM_temp = unique(EIM_temp)
  end_num = nrow(EIM_temp)
  tot_rm = start_num - end_num
  cat(paste("Duplicates removed - ", tot_rm, " rows removed", "\n", sep=""))
  cat(paste("Exporting...", "\n"))
  write.csv(EIM_temp, Output, na = "", row.names = FALSE)
  cat(paste("EIM file successfully exported", "\n"))
  return(EIM_temp)
  
  
}

# Use import.multiple.csv function to pull all the ML lab data, use .csv for the second argument, and use the EIM Study_ID as the third argument
# import.multiple.csv(Input, pattern, Output, study_ID)
# To find the correct pathways, navigate to this file on file explorer:
##      P:\Research_and_Monitoring\_03_References\SOPs\PCD Internal SOPs\Final\SOP for EIM Data Management

CT <- import.multiple.csv("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Cow_and_Thorn_Creeks/Cow Creek/Stream_Flow_and_Water_Quality/External_Lab_Data/Manchester Lab/Lab Data/_1_Raw_CT_Combined",
                           ".csv",
                           "P:/Research_and_Monitoring/_05_Deliverables/_1_Draft/EIM/COW_THORN/COW_THORN_EIM.csv",
                            "WQC-2016-ECY-00XX")

KT <- import.multiple.csv("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Kamiache_and_Thorn_Creeks/Kamiache_Creek/Stream_Flow_and_Water_Quality/External_Lab_Data/Manchester Lab/Lab Data/_1_Raw_KT_Combined",
                          ".csv",
                          "P:/Research_and_Monitoring/_05_Deliverables/_1_Draft/EIM/KAMIACHE_THORN/KAMIACHE_THORN_EIM.csv",
                          "WHM_EFF4")

PR <- import.multiple.csv("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Palouse_River_Tribs/Stream_Flow_and_Water_Quality/External_Lab_Data/Manchester Lab/Lab Data/_1_Raw",
                          ".csv",
                          "P:/Research_and_Monitoring/_05_Deliverables/_1_Draft/EIM/NFPR/PR_EIM.csv",
                          "WQC-2018-0010")


# Import new format of MEL .csvs and merge
setwd("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Palouse_River_Tribs/Stream_Flow_and_Water_Quality/External_Lab_Data/Manchester Lab/Lab Data/_1_Raw/MEL_New_Format")

new_MEL <- list.files(path = "P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Palouse_River_Tribs/Stream_Flow_and_Water_Quality/External_Lab_Data/Manchester Lab/Lab Data/_1_Raw/MEL_New_Format",
                      pattern = "\\.csv$",
                      full.names = TRUE) %>%
  map_df(~read_csv(.x, col_types = cols(.default = "c")))
new_MEL <- new_MEL[,c(1:60)]
new_MEL[is.na(new_MEL)] <- ""

# Merge PR with new_MEL using rbind
PR <- sapply(PR, as.character)
PR[is.na(PR)] <- ""
NFSF_EIM <- rbind(PR, new_MEL)

write.csv(NFSF_EIM, "P:/Research_and_Monitoring/_05_Deliverables/_1_Draft/EIM/PALOUSE_TRIBS/PR_TRIBS_EIM.csv")
