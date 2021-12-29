# Motherscript for all PCD Internal & External Data. This script should do everything you need it to do 

# Output: - Updated .csv files for each CREEK, plus updated compiled .csv files for CT & KT. 
#         - Final PCD & Manchester laboratory .csv files are created for both KT & CT
#         Data can then be directly used for shiny applications

## Output of this script is one final, cleaned .csv with all of the following data attached:
##      Manchester Laboratories
##      Tablet Data


# Temporary warning muting
defaultW <- getOption("warn") 

options(warn = -1) 

### Import packages ###
library(plyr)
library(dplyr)
library(readxl)
library(xlsx)
library(lubridate)
library(tidyr)
library(stringr)  

# Warnings back on
options(warn = defaultW)


### RUN SCRIPT
#################################################################################################################################################################
############### Updating PCD Mastersheets with New Tablet Data
begin = Sys.time()
cat("", "\n")
cat(paste("Initiated Mother Script at ", begin, "\n", sep = ''))
cat("", "\n")

# Excel Function

xlsx.read<-function(path,sheet){
  read_excel(path,sheet=5)
}

# Tablet Cleaner Function
Tablet_Cleaner = function(input_dir, master_dir, output_dir, output_lab){
  # Creating a list of multiple xlsx files
  input_dir = input_dir
  list.filenames<-list.files(input_dir, ".xlsm", full.names = TRUE)
  list(list.filenames)
  d2f = as.numeric(length(list.filenames))
  if(d2f == 0){
    return(cat(paste("No data available to update", "\n")))
  }
  list.filenames = list.filenames[!grepl("~", list.filenames)]
  CT.import.list<-llply(list.filenames, function(i) read_excel(i,sheet=5))
  
  # Pre-Check for all files being compiled (checking for any mismatching formatting issues)
  starter = read_excel(list.filenames[1], sheet=5)
  for(i in 2:length(list.filenames)){
    added = read_excel(list.filenames[i], sheet = 5)
    tryCatch(rbind(starter,added), error=function(e){
      message = paste("FORMATTING ERROR!! CHECK FOR DATA/FORMATTING INCONSISTENCIES IN FILE  ", list.filenames[i])
      stop(message)
    })
    starter = rbind(starter, added)
  }
  
  CT <-ldply(CT.import.list)
  CT = CT[rowSums(is.na(CT)) != ncol(CT),]
  # Clean Time, Stage, and FC
  CT$Stage = as.character(CT$Stage)
  CT$'Fecal Coliform (cfu/100ml)' = as.character(CT$'Fecal Coliform (cfu/100ml)')
  
  # Importing Master Sheet to be Written
  CT_EXCEL = read_excel(master_dir)
  
  # Cleaning Master Sheet (Datetime/Stage)
  CT_EXCEL$Stage = as.character(CT_EXCEL$Stage)
  CT_EXCEL = CT_EXCEL[c(1:18)]
  if(output_lab == "Cow" | output_lab == "Thorn (Union)"){
    CT_EXCEL = CT_EXCEL[c(1:17)]
  }
  CT_EXCEL$`Fecal Coliform (cfu/100ml)` = as.character(CT_EXCEL$`Fecal Coliform (cfu/100ml)`)
  # Appending new data to Master Sheet
  FINAL_CT = bind_rows(CT_EXCEL, CT)
  FINAL_CT = FINAL_CT[!duplicated(FINAL_CT$Datetime),]
  
  if(output_lab == "Cow"){
    FINAL_CT$`Site ID`[FINAL_CT$`Site ID` == "COW02.66"] <- "COW2.66"
  }
  
  # Exporting Updated Master
  write.xlsx(as.data.frame(FINAL_CT), output_dir, row.names = FALSE, showNA = FALSE)
  rows_added = nrow(FINAL_CT) - nrow(CT_EXCEL)
  
  # Summary of PCD Updated Data
  cat(paste("   - Records added to", output_lab, "master: ", rows_added, "\n"))
  return(rows_added)
}

Tab_Compiler = function(site1_input, site2_input, output){
  site1 = read_excel(site1_input)
  site2 = read_excel(site2_input)
  site1$'Fecal Coliform (cfu/100ml)' = as.character(site1$'Fecal Coliform (cfu/100ml)')
  site2$'Fecal Coliform (cfu/100ml)' = as.character(site2$'Fecal Coliform (cfu/100ml)')
  final_compiled = bind_rows(site1, site2)
  write.xlsx(as.data.frame(final_compiled), output, row.names = FALSE, showNA = FALSE)
  
}

cat(paste("Beginning tablet compiler...","\n", sep = ''))

# Updating PCD Mastersheet with Tablet Updater Function

COW = Tablet_Cleaner("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Cow_and_Thorn_Creeks/Cow Creek/Stream_Flow_and_Water_Quality/Tablet_Data/_1_Raw",
                     "P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Cow_and_Thorn_Creeks/Cow Creek/Stream_Flow_and_Water_Quality/Tablet_Data/_2_Compiled/Compiled_Water_Quality.xlsx",
                     "P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Cow_and_Thorn_Creeks/Cow Creek/Stream_Flow_and_Water_Quality/Tablet_Data/_2_Compiled/Compiled_Water_Quality.xlsx",
                     "Cow")
THORN_UNION = Tablet_Cleaner("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Cow_and_Thorn_Creeks/Thorn Creek/Stream_Flow_and_Water_Quality/Tablet_Data/_1_Raw",
                             "P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Cow_and_Thorn_Creeks/Thorn Creek/Stream_Flow_and_Water_Quality/Tablet_Data/_2_Compiled/Compiled_Water_Quality.xlsx",
                             "P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Cow_and_Thorn_Creeks/Thorn Creek/Stream_Flow_and_Water_Quality/Tablet_Data/_2_Compiled/Compiled_Water_Quality.xlsx",
                             "Thorn (Union)")
CT_COMPILER = Tab_Compiler("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Cow_and_Thorn_Creeks/Cow Creek/Stream_Flow_and_Water_Quality/Tablet_Data/_2_Compiled/Compiled_Water_Quality.xlsx",
                           "P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Cow_and_Thorn_Creeks/Thorn Creek/Stream_Flow_and_Water_Quality/Tablet_Data/_2_Compiled/Compiled_Water_Quality.xlsx",
                           "P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Cow_and_Thorn_Creeks/Compiled_Water_Quality.xlsx")
KAMIACHE = Tablet_Cleaner("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Kamiache_and_Thorn_Creeks/Kamiache_Creek/Stream_Flow_and_Water_Quality/Tablet_Data/_1_Raw",
                          "P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Kamiache_and_Thorn_Creeks/Kamiache_Creek/Stream_Flow_and_Water_Quality/Tablet_Data/_2_Compiled/Compiled_Water_Quality.xlsx",
                          "P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Kamiache_and_Thorn_Creeks/Kamiache_Creek/Stream_Flow_and_Water_Quality/Tablet_Data/_2_Compiled/Compiled_Water_Quality.xlsx",
                          "Kamiache")
THORN_SJ = Tablet_Cleaner("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Kamiache_and_Thorn_Creeks/Thorn_Creek/Stream_Flow_and_Water_Quality/Tablet_Data/_1_Raw",
                          "P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Kamiache_and_Thorn_Creeks/Thorn_Creek/Stream_Flow_and_Water_Quality/Tablet_Data/_2_Compiled/Compiled_Water_Quality.xlsx",
                          "P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Kamiache_and_Thorn_Creeks/Thorn_Creek/Stream_Flow_and_Water_Quality/Tablet_Data/_2_Compiled/Compiled_Water_Quality.xlsx",
                          "Thorn (St. John)")
KT_Compiled = Tab_Compiler("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Kamiache_and_Thorn_Creeks/Kamiache_Creek/Stream_Flow_and_Water_Quality/Tablet_Data/_2_Compiled/Compiled_Water_Quality.xlsx",
                           "P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Kamiache_and_Thorn_Creeks/Thorn_Creek/Stream_Flow_and_Water_Quality/Tablet_Data/_2_Compiled/Compiled_Water_Quality.xlsx",
                           "P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Kamiache_and_Thorn_Creeks/Compiled_Water_Quality.xlsx")
PR_TRIBS_Compiled = Tablet_Cleaner("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Palouse_River_Tribs/Stream_Flow_and_Water_Quality/Tablet_Data/_1_Raw",
                                   "P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Palouse_River_Tribs/Stream_Flow_and_Water_Quality/Tablet_Data/_2_Compiled/Compiled_Water_Quality.xlsx",
                                   "P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Palouse_River_Tribs/Stream_Flow_and_Water_Quality/Tablet_Data/_2_Compiled/Compiled_Water_Quality.xlsx",
                                   "Palouse Tribs")



cat(paste("Tablet data successfully updated","\n", sep = ''))

##################################################################################################################################################################################

############# PCD Data Merging with Manchester Data for Cow/Thorn & Kamiache/Thorn

### cleaning function for water quality data from

RUN_IT <- function(){
  cat(paste("Beginning PCD Manchester Lab Data Merger", "\n"))
  clean<- function(df){
    rdat = df[,c("PR_STATION_NM","FA_START_DT","FA_START_TM","RS_CHAR_NM","RS_REPORTED_VALUE_NR","RS_UOM_NM"
                 ,"RS_REPORTING_LIMIT","RS_QUAL_CD")]
    colnames(rdat)<-c("Station","Date","Time","Parameter","Value","Units","RepLim", "QAQC")
    return(rdat)
  }
  
  
  #### import multiple csv files with the clean function (above) and removes all the QAQC data  #####
  
  import.multiple.csv<-function(mypath,mypattern){ #sets up function with var. mypath and pattern
    list.filenames<-list.files(mypath,mypattern, full.names = TRUE) #lists all the files in the working directory in the list called "list.filenames"
    import.list<-llply(list.filenames,read.csv) # Reads in all the files in the list with the read csv function
    clean.import<-ldply(import.list,clean)# Utilized the Clean function created above specifically for the Manchester lab data
    cleaner<-clean.import[clean.import[,1]!="",] ## removes all of the QA QC data
  }
  
  import<-import.multiple.csv("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Kamiache_and_Thorn_Creeks/Kamiache_Creek/Stream_Flow_and_Water_Quality/External_Lab_Data/Manchester Lab/Lab Data/_1_Raw_KT_Combined",".csv")
  levels(import$Station)
  import$Station<-revalue(import$Station,c("KCO6.7"="KC06.7","TC6.3"="TC06.3","TC0.4.6"="TC04.6","TC13.9"="TC13.6","TC013.6"="TC13.6", "TC0.06"="TC00.6"))
  write.csv(import,"P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Kamiache_and_Thorn_Creeks/Kamiache_Creek/Stream_Flow_and_Water_Quality/External_Lab_Data/Manchester Lab/Lab Data/_2_Cleaned/running_MLdata.csv",row.names=F)
  
  clean_m <- read.csv("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Kamiache_and_Thorn_Creeks/Kamiache_Creek/Stream_Flow_and_Water_Quality/External_Lab_Data/Manchester Lab/Lab Data/_2_Cleaned/running_MLdata.csv")
  
  
  ###connect site and date columns
  c_date <- as.data.frame(clean_m$Date)
  c_datetime = as.data.frame(paste(clean_m$Date, clean_m$Time))
  c_datetime[,1] = as.character(c_datetime[,1])
  c_datetime[,1] = mdy_hm(c_datetime[,1])
  c_datetime[,1] = as.Date(c_datetime[,1], format = "%Y-%m-%d %h:%m:%s")
  c_date1 <- format(c_date, format = "%m/%d/%Y")
  clean_m$NewDate <- c_date1[,1]
  sited <- as.data.frame(paste(clean_m$Station, clean_m$NewDate))
  clean_m["site_d"] <- sited[,1]
  
  ###round time in Man with lubridate
  dts <- as.data.frame(paste(clean_m$Date, clean_m$Time))
  dts[, 1] = as.character(dts[, 1])
  dts[,1] = mdy_hm(dts[, 1])
  dts[,1] = lubridate::round_date(dts[,1], "15 minutes")
  
  ### create new site_dt column
  clean_m["DT_round"] <- dts[,1]
  clean_m["site_DT_round"] <- as.data.frame(paste(clean_m$Station, clean_m$DT_round))
  
  ###QAQC concatenate
  clean_m$QAQC<-as.character(clean_m$QAQC)
  clean_m$QA_parameter<-ifelse(clean_m$QAQC== "U"|clean_m$QAQC=="J",paste(clean_m$Parameter,clean_m$QAQC,sep="-"),"")
  
  ###long to wide data set with spread
  cm<-clean_m[,c(12,1,11,4,5,10)]
  cm = cm[!duplicated(cm),]
  spread <- spread(cm, Parameter,Value) 
  detach(package:plyr)
  ###consolidate QAQC values into one row, connected to unique time values
  cm<-clean_m[,c(12,1,4,5,10,11,13)]
  spread2 <- spread(cm, Parameter,Value) 
  spread2$site_DT_round = as.character(spread2$site_DT_round)
  spreadnew = spread2 %>% 
    dplyr::group_by(site_DT_round) %>% 
    summarise(QAQC_combo = paste(QA_parameter, collapse = ", "))
  colnames(spreadnew) = c("site_DT_round", "QAQC_combo")
  spread$site_DT_round = as.character(spread$site_DT_round)
  detach(package:dplyr)
  options(warn = -1) 
  suppressWarnings(library(plyr))
  suppressWarnings(library(dplyr))
  options(warn = defaultW)
  
  ##### merge QAQC combo and spread
  s_spread<- merge (spread, spreadnew, by = "site_DT_round", all.x = T)
  
  ###########PCD DATA################
  ##import data
  PCD_k <- read_xlsx("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Kamiache_and_Thorn_Creeks/Kamiache_Creek/Stream_Flow_and_Water_Quality/Tablet_Data/_2_Compiled/Compiled_Water_Quality.xlsx")
  PCD_t <- read_xlsx("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Kamiache_and_Thorn_Creeks/Thorn_Creek/Stream_Flow_and_Water_Quality/Tablet_Data/_2_Compiled/Compiled_Water_Quality.xlsx")
  PCD_t = PCD_t[,c(1:17)]
  PCD_k = PCD_k[,c(1:17)]
  PCD_Data <- rbind(PCD_k, PCD_t)
  PCD_Data$`Site ID`<- suppressMessages(revalue(PCD_Data$`Site ID`,c("KCO6.7"="KC06.7","TC6.3"="TC06.3","TC0.4.6"="TC04.6","TC13.9"="TC13.6","TC013.6"="TC13.6", "TC0.06"="TC00.6")))
  
  ###combine site and date column
  date <- as.data.frame(PCD_Data$Datetime)
  date1 <- format(date, format = "%m/%d/%Y")
  PCD_Data$Date <- date1[,1]
  sited1 <- as.data.frame(paste(PCD_Data$'Site ID', PCD_Data$Date))
  PCD_Data["site_d"] <- sited1[,1]
  
  ##reformat date time and round
  dts1 <- as.data.frame(PCD_Data$Datetime)
  dts1[,1] = lubridate::round_date(dts1[,1], "15 minutes")
  
  ## create new site_dt column
  PCD_Data["DT_round"] <- dts1[,1]
  PCD_Data["site_DT_round"] <- as.data.frame(paste(PCD_Data$`Site ID`, PCD_Data$DT_round))
  pcd <- PCD_Data[,-c(1,2,18,20)]
  
  ##################MERGE FILES#########################
  pcd_ml = merge(s_spread, pcd, by = "site_DT_round",all.x=T)
  pcd_ml = data.frame(pcd_ml, stringsAsFactors = FALSE)
  final <- pcd_ml[,c(2:31)]
  
  #### Change this directory to the shiny app directory for the output
  write.csv(final, "P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Kamiache_and_Thorn_Creeks/Kamiache_Creek/Stream_Flow_and_Water_Quality/Master_Data/Water_Quality_Data/PCD_ML_merge.csv")
  write.csv(final, "P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/_2_R_Scripts/Shiny_Apps/KT2_App/PCD_ML_merge.csv")
  
  #################################################COW AND THORN######################################################
  
  ### cleaning function for water quality data from
  clean<- function(df){
    rdat = df[,c("PR_STATION_NM","FA_START_DT","FA_START_TM","RS_CHAR_NM","RS_REPORTED_VALUE_NR","RS_UOM_NM"
                 ,"RS_REPORTING_LIMIT","RS_QUAL_CD")]
    colnames(rdat)<-c("Station","Date","Time","Parameter","Value","Units","RepLim", "QAQC")
    return(rdat)
  }
  
  #### import multiple csv files with the clean function (above) and removes all the QAQC data  #####
  
  import.multiple.csv<-function(mypath,mypattern){ #sets up function with var. mypath and pattern
    list.filenames<-list.files(mypath,mypattern, full.names = TRUE) #lists all the files in the working directory in the list called "list.filenames"
    import.list<-llply(list.filenames,read.csv) # Reads in all the files in the list with the read csv function
    clean.import<-ldply(import.list,clean)# Utilized the Clean function created above specifically for the Manchester lab data
    cleaner<-clean.import[clean.import[,1]!="",] ## removes all of the QA QC data
  }
  
  import<-import.multiple.csv("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Cow_and_Thorn_Creeks/Cow Creek/Stream_Flow_and_Water_Quality/External_Lab_Data/Manchester Lab/Lab Data/_1_Raw_CT_Combined",".csv")
  import$Station<-suppressMessages(revalue(import$Station,c("COW 0.07"="COW0.07","COW 2.66"="COW2.66","COW02.66"="COW2.66", "COW 7.4"="COW07.4","COW7.4"="COW07.4","COW 7.5"="COW07.5","COW7.5"="COW07.5", "COW 11.96"="COW11.96","COW 18.03"="COW18.03","LTH 0.07"="LTH0.07","TH 0.12"="TH0.12","TH 3.6"="TH03.6","TH3.6"="TH03.6","TH 4.45"="TH04.45","TH4.47"="TH04.45", "TH 7.35"="TH07.35","TH7.35"="TH07.35")))
  write.csv(import,"P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Cow_and_Thorn_Creeks/Cow Creek/Stream_Flow_and_Water_Quality/External_Lab_Data/Manchester Lab/Lab Data/_2_Cleaned/running_MLdata.csv",row.names=F)
  clean_m <- read.csv("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Cow_and_Thorn_Creeks/Cow Creek/Stream_Flow_and_Water_Quality/External_Lab_Data/Manchester Lab/Lab Data/_2_Cleaned/running_MLdata.csv")
  
  ###connect site and date columns
  c_date <- as.data.frame(clean_m$Date)
  c_date[,1] = as.character(c_date[,1])
  c_date[,1] = mdy(c_date[,1])
  c_date1 <- format(c_date, format = "%m/%d/%Y")
  clean_m$NewDate <- c_date1[,1]
  sited <- as.data.frame(paste(clean_m$Station, clean_m$NewDate))
  clean_m["site_d"] <- sited[,1]
  
  ###round time in Man with lubridate
  dts <- as.data.frame(paste(clean_m$Date, clean_m$Time))
  dts[, 1] = as.character(dts[, 1])
  dts[,1] = mdy_hm(dts[, 1])
  dts[,1] = lubridate::round_date(dts[,1], "15 minutes")
  
  ### create new site_dt column
  clean_m["DT_round"] <- dts[,1]
  clean_m["site_DT_round"] <- as.data.frame(paste(clean_m$Station, clean_m$DT_round))
  
  ###QAQC concatenate
  clean_m$QAQC<-as.character(clean_m$QAQC)
  clean_m$QA_parameter<-ifelse(clean_m$QAQC== "U"|clean_m$QAQC=="J",paste(clean_m$Parameter,clean_m$QAQC,sep="-"),"")
  
  ###long to wide data set with spread
  clean_m$site_DT_round=as.character(clean_m$site_DT_round)
  cm<-clean_m[,c(12,1,11,4,5,10)]
  cm = cm[!duplicated(cm),]
  spread <- pivot_wider(cm, names_from = Parameter, values_from = Value)
  detach(package:plyr)
  ###consolidate QAQC values into one row, connected to unique time values
  cm<-clean_m[,c(12,1,4,5,10,11,13)]
  spread2 <- pivot_wider(cm, names_from = Parameter, values_from = Value) 
  spreadnew<-group_by(spread2,site_DT_round) %>% summarise(QAQC_combo=paste(QA_parameter, collapse = ", "))
  
  ##### merge QAQC combo and spread
  s_spread<- merge (spread, spreadnew, by = "site_DT_round", all.x = T)
  s_spread$site_d = as.character(s_spread$site_d)
  detach(package:dplyr)
  options(warn = -1)
  suppressWarnings(library(plyr))
  suppressWarnings(library(dplyr))
  options(warn = defaultW)
  ###########PCD DATA################
  
  ##import data##  
  PCD_c <- read_excel("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Cow_and_Thorn_Creeks/Cow Creek/Stream_Flow_and_Water_Quality/Tablet_Data/_2_Compiled/Compiled_Water_Quality.xlsx")
  PCD_th <- read_excel("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Cow_and_Thorn_Creeks/Thorn Creek/Stream_Flow_and_Water_Quality/Tablet_Data/_2_Compiled/Compiled_Water_Quality.xlsx")
  if(ncol(PCD_th) > 18){
    PCD_th = PCD_th[,c(1:17, 21)]
  }
  
  
  PCD_Data <- rbind(PCD_c, PCD_th)
  
  ###combine site and date column
  date <- as.data.frame(PCD_Data$Datetime)
  date1 <- format(date, format = "%m/%d/%Y")
  PCD_Data$Date <- date1[,1]
  sited1 <- as.data.frame(paste(PCD_Data$'Site ID', PCD_Data$Date))
  PCD_Data["site_d"] <- sited1[,1]
  
  ##reformat date time and round
  dts1 <- as.data.frame(PCD_Data$Datetime)
  dts1[,1] = lubridate::round_date(dts1[,1], "15 minutes")
  
  ## create new site_dt column
  PCD_Data["DT_round"] <- dts1[,1]
  PCD_Data["site_DT_round"] <- as.data.frame(paste(PCD_Data$'Site ID', PCD_Data$DT_round))
  pcd <- PCD_Data[,-c(1,2,19,21)]
  
  ##################MERGE FILES#########################
  pcd_ml = merge(s_spread, pcd, by = "site_d", all.x = TRUE)
  pcd_ml = data.frame(pcd_ml, stringsAsFactors = FALSE)
  pcd_ml = pcd_ml[,c(-11)]
  pcd_ml$Fecal.Coliform..cfu.100ml. = as.numeric(pcd_ml$Fecal.Coliform..cfu.100ml.)
  pcd_new = PCD_Data[PCD_Data$`Site ID` == 'COW2.66A' | PCD_Data$`Site ID` == 'COW02.66A',]
  A266 = data.frame(site_d = NA,
                    site_DT_round.x = NA,
                    Station = pcd_new$`Site ID`,
                    DT_round = pcd_new$DT_round,
                    Ammonia = NA,
                    Nitrate.Nitrite.as.N = NA,
                    Ortho.Phosphate = NA,
                    Suspended.Sediment.Concentration = NA,
                    Total.Phosphorus = NA,
                    QAQC_combo = NA,
                    Outside.Air.Temp....C. = pcd_new$`Outside Air Temp. (°C)`,
                    Notes = pcd_new$Notes,
                    Temperature..water...C. = pcd_new$`Temperature, water (°C)`,
                    Temperature..Sensor.Water...C. = pcd_new$`Temperature, Sensor Water (°C)`,
                    Bar.Press..in.hg. = pcd_new$`Bar Press (in hg)`,
                    Dissolved.Oxygen.... = pcd_new$`Dissolved Oxygen (%)`,
                    Dissolved.Oxygen..mg.L. = pcd_new$`Dissolved Oxygen (mg/L)`,
                    Specific.Conductivity..at.25.deg.C. = pcd_new$`Specific Conductivity (at 25 deg C)`,
                    pH = pcd_new$pH,
                    Transparency..water = pcd_new$`Transparency, water`,
                    Fecal.Coliform..cfu.100ml. = as.numeric(pcd_new$`Fecal Coliform (cfu/100ml)`),
                    Flow..cfs. = pcd_new$`Flow (cfs)`,
                    Flow.Notes = pcd_new$`Flow Notes`,
                    Transparency..W..Hannah.meter..NTU = pcd_new$`Transparency (W/ Hannah meter) NTU`,
                    Stage = pcd_new$Stage,
                    site_DT_round.y = pcd_new$site_DT_round,
                    site_DT = NA)
  A266$Station = "COW2.66A"
  A266$Ammonia = as.numeric(A266$Ammonia)
  A266$Nitrate.Nitrite.as.N= as.numeric(A266$Nitrate.Nitrite.as.N)
  A266$Ortho.Phosphate = as.numeric(A266$Ortho.Phosphate)
  A266$Suspended.Sediment.Concentration = as.numeric(A266$Suspended.Sediment.Concentration)
  A266$Total.Phosphorus = as.numeric(A266$Total.Phosphorus)
  A266$Temperature..Sensor.Water...C. = as.numeric(A266$Temperature..Sensor.Water...C.)
  A266$Temperature..water...C. = as.numeric(A266$Temperature..water...C.)
  A266$Bar.Press..in.hg. = as.numeric(A266$Bar.Press..in.hg.)
  pcd_final = suppressWarnings(bind_rows(pcd_ml, A266))
  final <- pcd_final[,c(3:5,9:11,13,16,18:32)]
  
  #### Saves file in various spots 
  
  # To Master WQ Data Folder
  write.csv(final, "P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Cow_and_Thorn_Creeks/Cow Creek/Stream_Flow_and_Water_Quality/Master_Data/Water_Quality_Data/PCD_ML_merge.csv",
            row.names=F)
  
  # For Shiny Application
  write.csv(final, "P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/_2_R_Scripts/Shiny_Apps/CT2_App/PCD_ML_merge.csv", row.names = T)
  
  # For Shiny Application (Beta Version)
  write.csv(final, "P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/_2_R_Scripts/Shiny_Apps/CT_Beta_App/PCD_ML_merge.csv", row.names = T)
  
  
  #################### PALOUSE TRIBS #################
  
  ### cleaning function for water quality data from
  clean<- function(df){
    rdat = df[,c("PR_STATION_NM","FA_START_DT","FA_START_TM","RS_CHAR_NM","RS_REPORTED_VALUE_NR","RS_UOM_NM"
                 ,"RS_REPORTING_LIMIT","RS_QUAL_CD")]
    colnames(rdat)<-c("Station","Date","Time","Parameter","Value","Units","RepLim", "QAQC")
    return(rdat)
  }
  
  #### import multiple csv files with the clean function (above) and removes all the QAQC data  #####
  
  import.multiple.csv<-function(mypath,mypattern){ #sets up function with var. mypath and pattern
    list.filenames<-list.files(mypath,mypattern, full.names = TRUE) #lists all the files in the working directory in the list called "list.filenames"
    import.list<-llply(list.filenames,read.csv) # Reads in all the files in the list with the read csv function
    clean.import<-ldply(import.list,clean)# Utilized the Clean function created above specifically for the Manchester lab data
    cleaner<-clean.import[clean.import[,1]!="",] ## removes all of the QA QC data
  }
  
  import<-import.multiple.csv("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Palouse_River_Tribs/Stream_Flow_and_Water_Quality/External_Lab_Data/Manchester Lab/Lab Data/_1_Raw",".csv")
  write.csv(import,"P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Palouse_River_Tribs/Stream_Flow_and_Water_Quality/External_Lab_Data/Manchester Lab/Lab Data/_2_Cleaned/running_MLdata.csv",row.names=F)
  clean_m <- read.csv("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Palouse_River_Tribs/Stream_Flow_and_Water_Quality/External_Lab_Data/Manchester Lab/Lab Data/_2_Cleaned/running_MLdata.csv")
  
  ###connect site and date columns
  c_date <- as.data.frame(clean_m$Date)
  c_date[,1] = as.character(c_date[,1])
  c_date[,1] = mdy(c_date[,1])
  c_date1 <- format(c_date, format = "%m/%d/%Y")
  clean_m$NewDate <- c_date1[,1]
  sited <- as.data.frame(paste(clean_m$Station, clean_m$NewDate))
  clean_m["site_d"] <- sited[,1]
  
  ###round time in Man with lubridate
  dts <- as.data.frame(paste(clean_m$Date, clean_m$Time))
  dts[, 1] = as.character(dts[, 1])
  dts[,1] = mdy_hm(dts[, 1])
  dts[,1] = lubridate::round_date(dts[,1], "15 minutes")
  
  ### create new site_dt column
  clean_m["DT_round"] <- dts[,1]
  clean_m["site_DT_round"] <- as.data.frame(paste(clean_m$Station, clean_m$DT_round))
  
  ###QAQC concatenate
  clean_m$QAQC<-as.character(clean_m$QAQC)
  clean_m$QA_parameter<-ifelse(clean_m$QAQC== "U"|clean_m$QAQC=="J",paste(clean_m$Parameter,clean_m$QAQC,sep="-"),"")
  
  ###long to wide data set with spread
  clean_m$site_DT_round=as.character(clean_m$site_DT_round)
  cm<-clean_m[,c(12,1,11,4,5,10)]
  cm = cm[!duplicated(cm),]
  cm$Parameter = as.character(cm$Parameter)
  spread <- pivot_wider(cm, names_from = Parameter,values_from = Value) 
  detach(package:plyr)
  ###consolidate QAQC values into one row, connected to unique time values
  cm<-clean_m[,c(12,1,4,5,10,11,13)]
  spread2 <- pivot_wider(cm, names_from = Parameter, values_from = Value) 
  spreadnew<-group_by(spread2,site_DT_round) %>% summarise(QAQC_combo=paste(QA_parameter, collapse = ", "))
  
  ##### merge QAQC combo and spread
  s_spread<- merge(spread, spreadnew, by = "site_DT_round", all.x = T)
  s_spread$site_d = as.character(s_spread$site_d)
  detach(package:dplyr)
  options(warn=-1)
  suppressWarnings(library(plyr))
  suppressWarnings(library(dplyr))
  options(warn = defaultW)
  ###########PCD DATA################
  
  ##import data##  
  PCD_PR <- read_excel("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Palouse_River_Tribs/Stream_Flow_and_Water_Quality/Tablet_Data/_2_Compiled/Compiled_Water_Quality.xlsx")
  PCD_Data <- PCD_PR
  
  ###combine site and date column
  date <- as.data.frame(PCD_Data$Datetime)
  date1 <- format(date, format = "%m/%d/%Y")
  PCD_Data$Date <- date1[,1]
  sited1 <- as.data.frame(paste(PCD_Data$'Site ID', PCD_Data$Date))
  PCD_Data["site_d"] <- sited1[,1]
  
  ##reformat date time and round
  dts1 <- as.data.frame(PCD_Data$Datetime)
  dts1[,1] = lubridate::round_date(dts1[,1], "15 minutes")
  
  ## create new site_dt column
  PCD_Data["DT_round"] <- dts1[,1]
  PCD_Data["site_DT_round"] <- as.data.frame(paste(PCD_Data$'Site ID', PCD_Data$DT_round))
  pcd <- PCD_Data[,-c(1,2,19,21)]
  
  pcd_ml = merge(s_spread, pcd, by = "site_d", all.x = TRUE)
  pcd_ml = data.frame(pcd_ml, stringsAsFactors = FALSE)
  final = pcd_ml[,c(3:10, 12:22,24,25)]
  
  #### Saves file in various spots 
  write.csv(final, "P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Palouse_River_Tribs/Stream_Flow_and_Water_Quality/Master_Data/Water_Quality_Data/PCD_ML_Merge.csv")
  cat(paste("Manchester Laboratory data updated and combined with PCD data", "\n"))
}
final = RUN_IT()