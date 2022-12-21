library(tidyverse)
library(lubridate)



#FOR COW007

temp_file <- read.csv('P:\\Research_and_Monitoring\\_04_Project_Data\\Water_Quality\\_3_Incoming_Data\\FILE DROP\\File Drop Archive\\Thorn Creek Union_log_20220210_20220308.csv', header =  FALSE)

names(temp_file) = c("date","time","param","value","unit", "tag")
mydate1 <-  temp_file[2,1] %>% 
  str_replace_all("/","-")
mydate2 = temp_file[nrow(temp_file),1] %>% 
  str_replace_all("/","-")
temp_file = subset(temp_file, select = -c(5,6))
temp_file= temp_file %>% 
  pivot_wider(names_from = param, values_from = value )
temp_file$Datetime = as_datetime(paste(temp_file$date, temp_file$time), format = "%m/%d/%Y %H:%M:%S")
temp_file$Site = "COW0.07" 
col_order <- c("Site","Datetime","Stage temp","SpCond uscm","pH","ODO %sat","ODO mg/L","Turbid NTU","Stage")
temp_file = temp_file[, col_order]
temp_file <-  rename(temp_file,
       WT = `Stage temp`,
       SPC = `SpCond uscm`,
       DOPerc = `ODO %sat`,
       DO = `ODO mg/L`,
       TURB = `Turbid NTU`,
       S = `Stage`)


mydate2 = temp_file[nrow(temp_file),1]
mydate2 = ymd_hms(mydate2)
mydate2 = substring(mydate2, 0, 10)

#FOR THORN NEAR UNIONTOWN

temp_file2 <- read.csv('P:\\Research_and_Monitoring\\_04_Project_Data\\Water_Quality\\_3_Incoming_Data\\FILE DROP\\Thorn Creek Union_log_20220104_20220113 2.csv')

names(temp_file2) = c("date","time","param","value","unit", "tag")
temp_file2 = subset(temp_file2, select = -c(5,6))
temp_file2 = temp_file2 %>% 
  pivot_wider(names_from = param, values_from = value )
temp_file2$Datetime = mdy_hms(paste0(temp_file2$date, temp_file2$time))
temp_file2$Site = "TC00.6" 
col_order <- c("Site","Datetime","Stage temp","SpCond uscm","pH","ODO %sat","ODO mg/L","Turbid NTU","Stage")
temp_file2 = temp_file2[, col_order]
temp_file2 <-  rename(temp_file2,
                     WT = `Stage temp`,
                     SPC = `SpCond uscm`,
                     DOPerc = `ODO %sat`,
                     DO = `ODO mg/L`,
                     TURB = `Turbid NTU`,
                     S = `Stage`)

#this

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
