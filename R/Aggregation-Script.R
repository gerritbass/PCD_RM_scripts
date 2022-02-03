# BMP Placement - Field Aggregation Script
  # Nick Harris - Adapted from Ames Fowler's OG script for HCT Project

library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)
library(sf)
library(data.table)
library(exactextractr)
library(purrr)
library(stringr)

setwd("P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/BMP_Placement_Tool (ECY1803)/R - Farm Field Aggregation")

list.filenames<-list.files("Input HCT Rasters", ".tif", full.names = TRUE)
setwd("P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/BMP_Placement_Tool (ECY1803)/R - Farm Field Aggregation")
farmfields1 <- sf::st_read("FarmFields.shp")
farmfields1 <- farmfields1[,c(1,5)]
colnames(farmfields1)[1] = "Calculated_Acres"
farmfields = farmfields1
i=1

for(i in 1:length(list.filenames)){
  myname = str_sub(list.filenames[i], 19, nchar(list.filenames[i])-4)
  filename = substr(list.filenames[i], 19, nchar(list.filenames[i])-4)
  raster_ct_max <- raster(list.filenames[i])
  ex_list <- exact_extract(raster_ct_max, farmfields) %>% lapply(., as.data.table) # this will pull all cells and cell cover 

  # could write the summary func here. 
  farmfields2 <- farmfields %>% 
   mutate(erosion_max = ex_list %>% 
           map(., (function(x) x$value %>%  na.omit %>% max)) %>% unlist, # mean erosion rate
         CSA_area = ex_list %>% 
           map(., (function(x) (((x %>%  na.omit)$value>5/4.46)*(x %>%  na.omit)$coverage_fraction) %>% sum)) %>% unlist, #number of cells above 5t/ac 
         perc_CSA = ex_list %>%
           map(., (function(x) (((x %>%  na.omit)$value>5/4.46)*(x %>%  na.omit)$coverage_fraction) %>% sum)) %>% unlist)
  
  
  farmfields2$CSA_acres = round(((farmfields2$CSA_area*900)*0.000247105), 2)
  farmfields2$Perc_CSA = round(((farmfields2$CSA_acres/farmfields$Calculated_Acres)*100), 2)
  farmfields2 = farmfields2[,c(3,5,6)]
  colnames(farmfields2) = c(paste(myname,"_Sediment_Erosion", sep=""), paste(myname,"_CSA_Acres", sep=""), paste(myname, "_Percent_CSA", sep=""))
  farmfields1 = cbind(farmfields1, farmfields2)
  farmfields1 = farmfields1[,-ncol(farmfields1)]
  
}  

setwd("P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/BMP_Placement_Tool (ECY1803)/R - Farm Field Aggregation/Output")
farmfields1 %>% 
  st_write(dsn = '.',layer = paste('All_Final_20220202', sep=""), driver = 'ESRI Shapefile')

  
  
  
  # ggplot(farmfields)+
  #   geom_sf(aes(fill= CSA_area*900/10000))+
  #   scale_fill_viridis_c()+theme_minimal()+
  #   coord_sf(datum = st_crs(huc12))+ 
  #   labs(fill = "CSA area [ha]",
  #        x = "Easting (m)", 
  #        y = "Northing (m)")+ 
  #   theme(axis.text.x=element_text(angle = -45))
