# BMP Placement - Field Aggregation Script
  # Nick Harris - Adapted from Ames Fowler's OG script for HCT Project
  # Annotated by NH - 2/3/2022

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

list.filenames<-list.files("Input HCT Rasters", ".tif", full.names = TRUE) # Listing all the different images in the folder
setwd("P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/BMP_Placement_Tool (ECY1803)/R - Farm Field Aggregation")
farmfields1 <- sf::st_read("FarmFields.shp") # Change this to AllFarmFields_WSDA shapefile to get all the fields in the Palouse
farmfields1 <- farmfields1[,c(1,5)] # Clipping the data frame to have ID and calculated acres 
colnames(farmfields1)[1] = "Calculated_Acres"
farmfields = farmfields1
i=1

for(i in 1:length(list.filenames)){ # For loop to iterate through the list.filenames and work through each scenario (Conv., Mulch, No-till; all with and without buffers)
  myname = str_sub(list.filenames[i], 19, nchar(list.filenames[i])-4) # Just extracting the name of the raster image (e.g, CT)
  filename = substr(list.filenames[i], 19, nchar(list.filenames[i])-4) # Does the same thing as above for some reason? A bit redundant
  raster_ct_max <- raster(list.filenames[i]) # Reads the raster at the "i" position in the list. Will be 1st raster in the list.filenames if i=1
  ex_list <- exact_extract(raster_ct_max, farmfields) %>% lapply(., as.data.table) # This is where the rasters are "extracted" by the farmfields shapefile. 
                                                                                   # In theory, each polygon, or field, in the shapefile will extract the values out of all of the rasters within it.

  # This is the summary function. Transforms the farmfields shapefile to have values from ex_list appended to it
  farmfields2 <- farmfields %>% 
   mutate(erosion_max = ex_list %>% # This is taking the maximum erosion rate in each farm field
           map(., (function(x) x$value %>%  na.omit %>% max)) %>% unlist, # The way to think about this function would be that function(x) is calling for each "item" or "x" 
                                                                          # in the ex_list (kinda like "i" for a for-loop). For each "x", there is a column called "value" which
                                                                          # is being referenced here. This is the maximum erosion value here
         CSA_area = ex_list %>% 
           map(., (function(x) (((x %>%  na.omit)$value>5/4.46)*(x %>%  na.omit)$coverage_fraction) %>% sum)) %>% unlist, # This one is a little more tricky. Instead of calling for "max" value, I am asking to the function to 
                                                                                     # count how many cells are over 5 tons/acre (the conversion is for kg/ha i think? should double check to make sure)
                                                                                     # With the number of cells, this is where the area is getting messed up I believe. The output for this is the number of all cells that would
                                                                                     # over that 5 tons/acre criteria, but as I mentioned before, the ex_list will include all raster values for cells that are fully or partially within the field
                                                                                     # What happens below on line 48 is me multiplying the number of cells by 900 (# of cells * 900 m^2) then converting it to acres. I would look into a better way to calculate these acres...Ames would most likely know
                                                                                     
         perc_CSA = ex_list %>% # Not sure what this is doing here, but Percent CSA gets calculated down below. You could probably delete these two lines
           map(., (function(x) (((x %>%  na.omit)$value>5/4.46)*(x %>%  na.omit)$coverage_fraction) %>% sum)) %>% unlist)
  
  
  farmfields2$CSA_acres = round(((farmfields2$CSA_area*900)*0.000247105), 2) # CSA calculation I explained above, rounded to 2 decimal places
  farmfields2$Perc_CSA = round(((farmfields2$CSA_acres/farmfields$Calculated_Acres)*100), 2) # Percent of the field that is CSA. (CSA acres / Calculated Acres)
  farmfields2 = select(farmfields2, erosion_max, CSA_acres, Perc_CSA) # Trims the columns. Be sure to check which columns you want to keep before running this line
  colnames(farmfields2) = c(paste(myname,"_Sediment_Erosion", sep=""), paste(myname,"_CSA_Acres", sep=""), paste(myname, "_Percent_CSA", sep="")) # Cleaning columns
  farmfields1 = cbind(farmfields1, farmfields2) # Combines the original farm field shapefile with the calculated maximum erosion and acreages
  farmfields1 = farmfields1[,-ncol(farmfields1)] # Trims columns one more time
  
}  


setwd("P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/BMP_Placement_Tool (ECY1803)/R - Farm Field Aggregation/Output")
farmfields1 %>% # This writes the shapefile
  st_write(dsn = '.',layer = paste('All_20220204', sep=""), driver = 'ESRI Shapefile') 

  
  # ggplot(farmfields)+
  #   geom_sf(aes(fill= CSA_area*900/10000))+
  #   scale_fill_viridis_c()+theme_minimal()+
  #   coord_sf(datum = st_crs(huc12))+ 
  #   labs(fill = "CSA area [ha]",
  #        x = "Easting (m)", 
  #        y = "Northing (m)")+ 
  #   theme(axis.text.x=element_text(angle = -45))
