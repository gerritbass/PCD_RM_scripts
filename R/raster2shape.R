
library(data.table)
library(raster)
library(tidyverse)
library(sf)
library(rgdal)

## path here as I am not in a project putting this together 
## please change to serve your needs
dir_path = "P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/BMP_Placement_Tool (ECY1803)/Model_out/21.06.10/210615"
# Load spatial data -----------------------------
# map the spatial data and the spatial data back together 
huc12 <- shapefile(paste0(dir_path, "/huc12.shp"))
huc12 <- huc12[,c("OBJECTID", "Id", "gridcode",    "Shape_Leng",    "Shape_Area")]

# I would load all your rasters 
ct_max_errosion <- raster(paste0(dir_path, "/CT.max.Errosion.tif"))
ct_max_Lat <- raster(paste0(dir_path, "/CT.max.Lat.tif"))
ct_max_Q <- raster(paste0(dir_path, "/CT.max.Q.tif"))
ct_max_Qofe <- raster(paste0(dir_path, "/CT.max.Qofe.tif"))
ct_max_sed_t_ha <- raster(paste0(dir_path, "/CT.max.sed_t_ha.tif"))
ctbuf_max_errosion <- raster(paste0(dir_path, "/CT_buf.max.Errosion.tif"))
ctbuf_max_Lat <- raster(paste0(dir_path, "/CT_buf.max.Lat.tif"))
ctbuf_max_Q <- raster(paste0(dir_path, "/CT_buf.max.Q.tif"))
ctbuf_max_Qofe <- raster(paste0(dir_path, "/CT_buf.max.Qofe.tif"))
ctbuf_max_sed_t_ha <- raster(paste0(dir_path, "/CT_buf.max.sed_t_ha.tif"))
mt_max_errosion <- raster(paste0(dir_path, "/MT.max.Errosion.tif"))
mt_max_Lat <- raster(paste0(dir_path, "/MT.max.Lat.tif"))
mt_max_Q <- raster(paste0(dir_path, "/MT.max.Q.tif"))
mt_max_Qofe <- raster(paste0(dir_path, "/MT.max.Qofe.tif"))
mt_max_sed_t_ha <- raster(paste0(dir_path, "/MT.max.sed_t_ha.tif"))
mtbuf_max_errosion <- raster(paste0(dir_path, "/MT_buf.max.Errosion.tif"))
mtbuf_max_Lat <- raster(paste0(dir_path, "/MT_buf.max.Lat.tif"))
mtbuf_max_Q <- raster(paste0(dir_path, "/MT_buf.max.Q.tif"))
mtbuf_max_Qofe <- raster(paste0(dir_path, "/MT_buf.max.Qofe.tif"))
mtbuf_max_sed_t_ha <- raster(paste0(dir_path, "/MT_buf.max.sed_t_ha.tif"))
nt_max_errosion <- raster(paste0(dir_path, "/NT.max.Errosion.tif"))
nt_max_Lat <- raster(paste0(dir_path, "/NT.max.Lat.tif"))
nt_max_Q <- raster(paste0(dir_path, "/NT.max.Q.tif"))
nt_max_Qofe <- raster(paste0(dir_path, "/NT.max.Qofe.tif"))
nt_max_sed_t_ha <- raster(paste0(dir_path, "/NT.max.sed_t_ha.tif"))
ntbuf_max_errosion <- raster(paste0(dir_path, "/NT_buf.max.Errosion.tif"))
ntbuf_max_Lat <- raster(paste0(dir_path, "/NT_buf.max.Lat.tif"))
ntbuf_max_Q <- raster(paste0(dir_path, "/NT_buf.max.Q.tif"))
ntbuf_max_Qofe <- raster(paste0(dir_path, "/NT_buf.max.Qofe.tif"))
ntbuf_max_sed_t_ha <- raster(paste0(dir_path, "/NT_buf.max.sed_t_ha.tif"))

# And list them together, then we will "purrr::map" to loop processes over each raster
# Please change names when you expand the list for saving out named shapefiles
raster_list <- list("CT_max"= ct_max_errosion, 
                    "CT_l" = ct_max_Lat, 
                    "CT_Q" = ct_max_Q, 
                    "CT_Qofe" = ct_max_Qofe, 
                    "CT_sed" = ct_max_sed_t_ha,
                    "CTbuf_max"= ctbuf_max_errosion, 
                    "CTbuf_l" = ctbuf_max_Lat, 
                    "CTbuf_Q" = ctbuf_max_Q, 
                    "CTbuf_Qofe" = ctbuf_max_Qofe, 
                    "CTbuf_sed" = ctbuf_max_sed_t_ha)
                    #"MT_max" = mt_max_errosion,
                    #"MT_l" = mt_max_Lat, 
                    #"MT_Q" = mt_max_Q, 
                    #"MT_Qofe" = mt_max_Qofe, 
                    #"MT_sed" = mt_max_sed_t_ha,
                    #"MTbuf_max"= mtbuf_max_errosion, 
                    #"MTbuf_l" = mtbuf_max_Lat, 
                    #"MTbuf_Q" = mtbuf_max_Q, 
                    #"MTbuf_Qofe" = mtbuf_max_Qofe, 
                    #"MTbuf_sed" = mtbuf_max_sed_t_ha,
                    #"NT_max" = nt_max_errosion,
                    #"NT_l" = nt_max_Lat, 
                    #"NT_Q" = nt_max_Q, 
                    #"NT_Qofe" = nt_max_Qofe, 
                    #"NT_sed" = nt_max_sed_t_ha,
                    #"NTbuf_max" = ntbuf_max_errosion,
                    #"NTbuf_l" = ntbuf_max_Lat, 
                    #"NTbuf_Q" = ntbuf_max_Q, 
                    #"NTbuf_Qofe" = ntbuf_max_Qofe, 
                    #"NTbuf_sed" = ntbuf_max_sed_t_ha,)

# I did this analysis from a data.table rather than with raster calculations, so 
# I have sent the HUC12 raster also.  
huc12_r <- raster(paste0(dir_path, "/huc12_r.tif"))

# I was working in data tables so this script is all for xyz data tables rather
# than rasters, we can quickly translate raster to data tables. Map iterates a function 
# over each element in a list, if you are not familiar. Lists are nice for objects. 

DT_xyz_list <- raster_list %>% map(., function(H) as.data.frame(H, xy =T) %>% as.data.table())
huc12_xyz <- huc12_r %>% as.data.frame(., xy =T) %>% as.data.table()

# Then merge the huc12 raster per erosion raster and rename for consistency.  
DT_xyz_list <- DT_xyz_list %>% map(. , function(H) {
  H <- merge(H, huc12_xyz, by=c("x","y"))
  names(H) <- c("x", "y", "Errosion_kg_m2", "huc12")
  return(H)
  
})

# Functions ---------------------
# this function counts cells in each huc12 above a threshold and converts the count to area 
# note the conversion 1kg/m2 = 4.46 ton/acre 
by_huc12_cells <- function(x){
  area <- x[, .(area_m2 = .N*900), by = huc12]
  area_CSA_erosion_5t <- x[, .(csa_5t_m2 = sum(Errosion_kg_m2 > (5/4.46), na.rm=T)*900), by = huc12]
  area_CSA_erosion_4t <- x[, .(csa_4t_m2 = sum(Errosion_kg_m2 > (4/4.46), na.rm=T)*900), by = huc12]
  area_CSA_erosion_3t <- x[, .(csa_3t_m2 = sum(Errosion_kg_m2 > (3/4.46), na.rm=T)*900), by = huc12]
  area_CSA_erosion_2t <- x[, .(csa_2t_m2 = sum(Errosion_kg_m2 > (2/4.46), na.rm=T)*900), by = huc12]
  temp <- merge(area, area_CSA_erosion_5t) %>%  merge(area_CSA_erosion_4t)%>%
    merge(area_CSA_erosion_3t) %>% merge(area_CSA_erosion_2t)
  temp$huc12 <- as.character(temp$huc12)
  return(temp)
}


# this function merges data with the huc12 shapefile and save out a shape file. 
#Please change the destination(dsn) path to one of your choice
huc_it <- function(x,y, huc12=huc12){
  temp_out <- merge(huc12, x, by.x = "Id", by.y = "huc12")
  tempname <- (paste0("huc12_",y))
  writeOGR(obj=temp_out, dsn=paste0(dir_path, "/20220316_huc12"), layer=tempname, driver="ESRI Shapefile")
}

##### 
# find the threshold counts for each raster/treatment. 
huc12_data_cells <- DT_xyz_list %>% map(by_huc12_cells)

# save a shape file of each. You could merge all treatment to one shape file 
# one way to do that would be to add a column with treatment name values then dcast 
# dcast/pivot_wider to a wide format to add treatment names as a prefix then use the huc_it function. 
test <- map2(.x = huc12_data_cells, .y = names(huc12_data_cells), .f = ~huc_it(.x, .y, huc12))
