###############Clean database#######

library(dplyr)
library(MASS)
library(reshape2)
library(reshape)
library(tidyr)
library(stringr)
library(tidyverse)

setwd("P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/Historical Database/Reports")
db <- read.csv("PCDConservationProjectDatabase_20220804.csv", header=T,na.strings=c("","NA"))
colnames(db)
##find rows with missing information
missing <- filter_at(db, vars(HUC.12.Name,
                              Practice.Type.1.NRCS.Code.and.Description, 
                              Practice.Type.1.Measurement, 
                              Practice.Type.1.Unit.of.Measurement), any_vars(is.na(.)))
write.csv(missing, "P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/Historical Database/Reports/missing220804.csv")
##remove missing from database
db2 <- anti_join(db, missing, by = NULL, copy = FALSE)



## Move practice type columns to new data frame. Use melt independently for these columns with a unique identifier
db2$UNIQUE_ID = paste(db2$Cooperator.Name, db2$Completion.Date, sep = " ")

NRCS_PRACS = db2[, c(27:66, 115)]

###Use gather instead of melt
NRCS_MELT = gather(NRCS_PRACS, variable, value, Practice.Type.1.NRCS.Code.and.Description:Practice.Type.10.Costshare.Amount....)


## For loop to iterate non-unique, duplicated cooperator names to become unique

NRCS_MELT$NEW_UNIQUE_ID = NA

for(i in 1:nrow(NRCS_MELT)){
  if(str_detect(NRCS_MELT[i,2], "Practice.Type.10.") == TRUE){
    NRCS_MELT[i,4] = paste(NRCS_MELT[i,1], "10", sep = ' ')
  }
  else if(str_detect(NRCS_MELT[i,2], "Practice.Type.1.") == TRUE){
    NRCS_MELT[i,4] = paste(NRCS_MELT[i,1], "1", sep = ' ')
  }
  else if(str_detect(NRCS_MELT[i,2], "Practice.Type.2.") == TRUE){
    NRCS_MELT[i,4] = paste(NRCS_MELT[i,1], "2", sep = ' ')
  }
  else if(str_detect(NRCS_MELT[i,2], "Practice.Type.3.") == TRUE){
    NRCS_MELT[i,4] = paste(NRCS_MELT[i,1], "3", sep = ' ')
  }
  else if(str_detect(NRCS_MELT[i,2], "Practice.Type.4.") == TRUE){
    NRCS_MELT[i,4] = paste(NRCS_MELT[i,1], "4", sep = ' ')
  }
  else if(str_detect(NRCS_MELT[i,2], "Practice.Type.5.") == TRUE){
    NRCS_MELT[i,4] = paste(NRCS_MELT[i,1], "5", sep = ' ')
  }
  else if(str_detect(NRCS_MELT[i,2], "Practice.Type.6.") == TRUE){
    NRCS_MELT[i,4] = paste(NRCS_MELT[i,1], "6", sep = ' ')
  }
  else if(str_detect(NRCS_MELT[i,2], "Practice.Type.7.") == TRUE){
    NRCS_MELT[i,4] = paste(NRCS_MELT[i,1], "7", sep = ' ')
  }
  else if(str_detect(NRCS_MELT[i,2], "Practice.Type.8.") == TRUE){
    NRCS_MELT[i,4] = paste(NRCS_MELT[i,1], "8", sep = ' ')
  }
  else if(str_detect(NRCS_MELT[i,2], "Practice.Type.9.") == TRUE){
    NRCS_MELT[i,4] = paste(NRCS_MELT[i,1], "9", sep = ' ')
  }
  
  
}

## Removing extra characters on variable names to name only four different types of variables (Code, Measurement, Unit, Costshare)

NRCS_MELT$variable = str_remove(NRCS_MELT$variable, "Practice.Type.1.")
NRCS_MELT$variable = str_remove(NRCS_MELT$variable, "Practice.Type.2.")
NRCS_MELT$variable = str_remove(NRCS_MELT$variable, "Practice.Type.3.")
NRCS_MELT$variable = str_remove(NRCS_MELT$variable, "Practice.Type.4.")
NRCS_MELT$variable = str_remove(NRCS_MELT$variable, "Practice.Type.5.")
NRCS_MELT$variable = str_remove(NRCS_MELT$variable, "Practice.Type.6.")
NRCS_MELT$variable = str_remove(NRCS_MELT$variable, "Practice.Type.7.")
NRCS_MELT$variable = str_remove(NRCS_MELT$variable, "Practice.Type.8.")
NRCS_MELT$variable = str_remove(NRCS_MELT$variable, "Practice.Type.9.")
NRCS_MELT$variable = str_remove(NRCS_MELT$variable, "Practice.Type.10.")

## Reshaping dataframe by "new" UNIQUE_ID

NRCS_SHAPED = reshape(NRCS_MELT, idvar = "NEW_UNIQUE_ID",
                      v.names = "value",
                      timevar = "variable",
                      direction = "wide")

## Cleaning reshaped dataframe to be merged back with original db2 dataframe

NRCS_SHAPED = NRCS_SHAPED[,c(2:6)]
NRCS_SHAPED$NEW_UNIQUE_ID = substr(NRCS_SHAPED$NEW_UNIQUE_ID,1,nchar(NRCS_SHAPED$NEW_UNIQUE_ID)-2)
colnames(NRCS_SHAPED) = c("UNIQUE_ID", "NRCS Code and Description", "Measurement", "Unit", "Costshare Amount")

## Change Costshare Amount to numeric without dollar sign
NRCS_SHAPED$`Costshare Amount`<-as.numeric(gsub('[$,]', '', NRCS_SHAPED$`Costshare Amount`))


## Merging & cleaning final dataframe

FINAL = merge(db2, NRCS_SHAPED, by = "UNIQUE_ID", all.x = TRUE, all.y = TRUE)
FINAL = FINAL[,-c(28:70, 74, 76:115)]
FINAL = FINAL[,c(1:27, 32:35, 28:31)]

## Find unique values in funding source, year, etc
fs<-unique(FINAL$Funding.Source)
print(fs)
practices<-unique(FINAL$`NRCS Code and Description`)
write.csv(practices, "P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/Historical Database/Reports/NRCS_Practices220804.csv")

######### Multiple funding sources, column cleaning
FINAL$Archive = NA
FINAL$Funding.Source = as.character(FINAL$Funding.Source)
for(i in 1:nrow(FINAL)){
  qual = as.numeric(str_count(FINAL$Funding.Source[i], ))
  if(isTRUE(as.numeric(qual) > 44)){
    FINAL$Archive[i] = FINAL$Funding.Source[i]
    FINAL$Funding.Source[i] = "Multiple Funding Sources"
    
  }
  else{}
}

### Make new column with NRCS Practices w/o code #
FINAL$NRCS_Description = NA
FINAL$NRCS_Description <- str_remove_all(FINAL$`NRCS Code and Description`, "[0123456789]")

###change columns to numeric
FINAL$`Costshare Amount`<- as.numeric(as.character(FINAL$`Costshare Amount`))
FINAL$Project.Year <- as.numeric(as.character(FINAL$Project.Year))
FINAL$Measurement <- as.numeric(as.character(FINAL$Measurement))

## Final cleaning to remove rows with NAs for code descriptions

FINAL = FINAL[!is.na(FINAL$`NRCS Code and Description`),] # Projects without a code are deleted here

##########################################################################
#Show duplicate projects - incentives
dup <- FINAL[duplicated(FINAL[, c(2, 28)]),]
dup2 <- FINAL[duplicated(FINAL[, c(2, 28)],fromLast=TRUE),]
allDup<- full_join(dup, dup2, by = "UNIQUE_ID")
allDup = allDup[,c(2:10,19:25,27:35,38:46,55:61,63:71)]
write.csv(allDup, "P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/Historical Database/Reports/duplicates20220804.csv")

FINAL <- FINAL[,c(2:37)]

write.csv(FINAL, "P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/Historical Database/Reports/FINAL_DB_220804.csv")
write.csv(FINAL, "P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/Historical Database/Reports/Database_app/piechart/FINAL_DB220804.csv")
write.csv(FINAL, "P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/Historical Database/Reports/Database_app/RipRest_graph/FINAL_DB220804.csv")
write.csv(FINAL, "P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/Historical Database/Reports/Database_app/TotalFunding/FINAL_DB220804.csv")
write.csv(FINAL, "P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/Historical Database/Reports/Database_app/FundingSources/FINAL_DB220804.csv")
write.csv(FINAL, "P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/Historical Database/Reports/Database_app/TotalCostshare/FINAL_DB220804.csv")
write.csv(FINAL, "P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/Historical Database/Reports/Database_app/HUC12_Measurements/FINAL_DB220804.csv")


########### 
#Find rows recorded in degrees minutes seconds
#Only use if needed.
#degrees <- FINAL[str_detect(FINAL$Latitude_1, "'") == TRUE,] %>%
#  drop_na(Latitude_1)
#  data.frame(lapply(degrees, as.character), stringsAsFactors=FALSE)


## Make .csv without NA lats/longs
FINAL_points <- FINAL[!is.na(FINAL$Latitude_1),] 
write.csv(FINAL_points, "P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/Historical Database/Reports/FINAL_points220804.csv")


###### Create subsets based on practice type
library(stringr)
riparian <- FINAL[str_detect(FINAL$Project.Category, "Riparian") == TRUE,]
conservation_ag <- FINAL[str_detect(FINAL$Project.Category, "Conservation Agriculture") == TRUE,]
habitat <- FINAL[str_detect(FINAL$Project.Category, "Habitat") == TRUE,]
livestock <- FINAL[str_detect(FINAL$Project.Category, "Livestock") == TRUE,]



#################################CREATE GRAPHS/REPORTS#####################
library(ggplot2)
library(ggmap)
library(ggrepel)
library(RColorBrewer)
library(pals)
library(ggforce)
library(scales)
library(wesanderson)

setwd("P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/Historical Database/Reports/Graphs")

##by project category
category_sum <- FINAL %>%
  group_by(Project.Category) %>%
  summarise(n = n()) %>%
  na.omit(category_sum)

category_sum <- category_sum %>%
  mutate(end = 2 * pi * cumsum(n)/sum(n),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
category_sum$Project.Category = as.character(category_sum$Project.Category)
library(ggforce) # for 'geom_arc_bar'
category_pie <- ggplot(category_sum) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                   start = start, end = end, fill = Project.Category)) +
  geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = paste(round(n/sum(n) * 100, 1), "%"),
                hjust = hjust, vjust = vjust)) +
  coord_fixed() +
  scale_x_continuous(limits = c(-1.5, 1.4),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-2, 2),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  labs(fill= "Project Type",
       x = NULL,
       y = NULL,
       title = "Percent of Projects Completed Between 1998-2022")+
  scale_fill_manual(values = wes_palette("Chevalier1", n=4)) +
  theme(panel.background = element_blank(), 
        plot.title = element_text(hjust = -0.5, vjust = -1))

print(category_pie)
ggsave('Project Type Pie Chart 220804.png', plot = category_pie, scale = 1, width = 5.5, height = 5.5, units = "in", dpi = "print")

##sum totals
total_funding <- FINAL %>%
  group_by(Project.Category) %>%
  summarise(totaldol = sum(`Costshare Amount`, na.rm = TRUE)) %>%
  na.omit(total_funding)
print(total_funding)



###theme for graphs
theme_new <- function() {
  theme(axis.text.x = 
          element_text(size = 15,
                       angle = 45,
                       hjust = 1,
                       vjust = 1),
        axis.text.y = element_text(size = 15),
        panel.grid.major.y = element_line(color = "gray"),
        panel.background = element_rect(fill="transparent", color = NA),
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.background = element_rect(color=NA))
}

## riparian
total_r<- riparian %>%
  group_by(`NRCS Code and Description`) %>%
  summarize(totaldol = sum(`Costshare Amount`, na.rm = TRUE)) %>%
  na.omit(total_r)
total_r$totaldol = round(total_r$totaldol, 0)

R_dollars <- ggplot(total_r[which(total_r$totaldol>0),], aes(x = reorder(`NRCS Code and Description`, -totaldol), y = totaldol))+
  geom_bar(stat = "identity", fill = "#C7B19C")+
  scale_y_continuous("Costshare Amount",
                     labels = scales::dollar_format(scale = 0.001, suffix = "K"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  geom_text(aes(label = format(totaldol, digits = 8, big.mark = ",")),
            hjust=0.5,
            vjust=-0.75,
            size=5) +
  theme(axis.text.x = 
          element_text(size = 15,
                       angle = 45,
                       hjust = 1,
                       vjust = 1),
        axis.text.y = element_text(size = 15),
        panel.grid.major.y = element_line(color = "gray"),
        panel.background = element_rect(fill="transparent", color = NA),
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 20))+
  labs(x = "NRCS Practice Type",
       y = "Costshare Amount",
       title = "Dollars Spent by Riparian Practice Types from 1998-2022")
print(R_dollars)
ggsave('Dollars Spent on Riparian Practices 20220804.png', plot = R_dollars, scale = 1, width = 18, height = 10, units = "in", dpi = "print")

## conservation agriculture
total_ca<- conservation_ag %>%
  group_by(`NRCS Code and Description`) %>%
  summarize(totaldol = sum(`Costshare Amount`, na.rm = TRUE)) %>%
  na.omit(total_ca)
total_ca$totaldol = round(total_ca$totaldol, 0)

CA_dollars <- ggplot(total_ca[which(total_ca$totaldol>0),], aes(x = reorder(`NRCS Code and Description`, -totaldol), y = totaldol))+
  geom_bar(stat = "identity", fill = "#446455")+
  scale_y_continuous("Costshare Amount",
                     labels = scales::dollar_format(scale = 0.001, suffix = "K"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  geom_text(aes(label = format(totaldol, digits = 8, big.mark = ",")) ,                
            hjust=0.5,
            vjust=-0.75,
            size=5) +
  theme(axis.text.x = 
          element_text(size = 15,
                       angle = 45,
                       hjust = 1,
                       vjust = 1),
        axis.text.y = element_text(size = 15),
        panel.grid.major.y = element_line(color = "gray"),
        panel.background = element_rect(fill="transparent", color = NA),
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 20))+
  labs(x = "NRCS Practice Type",
       y = "Costshare Amount",
       title = "Dollars Spent by Conservation Agriculture Practice Types from 1998-2022")
print(CA_dollars)
ggsave('Dollars Spent on Conservation Agriculture Practices 20220804.png', plot = CA_dollars, scale = 1, width = 20, height = 12, units = "in", dpi = "print")

##habitat restoration
total_h<- habitat %>%
  group_by(`NRCS Code and Description`) %>%
  summarize(totaldol = sum(`Costshare Amount`, na.rm = TRUE)) %>%
  na.omit(total_h)
total_h$totaldol = round(total_h$totaldol, 0)

H_dollars <- ggplot(total_h[which(total_h$totaldol>0),], aes(x = reorder(`NRCS Code and Description`, -totaldol), y = totaldol))+
  geom_bar(stat = "identity", fill = "#FDD262")+
  scale_y_continuous("Costshare Amount",
                     labels = scales::dollar_format(scale = 0.001, suffix = "K"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 24)) +
  geom_text(aes(label = format(totaldol, digits = 8, big.mark = ",")) ,                
            hjust=0.5,
            vjust=-0.75,
            size=5) +
  theme(axis.text.x = 
          element_text(size = 15,
                       angle = 45,
                       hjust = 1,
                       vjust = 1),
        axis.text.y = element_text(size = 15),
        panel.grid.major.y = element_line(color = "gray"),
        panel.background = element_rect(fill="transparent", color = NA),
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 20))+
  labs(x = "NRCS Practice Type",
       y = "Costshare Amount",
       title = "Dollars Spent by Habitat Restoration Practice Types from 1998-2022")
print(H_dollars)
ggsave('Dollars Spent on Habitat Restoration Practices 20220804.png', plot = H_dollars, scale = 1, width = 17, height = 10, units = "in", dpi = "print")

## livestock
total_l<- livestock %>%
  group_by(`NRCS Code and Description`) %>%
  summarize(totaldol = sum(`Costshare Amount`, na.rm = TRUE)) %>%
  na.omit(total_l)
total_l$totaldol = round(total_l$totaldol, 0)

L_dollars <- ggplot(total_l[which(total_l$totaldol>0),], aes(x = reorder(`NRCS Code and Description`, -totaldol), y = totaldol))+
  geom_bar(stat = "identity", fill = "#D3DDDC")+
  scale_y_continuous("Costshare Amount",
                     labels = scales::dollar_format(scale = 0.001, suffix = "K"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 26)) +
  geom_text(aes(label = format(totaldol, digits = 8, big.mark = ",")) ,                
            hjust=0.5,
            vjust=-0.75,
            size=5) +
  theme(axis.text.x = 
          element_text(size = 15,
                       angle = 55,
                       hjust = 1,
                       vjust = 1),
        axis.text.y = element_text(size = 15),
        panel.grid.major.y = element_line(color = "gray"),
        panel.background = element_rect(fill="transparent", color = NA),
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 20))+
  labs(x = "NRCS Practice Type",
       y = "Costshare Amount",
       title = "Dollars Spent by Livestock Practice Types from 1998-2022")
print(L_dollars)
ggsave('Dollars Spent on Livestock Practices 20220804.png', plot = L_dollars, scale = 1, width = 23, height = 10, units = "in", dpi = "print")

###### Funding by year by project types
FINAL$Project.Year <- as.numeric(as.character(FINAL$Project.Year))
pt_funding <- ggplot(data=subset(FINAL, !is.na(Project.Category)), aes(x = Project.Year, y = `Costshare Amount`))+
  geom_bar(stat = "identity", aes(fill = Project.Category), width = 0.5) +
  scale_fill_manual(values = wes_palette("Chevalier1", n=4)) +
  scale_y_continuous("Costshare Amount",
                     labels = scales::dollar_format(scale = 0.001, suffix = "K"))+ 
  scale_x_continuous(limits = c(1997, 2023), breaks = 1998:2022) +
  theme_new()+
  labs(fill = "Project Type",
       x = "Project Year",
       y = "Costshare Amount",
       title = "Funding by Project Type 1998-2021")
print(pt_funding)
ggsave('Funding Overtime by Project Type 20220804.png', plot = pt_funding, scale = 1, width = 15, height = 10, units = "in", dpi = "print")


###scatterplot of funding by year by project types
PROJECT_CS_SUM = FINAL %>%
  group_by(Project.Year, Project.Category) %>%
  summarize(Project_Sum = sum(`Costshare Amount`, na.rm = TRUE))
scatter_funding<- ggplot(PROJECT_CS_SUM, aes(x = Project.Year, y = Project_Sum, color = Project.Category)) +
  geom_point(size = 5) +
  geom_line()+
  scale_y_continuous("Costshare Amount",
                     labels = scales::dollar_format(scale = 0.001, suffix = "K"))+ 
  scale_x_continuous(limits = c(1997, 2023), breaks = 1998:2022) +
  theme_new()+
  scale_color_manual(values= wes_palette("Chevalier1", n=4)) +
  labs(color = "Project Type",
       x = "Project Year",
       y = "Costshare Amount",
       title = "Funding Overtime by Project Type (1998-2022)")

print(scatter_funding)
ggsave('Scatterplot Funding Overtime by Project Type 20220804.png', plot = scatter_funding, scale = 1, width = 15, height = 10, units = "in", dpi = "print")


##total funding without fill
total_f<- FINAL %>%
  group_by(Project.Year) %>%
  summarize(totaldol = sum(`Costshare Amount`, na.rm = TRUE))

total_f$totaldol = round(total_f$totaldol, 0)

total_funds <- ggplot(total_f, aes(x = Project.Year, y = totaldol))+
  geom_bar(stat = "identity", width = 0.5, fill = "#81A88D") +
  scale_y_continuous("Costshare Amount",
                     labels = scales::dollar_format(scale = 0.001, suffix = "K"))+
  scale_x_continuous(limits = c(1997, 2023), breaks = 1998:2022) +
  geom_text(aes(label = format(totaldol, digits = 8, big.mark = ",")) ,                
            hjust=0.5,
            vjust=-0.75,
            size=5) +
  theme_new()+
  labs(x = "Project Year",
       y = "Costshare Amount",
       title = "Total Funding 1998-2022")
print(total_funds)
ggsave('Funding Overtime 20220804.png', plot = total_funds, scale = 1, width = 18, height = 10, units = "in", dpi = "print")

acres = NA

### total acreage completed overtime - need to only select measurements in units of acres
acres <- subset(FINAL, FINAL$Unit=="Acres")
acres$Project.Year = as.numeric(acres$Project.Year)
pt_acreage<- ggplot(acres, aes(x = Project.Year, y = Measurement))+
  geom_bar(stat="identity", aes(fill = Project.Category), width = 0.5) +
  scale_fill_manual(values = wes_palette("Chevalier1", n=4)) +
  scale_x_continuous(limits = c(1997, 2023), breaks = 1998:2022) +
  theme_new()+
  labs(fill = "Project Type",
       x = "Project Year",
       y = "Acres",
       title = "Acreage of Projects 1998-2022")
print(pt_acreage)
ggsave('Acreage Overtime by Project Type 20220804.png', plot = pt_acreage, scale = 1, width = 15, height = 10, units = "in", dpi = "print")

##acreage sum totals
total_acres <- acres %>%
  group_by(Project.Category) %>%
  summarise(totalacres = sum(Measurement, na.rm = TRUE)) %>%
  na.omit(total_acres)
print(total_acres)


#########################################Summarize by HUC12############################
## to get rid of HUC12 name (only code) 
FINAL$HUC.12.Name <- as.character(gsub("[^0-9]","", FINAL$HUC.12.Name)) %>%
  substr(1, 12)
###run subsets again to get HUC12 codes only

#Practice measurement totals by HUC12
HUC12_measure <- FINAL %>%      ###summarize by HUC12 and practices
  group_by(HUC.12.Name, `NRCS Code and Description`, Unit) %>%
  summarise(practice_measurement = sum(Measurement, na.rm = TRUE)) %>%
  na.omit(HUC12_measure)
  HUC12_m <- HUC12_measure[, c(1, 2, 4, 3)]
print(HUC12_m)
write.csv(HUC12_m, "P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/Historical Database/Reports/HUC12/HUC12_measurements220804.csv")

##HUC12 Totals
HUC12_sum <- FINAL %>%    ###summarize only by HUC12
  group_by(HUC.12.Name) %>%
  summarise(HUC12_costshare = sum(`Costshare Amount`, na.rm = TRUE),
            HUC12_projects = n()) %>%
  na.omit(HUC12_sum)
print(HUC12_sum)


HUC12_practices <- FINAL %>%      ###summarize by HUC12 and practices
  group_by(HUC.12.Name, `NRCS Code and Description`) %>%
  summarise(practice_costshare = sum(`Costshare Amount`, na.rm = TRUE),
            practice_projects = n()) %>%
  na.omit(HUC12_practices)

print(HUC12_practices)

##merge both summarized tables
HUC12_data <- merge(HUC12_sum, HUC12_practices)

write.csv(HUC12_data, "P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/Historical Database/Reports/HUC12/HUC12_totals220804.csv")

## Riparian projects in HUC12
HUC12_r <- riparian %>%
  group_by(HUC.12.Name) %>%
  summarise(HUC12_costshare = sum(`Costshare Amount`, na.rm = TRUE),
            HUC12_projects = n()) %>%
  na.omit(HUC12_r)
print(HUC12_r)


HUC12_rpractices <- riparian %>%      ###summarize by HUC12 and practices
  group_by(HUC.12.Name, `NRCS Code and Description`) %>%
  summarise(practice_costshare = sum(`Costshare Amount`, na.rm = TRUE),
            practice_projects = n()) %>%
  na.omit(HUC12_rpractices)
print(HUC12_rpractices)

##merge both summarized tables
HUC12_riparian <- merge(HUC12_r, HUC12_practices)

write.csv(HUC12_riparian, "P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/Historical Database/Reports/HUC12/HUC12_riparian220804.csv")



## Conservation ag projects in HUC12
HUC12_c <- conservation_ag %>%
  group_by(HUC.12.Name) %>%
  summarise(HUC12_costshare = sum(`Costshare Amount`, na.rm = TRUE),
            HUC12_projects = n()) %>%
  na.omit(HUC12_c)
print(HUC12_c)


HUC12_cpractices <- conservation_ag %>%      ###summarize by HUC12 and practices
  group_by(HUC.12.Name, `NRCS Code and Description`) %>%
  summarise(practice_costshare = sum(`Costshare Amount`, na.rm = TRUE),
            practice_projects = n()) %>%
  na.omit(HUC12_cpractices)
print(HUC12_cpractices)

##merge both summarized tables
HUC12_conag <- merge(HUC12_c, HUC12_cpractices)

write.csv(HUC12_conag, "P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/Historical Database/Reports/HUC12/HUC12_conservationag220804.csv")

## Habitat projects in HUC12
HUC12_h <- habitat %>%
  group_by(HUC.12.Name) %>%
  summarise(HUC12_costshare = sum(`Costshare Amount`, na.rm = TRUE),
            HUC12_projects = n()) %>%
  na.omit(HUC12_h)
print(HUC12_h)


HUC12_hpractices <- habitat %>%      ###summarize by HUC12 and practices
  group_by(HUC.12.Name, `NRCS Code and Description`) %>%
  summarise(practice_costshare = sum(`Costshare Amount`, na.rm = TRUE),
            practice_projects = n()) %>%
  na.omit(HUC12_hpractices)
print(HUC12_hpractices)

##merge both summarized tables
HUC12_habitat <- merge(HUC12_h, HUC12_hpractices)

write.csv(HUC12_habitat, "P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/Historical Database/Reports/HUC12/HUC12_habitat220804.csv")


## Livestock projects in HUC12
HUC12_l <- livestock %>%
  group_by(HUC.12.Name) %>%
  summarise(HUC12_costshare = sum(`Costshare Amount`, na.rm = TRUE),
            HUC12_projects = n()) %>%
  na.omit(HUC12_l)
print(HUC12_l)


HUC12_lpractices <- livestock %>%      ###summarize by HUC12 and practices
  group_by(HUC.12.Name, `NRCS Code and Description`) %>%
  summarise(practice_costshare = sum(`Costshare Amount`, na.rm = TRUE),
            practice_projects = n()) %>%
  na.omit(HUC12_lpractices)
print(HUC12_lpractices)

##merge both summarized tables
HUC12_livestock <- merge(HUC12_l, HUC12_lpractices)

write.csv(HUC12_livestock, "P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/Historical Database/Reports/HUC12/HUC12_livestock220804.csv")


####################################################################################
##summarize by current year (2022)
FINAL$Completion.Date <- as.Date(FINAL$Completion.Date, "%m/%d/%y")
FY2022 <- FINAL[FINAL$Completion.Date >= "2021-07-01" & FINAL$Completion.Date <= "2022-06-30",]
sumFY22 <- subset(FY2022, !is.na(Employee.Name))
write.csv(sumFY22, "P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/Historical Database/Reports/summaryFY22_2.csv")

#summarize by acres
acres22 <- subset(sumFY22, sumFY22$Unit=="Acres")
totalacres2022 <- acres22 %>%
  group_by(`NRCS Code and Description`) %>%
  summarise(totalacres = sum(Measurement, na.rm = TRUE)) %>%
  na.omit(totalacres2022)
print(totalacres2022)
write.csv(totalacres2022, "P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/Historical Database/Reports/totalacres20220804.csv")



