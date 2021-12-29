###############Clean database#######

library(dplyr)
library(MASS)
library(reshape2)
library(reshape)
library(tidyr)
library(stringr)
library(tidyverse)

setwd("P:/Research_and_Monitoring/Cailin_OMalley/Historical Database/Reports")
db <- read.csv("Smartsheet_HistoricalDB21.csv", header=T,na.strings=c("","NA"))
colnames(db)
##find rows with missing information
missing <- filter_at(db, vars(HUC.12.Name,
                              Practice.Type.1.NRCS.Code.and.Description, 
                              Practice.Type.1.Measurement, 
                              Practice.Type.1.Unit.of.Measurement), any_vars(is.na(.)))
write.csv(missing, "P:/Research_and_Monitoring/Cailin_OMalley/Historical Database/Reports/missing.csv")
##remove missing from database
db2 <- anti_join(db, missing, by = NULL, copy = FALSE)


## Move practice type columns to new data frame. Use melt independently for these columns with a unique identifier
db2$UNIQUE_ID = paste(db2$Cooperator.Name.1, db2$Project.Completion.Date, sep = " ")

NRCS_PRACS = db2[, c(33:72, 81)]

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
FINAL = FINAL[,-c(1, 34:73)]
FINAL = FINAL[,c(1:32, 40:44, 33:39)]

## Find unique values in funding source, year, etc
fs<-unique(FINAL$Funding.Source)
print(fs)
practices<-unique(FINAL$`NRCS Code and Description`)
write.csv(practices, "P:/Research_and_Monitoring/Cailin_OMalley/Historical Database/Reports/NRCS Practices.csv")

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

###change columns to numeric
FINAL$`Costshare Amount`<- as.numeric(as.character(FINAL$`Costshare Amount`))
FINAL$Project.Year <- as.numeric(as.character(FINAL$Project.Year))
FINAL$Measurement <- as.numeric(as.character(FINAL$Measurement))

## Final cleaning to remove rows with NAs for code descriptions

FINAL = FINAL[!is.na(FINAL$`NRCS Code and Description`),] # Projects without a code are deleted here
write.csv(FINAL, "P:/Research_and_Monitoring/Cailin_OMalley/Historical Database/Reports/FINAL_DB.csv")

##################Create new column for management types##############
FINAL$Management = NA

for(i in 1:nrow(FINAL)){
  if(str_detect(FINAL$`NRCS Code and Description`[i], "345|329") == TRUE){
    FINAL$Management[i] <- "Residue"
  }else if(str_detect(FINAL$`NRCS Code and Description`[i], "595") == TRUE){
    FINAL$Management[i] <- "Pest"
  }else if(str_detect(FINAL$`NRCS Code and Description`[i], "590") == TRUE){
    FINAL$Management[i] <- "Nutrient"
  }else if(str_detect(FINAL$`NRCS Code and Description`[i], "528|550|614|642") == TRUE){
    FINAL$Management[i] <- "Range"
  }else if(str_detect(FINAL$`NRCS Code and Description`[i], "328|340|484") == TRUE){
      FINAL$Management[i] <- "Soil"
  }else if(str_detect(FINAL$`NRCS Code and Description`[i], "327|342|412|315|612|644|645|657|422|582|382|395|580") == TRUE){
    FINAL$Management[i] <- "Habitat" 
  }}

VSP_FINAL<- FINAL[!is.na(FINAL$Management),] %>%
  filter(Project.Year %in% (2011:2019))
write.csv(VSP_FINAL, "P:/Research_and_Monitoring/Cailin_OMalley/Historical Database/Reports/VSP_FINAL.csv")

VSP_2011 <- VSP_FINAL[str_detect(VSP_FINAL$Project.Year, "2011") == TRUE,]
write.csv(VSP_2011, "P:/Research_and_Monitoring/Cailin_OMalley/Historical Database/Reports/VSP_2011.csv")

###WRIA
WRIA35 <- VSP_FINAL[str_detect(VSP_FINAL$WRIA, "35") == TRUE,]
WRIA34 <- VSP_FINAL[str_detect(VSP_FINAL$WRIA, "34") == TRUE,]

WRIA35_practices <- WRIA35 %>%
  group_by(Management, `NRCS Code and Description`, Unit) %>%
  summarise(practice_measurement = sum(Measurement, na.rm = TRUE)) %>%
  na.omit(WRIA35_practices)
print(WRIA35_practices)
write.csv(WRIA35_practices, "P:/Research_and_Monitoring/Cailin_OMalley/Historical Database/Reports/WRIA35_practices.csv")

WRIA34_practices <- WRIA34 %>%
  group_by(Management, `NRCS Code and Description`, Unit) %>%
  summarise(practice_measurement = sum(Measurement, na.rm = TRUE)) %>%
  na.omit(WRIA34_practices)
print(WRIA34_practices)
write.csv(WRIA34_practices, "P:/Research_and_Monitoring/Cailin_OMalley/Historical Database/Reports/WRIA34_practices.csv")


Management_practices <- VSP_FINAL %>%      ###summarize by management type and practices
  group_by(Management, `NRCS Code and Description`, Unit) %>%
  summarise(practice_measurement = sum(Measurement, na.rm = TRUE)) %>%
  na.omit(Management_practices)
print(Management_practices)
write.csv(Management_practices, "P:/Research_and_Monitoring/Cailin_OMalley/Historical Database/Reports/VSPmanagement_practices.csv")


Management_years <- VSP_FINAL %>% ###summarise by year
  group_by(Project.Year, Management, `NRCS Code and Description`) %>%
  summarise(practice_measurement = sum(Measurement, na.rm = TRUE)) %>%
  na.omit(Management_years)


Management_projects <- VSP_FINAL %>%  ###total count of projects
  group_by(WRIA, Management, `NRCS Code and Description`) %>%
  summarise(project_count = n()) %>%
  na.omit(Management_projects)

WRIA <- VSP_FINAL %>%
  group_by(WRIA, `NRCS Code and Description`, Unit) %>%
  summarise(practice_measurement = sum(Measurement, na.rm = TRUE)) %>%
  na.omit(WRIA)
WRIA$Practice_WRIA = NA 

  
for(i in 1:nrow(WRIA)){
  if(str_detect(WRIA[i,1], "34") == TRUE){
    WRIA[i,5] = paste(WRIA[i,2], "(34)", sep = ' ')
  }
  else if(str_detect(WRIA[i,1], "35") == TRUE){
    WRIA[i,5] = paste(WRIA[i,2], "(35)", sep = ' ')
  }
}
  
  
WRIA_sum <- merge(WRIA, Management_projects, by = c("NRCS Code and Description"))
WRIA_subset <- subset(WRIA_sum, WRIA.x == WRIA.y)
WRIA_sum2 = (WRIA_subset[, c(5, 8, 4, 3)])
write.csv(WRIA_sum2, "P:/Research_and_Monitoring/Cailin_OMalley/Historical Database/Reports/WRIA_sum.csv")

Year_projects <- VSP_FINAL %>%  ###total count of projects by year
  group_by(Project.Year) %>%
  summarise(project_count = n()) %>%
  na.omit(Year_projects)

###summarize by management type and practices - need to show 2011-2019
practices_projects <- merge(Management_practices, Management_projects)
write.csv(practices_projects, "P:/Research_and_Monitoring/Cailin_OMalley/Historical Database/Reports/VSP_sum.csv")

###summarize by year - doesn't show all the years?
practices_year <- merge(Management_practices, Management_years)

###### Create subsets based on practice type
library(stringr)
residue <- VSP_FINAL[str_detect(VSP_FINAL$Management, "Residue") == TRUE,]
pest <- VSP_FINAL[str_detect(VSP_FINAL$Management, "Pest") == TRUE,]
nutrient <- VSP_FINAL[str_detect(VSP_FINAL$Management, "Nutrient") == TRUE,]
range <- VSP_FINAL[str_detect(VSP_FINAL$Management, "Range") == TRUE,]
soil <- VSP_FINAL[str_detect(VSP_FINAL$Management, "Soil") == TRUE,]
habitat <- VSP_FINAL[str_detect(VSP_FINAL$Management, "Habitat") == TRUE,]


#### summarize #####
total_measurements <- VSP_FINAL %>%
  group_by(`NRCS Code and Description`) %>%
  summarise(totalmeasure = sum(Measurement, na.rm = TRUE)) %>%
  na.omit(total_measurements)
print(total_measurements)

### 2018-2019 datasets
VSP_2018 <- VSP_FINAL[str_detect(VSP_FINAL$Project.Year, "2018") == TRUE,]
VSP_2019 <- VSP_FINAL[str_detect(VSP_FINAL$Project.Year, "2019") == TRUE,]

#################################CREATE GRAPHS/REPORTS#####################
library(ggplot2)
library(ggmap)
library(ggrepel)
library(RColorBrewer)
library(pals)
library(ggforce)
library(scales)
library(wesanderson)

setwd("P:/Research_and_Monitoring/Cailin_OMalley/Historical Database/Reports/Graphs")

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

################# Separate graphs by management type ###############
## residue
total_res<- residue %>%
  group_by(`NRCS Code and Description`) %>%
  summarize(totaldol = sum(`Costshare Amount`, na.rm = TRUE)) %>%
  na.omit(total_res)
total_res$totaldol = round(total_res$totaldol, 0)

Res_dollars <- ggplot(total_res, aes(x = reorder(`NRCS Code and Description`, -totaldol), y = totaldol))+
  geom_bar(stat = "identity", fill = "#C7B19C")+
  scale_y_continuous("Costshare Amount",
                     labels = scales::dollar_format(scale = 0.001, suffix = "K"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
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
       title = "Dollars Spent on Residue and Tillage Management from 1998-Present")
print(Res_dollars)
ggsave('Dollars Spent on Residue and Tillage Management.png', plot = Res_dollars, scale = 1, width = 18, height = 10, units = "in", dpi = "print")

## pest management
total_pest<- pest %>%
  group_by(`NRCS Code and Description`) %>%
  summarize(totaldol = sum(`Costshare Amount`, na.rm = TRUE)) %>%
  na.omit(total_pest)
total_pest$totaldol = round(total_pest$totaldol, 0)

Pest_dollars <- ggplot(total_pest, aes(x = reorder(`NRCS Code and Description`, -totaldol), y = totaldol))+
  geom_bar(stat = "identity", fill = "#C7B19C")+
  scale_y_continuous("Costshare Amount",
                     labels = scales::dollar_format(scale = 0.001, suffix = "K"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
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
       title = "Dollars Spent on Pest Management from 1998-Present")
print(Pest_dollars)
ggsave('Dollars Spent on Pest Management.png', plot = Pest_dollars, scale = 1, width = 18, height = 10, units = "in", dpi = "print")

## nutrient
total_nutr<- nutrient %>%
  group_by(`NRCS Code and Description`) %>%
  summarize(totaldol = sum(`Costshare Amount`, na.rm = TRUE)) %>%
  na.omit(total_nutr)
total_nutr$totaldol = round(total_res$totaldol, 0)

Nutr_dollars <- ggplot(total_nutr, aes(x = reorder(`NRCS Code and Description`, -totaldol), y = totaldol))+
  geom_bar(stat = "identity", fill = "#C7B19C")+
  scale_y_continuous("Costshare Amount",
                     labels = scales::dollar_format(scale = 0.001, suffix = "K"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
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
       title = "Dollars Spent on Nutrient Management from 1998-Present")
print(Nutr_dollars)
ggsave('Dollars Spent on Nutrient Management.png', plot = Nutr_dollars, scale = 1, width = 18, height = 10, units = "in", dpi = "print")

## range
total_range<- range %>%
  group_by(`NRCS Code and Description`) %>%
  summarize(totaldol = sum(`Costshare Amount`, na.rm = TRUE)) %>%
  na.omit(total_range)
total_range$totaldol = round(total_res$totaldol, 0)

Range_dollars <- ggplot(total_range, aes(x = reorder(`NRCS Code and Description`, -totaldol), y = totaldol))+
  geom_bar(stat = "identity", fill = "#C7B19C")+
  scale_y_continuous("Costshare Amount",
                     labels = scales::dollar_format(scale = 0.001, suffix = "K"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
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
       title = "Dollars Spent on Range Management from 1998-Present")
print(Range_dollars)
ggsave('Dollars Spent on Range Management.png', plot = Range_dollars, scale = 1, width = 18, height = 10, units = "in", dpi = "print")

## soil
total_soil<- soil %>%
  group_by(`NRCS Code and Description`) %>%
  summarize(totaldol = sum(`Costshare Amount`, na.rm = TRUE)) %>%
  na.omit(total_soil)
total_soil$totaldol = round(total_soil$totaldol, 0)

Soil_dollars <- ggplot(total_soil, aes(x = reorder(`NRCS Code and Description`, -totaldol), y = totaldol))+
  geom_bar(stat = "identity", fill = "#C7B19C")+
  scale_y_continuous("Costshare Amount",
                     labels = scales::dollar_format(scale = 0.001, suffix = "K"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
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
       title = "Dollars Spent on Soil Management from 1998-Present")
print(Soil_dollars)
ggsave('Dollars Spent on Soil Management.png', plot = Soil_dollars, scale = 1, width = 18, height = 10, units = "in", dpi = "print")

## habitat
total_hab<- habitat %>%
  group_by(`Project.Year`) %>%
  summarize(totalacres = sum(`Measurement`, na.rm = TRUE)) %>%
  na.omit(total_hab)
total_hab$totalacres = round(total_res$totalacres, 0)

Habitat_acres <- ggplot(total_hab, aes(x = Project.Year, y = totalacres))+
  geom_bar(stat = "identity", fill = "#C7B19C")+
  scale_y_continuous()+
  scale_x_continuous(limits = c(2014, 2020), breaks = 2015:2019) +
  geom_text(aes(label = format(totalacres, digits = 8, big.mark = ",")),
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
  labs(x = "Year",
       y = "Acres Completed",
       title = "Acres of Habitat Management from 2015-2019")
print(Habitat_acres)
ggsave('Acres of Habitat Management.png', plot = Habitat_acres, scale = 1, width = 18, height = 10, units = "in", dpi = "print")

###2018 acreage
vsp2018 <- ggplot(data = VSP_2018, aes(x = Management, y = Measurement)) +
  geom_bar(stat="identity")
print(vsp2018)

###2019 acreage
vsp2019 <- ggplot(data = VSP_2019, aes(x = Management, y = Measurement)) +
  geom_bar(stat="identity")
print(vsp2019)

###2011-2019 projects
vsp2011_2019 <- ggplot(data = VSP_FINAL, aes(x = Project.Year, y = Measurement, fill = Management)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_x_continuous(limits = c(2010, 2020), breaks = 2011:2019) +
  theme_new()+
  labs(fill = "Management",
       x = "Project Year",
       y = "Measurement",
       title = "Acreage of Management Types Implemented from 2011 to 2019")
print(vsp2011_2019)
