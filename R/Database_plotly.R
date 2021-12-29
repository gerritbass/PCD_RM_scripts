library(dplyr)
library(MASS)
library(reshape2)
library(reshape)
library(tidyr)
library(stringr)
library(tidyverse)
library(plotly)
library(forcats)



setwd("P:/Research_and_Monitoring/Cailin_OMalley/Historical Database/Reports/Database_app")
FINAL <- read.csv("P:/Research_and_Monitoring/Cailin_OMalley/Historical Database/Reports/FINAL_DB.csv")


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
colors <- c('#446455', '#FDD262', '#D3DDDC', '#C7B19C')
cat_pie <- plot_ly(category_sum, labels = ~Project.Category, values = ~n, type = 'pie',
                   textposition = 'inside',
                   textinfo = 'percent',
                   insidetextfont = list(size = 26),
                   hoverinfo = 'text',
                   text = ~paste('Total Projects Completed:', n),
                   marker = list(colors = colors))%>%
  layout(title = 'Percent of Projects Completed Between 1998-2019',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
cat_pie


##sum totals
total_funding <- FINAL %>%
  group_by(Project.Category) %>%
  summarise(totaldol = sum(`Costshare.Amount`, na.rm = TRUE)) %>%
  na.omit(total_funding)
print(total_funding)

## riparian
total_r <- riparian %>%
  group_by(`NRCS.Code.and.Description`) %>%
  summarize(totaldol = sum(`Costshare.Amount`, na.rm = TRUE)) %>%
  na.omit(total_r)
total_r$totaldol = round(total_r$totaldol, 0)

d <- prettyNum(total_r$totaldol,big.mark = ",", scientific=FALSE)
total_r$d <- paste("$", sep="", d)
Rip_graph <- total_r %>%
  mutate(NRCS.Code.and.Description = fct_reorder(NRCS.Code.and.Description, totaldol, .desc = TRUE)) %>%
  plot_ly(x = ~NRCS.Code.and.Description, y = ~totaldol, 
          hoverinfo = "text",
          text = ~paste("Total Costshare Amount:", d)) %>%
  add_bars(color = I("#C7B19C")) %>%
  layout(xaxis = list(title = "NRCS Practice Type", showgrid = FALSE),
         yaxis = list(title = "Costshare Amount"),
         title = "Dollars Spent by Riparian Practice Types from 1998-Present")
Rip_graph

## conservation agriculture
total_ca<- conservation_ag %>%
  group_by(`NRCS.Code.and.Description`) %>%
  summarize(totaldol = sum(`Costshare.Amount`, na.rm = TRUE)) %>%
  na.omit(total_ca)
total_ca$totaldol = round(total_ca$totaldol, 0)

d <- prettyNum(total_ca$totaldol,big.mark = ",", scientific=FALSE)
total_ca$d <- paste("$", sep="", d)
CA_graph <- total_ca %>%
  mutate(NRCS.Code.and.Description = fct_reorder(NRCS.Code.and.Description, totaldol, .desc = TRUE)) %>%
  plot_ly(x = ~NRCS.Code.and.Description, y = ~totaldol, 
          hoverinfo = "text",
          text = ~paste("Total Costshare Amount:", d)) %>%
  add_bars(color = I("#446455")) %>%
  layout(xaxis = list(title = "NRCS Practice Type", showgrid = FALSE),
         yaxis = list(title = "Costshare Amount"),
         title = "Dollars Spent by Conservation Agriculture Practice Types from 1998-Present")
CA_graph

##habitat restoration
total_h<- habitat %>%
  group_by(`NRCS.Code.and.Description`) %>%
  summarize(totaldol = sum(`Costshare.Amount`, na.rm = TRUE)) %>%
  na.omit(total_h)
total_h$totaldol = round(total_h$totaldol, 0)

d <- prettyNum(total_h$totaldol,big.mark = ",", scientific=FALSE)
total_h$d <- paste("$", sep="", d)
H_graph <- total_h %>%
  mutate(NRCS.Code.and.Description = fct_reorder(NRCS.Code.and.Description, totaldol, .desc = TRUE)) %>%
  plot_ly(x = ~NRCS.Code.and.Description, y = ~totaldol, 
          hoverinfo = "text",
          text = ~paste("Total Costshare Amount:", d)) %>%
  add_bars(color = I("#FDD262")) %>%
  layout(xaxis = list(title = "NRCS Practice Type", showgrid = FALSE),
         yaxis = list(title = "Costshare Amount"),
         title = "Dollars Spent by Habitat Restoration Practice Types from 1998-Present")
H_graph

## livestock
total_l<- livestock %>%
  group_by(`NRCS.Code.and.Description`) %>%
  summarize(totaldol = sum(`Costshare.Amount`, na.rm = TRUE)) %>%
  na.omit(total_l)
total_l$totaldol = round(total_l$totaldol, 0)
d <- prettyNum(total_l$totaldol,big.mark = ",", scientific=FALSE)
total_l$d <- paste("$", sep="", d)

L_graph <- total_l %>%
  mutate(NRCS.Code.and.Description = fct_reorder(NRCS.Code.and.Description, totaldol, .desc = TRUE)) %>%
  plot_ly(x = ~NRCS.Code.and.Description, y = ~totaldol, 
          hoverinfo = "text",
          text = ~paste("Total Costshare Amount:", d)) %>%
  add_bars(color = I("#D3DDDC")) %>%
  layout(xaxis = list(title = "NRCS Practice Type", showgrid = FALSE),
         yaxis = list(title = "Costshare Amount"),
         title = "Dollars Spent by Livestock Practice Types from 1998-Present")
L_graph


###### Funding by year by project types

PROJECT_CS = FINAL %>%
  group_by(Project.Year, Project.Category) %>%
  summarize(Project_Sum = sum(`Costshare.Amount`, na.rm = TRUE)) %>%
  na.omit()
PROJECT_CS$Project_Sum = round(PROJECT_CS$Project_Sum, 0)

P_CS_SUM <- PROJECT_CS%>%
  group_by(Project.Year) %>%
  summarize(total = sum(Project_Sum, na.rm = TRUE)) %>%
  na.omit()
P_CS_SUM$total = round(P_CS_SUM$total, 0)

PROJECT_CS_SUM <- merge(PROJECT_CS, P_CS_SUM, by = "Project.Year")

d_pt <- prettyNum(PROJECT_CS_SUM$Project_Sum,big.mark = ",", scientific=FALSE)
PROJECT_CS_SUM$d_pt <- paste("$", sep="", d_pt)
d <-prettyNum(PROJECT_CS_SUM$total,big.mark = ",", scientific=FALSE)
PROJECT_CS_SUM$d <- paste("$", sep="", d)

total_cs <- PROJECT_CS_SUM %>%
  plot_ly(x = ~Project.Year, y = ~Project_Sum, color = ~Project.Category,
          hoverinfo = "text",
          text = ~paste("Year:", Project.Year, "<br>",
                        "Total Costshare Amount:", d, "<br>",
                        "Costshare Amount by Project Type:", d_pt)) %>%
  add_bars(colors = colors) %>%
  layout(xaxis = list(title = "Project Year"),
         yaxis = list(title = "Total Costshare Dollars Spent"), barmode = 'stack',
         title = "Total Costshare Dollars Spent from 1998-2019")
total_cs


###Total Funding
FINAL$Project.Year <- as.numeric(as.character(FINAL$Project.Year))
total_f<- FINAL %>%
  group_by(Project.Year) %>%
  summarize(totaldol = sum(`Costshare.Amount`, na.rm = TRUE))
d <-prettyNum(total_f$totaldol,big.mark = ",", scientific=FALSE)
total_f$d <- paste("$", sep="", d)

total_f$totaldol = round(total_f$d, 0)

total_funds <- total_f %>%
  plot_ly(x = ~Project.Year, 
          y = ~totaldol, 
          hoverinfo = "text",
          text = ~paste("Year:", Project.Year, "<br>",
                        "Total Costshare Amount:", d)) %>%
  add_bars(color = I("#81A88D")) %>%
  layout(xaxis = list(title = "Project Year", showgrid = FALSE),
         yaxis = list(title = "Costshare Amount"),
         title = "Dollars Spent by Funding Sources from 1998-Present")
total_funds




