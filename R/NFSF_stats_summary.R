library(tidyverse)
library(lubridate)
library(knitr)
library(kableExtra)
library(pander)
library(flextable)
library(plotly)
library(ggthemes)

PR_MASTER = read.csv("P:/Research_and_Monitoring/_04_Project_Data/Water_Quality/Palouse_River_Tribs/Stream_Flow_and_Water_Quality/Master_Data/Water_Quality_Data/PCD_ML_Merge_For_EIM_20220422.csv")

PR_MASTER_short <- PR_MASTER [ -c(1,3,9,10,11,13,22)]

PR_MASTER_long <- PR_MASTER_short %>% 
  pivot_longer(!Station, names_to = "Parameter", values_to = "value")


PALOUSE_BY_WATERSHED <- mutate(PR_MASTER_long, watershed = case_when(Station == 'STEPTOE0.70'~ "STEPTOE",
                                                                Station == 'STEPTOE5.23' ~ "STEPTOE",
                                                                Station == 'UFC27.31' ~ "UFC",
                                                                Station == 'WC0.01' ~ "WILBUR",
                                                                Station == 'FMC0.28' ~ "FOURMILE",
                                                                Station == 'NFPR12' ~ "DUFFIELD",
                                                                Station == 'NFPR3' ~ "CEDAR",
                                                                Station == 'NFPR6' ~ "SILVER",
                                                                Station == 'NFPR5A' ~ "SILVER",
                                                                Station == 'DC0.51' ~ "DRY",
                                                                Station == 'NFPR8' ~ "CLEAR",
                                                                Station == 'NFPR9' ~ "CLEAR",
                                                                Station == 'SFC0.35' ~ "MAINSTEM SOUTHFORK",
                                                                Station == 'SFPR12.98' ~ "MAINSTEM SOUTHFORK")) %>% 
  subset(watershed!= 'NA')


cols <- c("watershed","Parameter")

WATERSHED_STATS <- PALOUSE_BY_WATERSHED %>% 
  group_by(across(all_of(cols))) %>%
  summarise(MIN = min(value, na.rm = TRUE),
            MAx = max(value, na.rm=TRUE),
            MEAN = mean(value, na.rm = TRUE))

write_csv(WATERSHED_STATS,"P:\\Research_and_Monitoring\\_04_Project_Data\\PCD_RM_scripts\\data\\NF_SF_WQ_stats.csv")
