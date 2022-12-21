library(tidyverse)

# read in data
data <- read_csv("P:/Research_and_Monitoring/_04_Project_Data/NF_SF_WQ_stats.csv")


# find mean, min, and max for each varialbe across all streams
avg_by_parameter <- data %>% 
  group_by(Parameter) %>%
  summarise(mean(MIN),
            mean(MAx),
            mean(MEAN))

# find mean, min, and max for all variables at EACH stream
avg_by_stream <- data %>% 
  group_by(watershed,Parameter) %>%
  summarise(mean(MIN),
            mean(MAx),
            mean(MEAN))


