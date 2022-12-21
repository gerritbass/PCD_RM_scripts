# Gerrit Bass Winer 2022/23
# Script to get a specific file of  old thorn data into WISKI


file1 <- "R:\\_04_Project_Data\\Water_Quality\\_6_Archive\\Cow_and_Thorn_Creeks\\Thorn Creek\\Stream_Flow_and_Water_Quality\\Continuous_Station_Data\\_2_Working\\Archive\\THORN_SC_COMPILED_2020-06-29.csv"


thorn_data <- read_csv(file1)

thorn_data_cleaned <- thorn_data %>% 
  mutate(Site = "B9D00E7E") %>% 
  select(3,13,12,6:11) %>% 
  rename(WT = 3, SPC = 4, pH = 5, DOPerc = 6, DO = 7, TURB = 8, S = 9)


write_csv(thorn_data_cleaned,paste("R:\\_04_Project_Data\\Water_Quality\\_3_Incoming_Data\\_To_WISKI\\Continuous Station\\missingthorn5_18-6_20.csv"))
