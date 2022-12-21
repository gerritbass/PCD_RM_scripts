# THIS SCRIPT IS A WORKSPACE FOR CODE THAT GOES INTO BRADS VSP REPORT RMARKDOWN FILE. CODES IN "BRADS_PROJECT_REPORT.RMD" ARE THE MOST UP TO DATE.



# ENTER STREAM NAME (Alpowa, Deadman,Pataha,Hooper,Pullman, or Potlatch) 
STREAM_NAME <- "Deadman"

# Streamflow and Temp Statistical Analysis for Brad's Projects
library(ggplot2)
library(readxl)
library(dplyr)
library(ggpubr)
library(lubridate)

# Read in data
alpowa <-  read_excel("P:/Research_and_Monitoring/_04_Project_Data/PCD_RM_scripts/data/Brads_project_data/Alpowa_Creek_flow_and_temp.xlsx", sheet=3)

deadman <- read_excel("P:/Research_and_Monitoring/_04_Project_Data/PCD_RM_scripts/data/Brads_project_data/Deadman_Creek_flow_and_temp.xlsx", sheet=3)

pataha <- read_excel("P:/Research_and_Monitoring/_04_Project_Data/PCD_RM_scripts/data/Brads_project_data/Pataha_flow_and_temp.xlsx", sheet=3)

hooper <- read_excel("P:/Research_and_Monitoring/_04_Project_Data/Miscellaneous_Projects/Brad's Projects/Alpowa Creek WQ Analysis/Whitman County/Palouse_River_Hooper_1959_2020.xlsx", sheet = 14)

# if statement to pick which data to use based on stream name or call you loser
if (STREAM_NAME == 'Alpowa'){
  ECY_DATA <- alpowa
} else if (STREAM_NAME == 'Deadman'){
  ECY_DATA <- deadman
} else if (STREAM_NAME == 'Pataha'){
  ECY_DATA <- pataha
} else if (STREAM_NAME == 'Hooper'){
  ECY_DATA <- hooper
} else if (STREAM_NAME == 'Pullman'){
  ECY_DATA <- pullman
} else if ( STREAM_NAME =='Potlatch'){
  ECY_DATA <- potlatch
} else{print("Enter a Valid Stream Name Loser")}

# Seperating Temperature and Flow Data
TEMP = ECY_DATA[,c(1,3)]
Q = ECY_DATA[,c(1,2)]

## Analysis of Streamflow (Q)

# Cleaning Q
Q$Month = format(as.Date(Q$Date), "%m")
Q$Month = as.numeric(Q$Month)
SUMMER_Q = Q[Q$Month > 5 & Q$Month < 10,]
SUMMER_Q = SUMMER_Q[!is.na(SUMMER_Q$`Flow (cfs)`),]
SUMMER_Q$Year = format(as.Date(SUMMER_Q$Date), "%Y") 
SUMMER_Q$Group = NA

for(i in 1:nrow(SUMMER_Q)){
  if(SUMMER_Q$Year[i] < 2012){
    SUMMER_Q$Group[i] = "Baseline"
  }
  else{
    SUMMER_Q$Group[i] = "After Baseline"
  }
}

colnames(SUMMER_Q) = c("Date", "Value", "Month", "Year", "Group")

## Checking for Normality

#QQplot of Q
ggqqplot(SUMMER_Q, x = "Value",
         ggtheme = theme_pubclean())

# Histogram of Q
ggplot(SUMMER_Q, aes(x= Value, color = Group))+
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")
  

#split histogram of Q
p<-ggplot(SUMMER_Q, aes(x= Value, color= Group)) +
  geom_histogram(fill="white", position="dodge")+
  theme(legend.position="top")
p

# Shapiro Test of Q (p-value greater than 0.05 assumes normality)
shapiro.test(SUMMER_Q$Value)

# Kruskal-Wallis Test for Q
group_by(SUMMER_Q, Group) %>%
  summarise(count = n(),
            variance = var(Value, na.rm = TRUE),
            mean = mean(Value, na.rm=TRUE),
            sd = sd(Value, na.rm=TRUE),
            median = median(Value, na.rm=TRUE),
            IQR = IQR(Value, na.rm=TRUE)
  )
kruskal.test(Value ~ Group, data = SUMMER_Q)

mean(SUMMER_Q$Value)
sd(SUMMER_Q$Value)

var(SUMMER_Q$Value)
nrow(SUMMER_Q)


## Analysis of Stream Temperature

# Cleaning Temp
TEMP$Year = format(as.Date(TEMP$Date), "%Y")
TEMP$Month = format(as.Date(TEMP$Date), "%m")
TEMP$Month = as.numeric(TEMP$Month)
TEMP = TEMP %>% drop_na(`Water Temp`)
TEMP = TEMP[!is.na(TEMP$`Water Temp`),]
TEMP$Group = NA
for(i in 1:nrow(TEMP)){
  if(TEMP$Year[i] < 2012){
    TEMP$Group[i] = "Baseline"
  }
  else{
    TEMP$Group[i] = "After Baseline"
  }
}

colnames(TEMP) = c("Date", "Value","Year", "Month", "Group")

## Checking for Normality in Temperature

#qqplot of temp 
ggqqplot(TEMP, x = "Value",
         ggtheme = theme_pubclean())

# temperature histogram
ggplot(TEMP, aes(x= Value, color = Group))+
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

#split histogram of temp
p<-ggplot(TEMP, aes(x= Value, color= Group)) +
  geom_histogram(fill="white", position="dodge")+
  theme(legend.position="top")
p

# Shapiro Test of Temp (p-value greater than 0.05 assumes normality)
# 5000 is max number of samples the test will work with
shapiro_test(TEMP$Value[0:5000])  

# Kruskal-Wallis Test for Temp
group_by(TEMP, Group) %>%
  summarise(count = n(),
            variance = var(Value, na.rm = TRUE),
            mean = mean(Value, na.rm=TRUE),
            sd = sd(Value, na.rm=TRUE),
            median = median(Value, na.rm=TRUE),
            IQR = IQR(Value, na.rm=TRUE)
  )
kruskal.test(Value ~ Group, data = TEMP)

box_Q <- ggplot(SUMMER_Q, aes(x = Group, y = Value, fill = Group ))+
  geom_boxplot()+
  coord_flip()+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  theme_classic()+
  labs(x = '',y = 'Streamflow')+
  theme(legend.position="none") 
box_Q

SUMMER_Q %>% 
  mutate(Month = factor(month.abb[Month], levels = month.abb)) %>% 
  group_by(Month, Group) %>% 
  summarize(avg_monthly_flow = mean(Value)) %>% 
  ggplot(aes(x = Month, y = avg_monthly_flow, color = Group, group = Group))+
  geom_point(size = 3, stat='summary', fun.y = sum)+
  stat_summary(fun.y = sum, geom = "line", size = 1)+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  labs(y = 'Average Monthly Flow (cfs)')+
  theme_minimal()

TEMP %>% 
  mutate(Month = factor(month.abb[Month], levels = month.abb)) %>% 
  group_by(Month, Group) %>% 
  summarize(avg_monthly_temp = mean(Value)) %>% 
  ggplot(aes(x = Month, y = avg_monthly_temp, group = Group, color = Group))+
  geom_point(size = 3, stat='summary', fun.y = sum)+
  stat_summary(fun.y = sum, geom = "line", size = 1)+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  labs(y = 'Average Monthly Water Temperature (degC)')+
  theme_minimal()


