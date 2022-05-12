# Streamflow and Temp Statistical Analysis for Brad's Projects

library(ggplot2)
library(readxl)
library(dplyr)
library(ggpubr)

# Reading in Data
ECY_DATA = read_excel("P:/Research_and_Monitoring/Nick_Harris/Brad EIM Data/Streamflow_Temp_Data_New.xlsx", sheet=3)
TEMP = ECY_DATA[,c(1,3)]
Q = ECY_DATA[,c(1,2)]

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

#plot data to check for normality
ggqqplot(SUMMER_Q, x = "Value",
         ggtheme = theme_pubclean())

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


# Cleaning Temp
TEMP$Year = format(as.Date(TEMP$Date), "%Y")
TEMP = TEMP[!is.na(TEMP$`Water Temperature (?C)`),]
TEMP$Group = NA
for(i in 1:nrow(TEMP)){
  if(TEMP$Year[i] < 2012){
    TEMP$Group[i] = "Baseline"
  }
  else{
    TEMP$Group[i] = "After Baseline"
  }
}

colnames(TEMP) = c("Date", "Value","Year", "Group")

#plot data to check for normality
ggqqplot(TEMP, x = "Value",
         ggtheme = theme_pubclean())

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
mean(TEMP$Value)
sd(TEMP$Value)

var(TEMP$Value)
nrow(TEMP)
