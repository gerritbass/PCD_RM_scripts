library(tidyverse)
library(lubridate)


WISKI_test <-  read_delim('P:/Research_and_Monitoring/_04_Project_Data/from_WISKI/test.zrx',skip = 5,comment = '#',col_names = FALSE,na = '-99999.0') %>%  
               mutate(X1 = ymd_hms(X1))
                      
colnames(WISKI_test) <- c('DATE','temp_C')                      
                
ggplot(WISKI_test, aes(x=DATE, y=temp_C))+
  geom_line()+
  geom_point()

#add test comments
