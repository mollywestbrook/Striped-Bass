## Organizing WQ Data

#libraries:
library(here)
library(tidyverse)
library(data.table)

#WT
#bring in data:
currentwtemp<-read_csv(here("WaterQualityDataRepo", "WTEMP_2023_08_30_30_full_for2324.txt"))

currentwtemp$Date <- "2023-08"
currentwtemp$EarlyorLate <- "Late"

WTdata <- rbind(currentwtemp, WTdata)

WTcomplete <- rbind(WTcomplete, WTdata)

#reformat Date to YYYY MM

# WTcomplete_tmp <- WTcomplete
# WTcomplete_tmp[c('Year', 'Month')] <- str_split_fixed(WTcomplete_tmp$Date, '-', 2)
# WTcomplete_tmp <- WTcomplete_tmp[-c(58)]
# WTcomplete <- WTcomplete_tmp

#DO
# currentDO<-read_csv(here("WaterQualityDataRepo", "DO_2023_08_30_30_full_for2324.txt"))
# 
# currentDO$EarlyorLate <- "Late"
# currentDO$Year <- "2023"
# currentDO$Month <- "08"
# 
# DOdata <- rbind(currentDO, DOdata)
# 
# DOcomplete <- rbind(DOcomplete, currentDO)

# #reformat Date to YYYY MM
# 
# Docomplete_tmp <- DOcomplete
# Docomplete_tmp[c('Year', 'Month')] <- str_split_fixed(DOcomplete$Date, '-', 2)
# Docomplete_tmp <- Docomplete_tmp[-c(58)]
# DOcomplete <- Docomplete_tmp
  
##################################################################

##Missing Data
#2023-08-Late
#2020-12, 2020-11

WTcomplete <- read_csv(here("WTcomplete.csv"))
 
WTComplete_tmp <- WTcomplete
WTComplete_tmp$ID <- paste(WTComplete_tmp$Year, WTComplete_tmp$Month, WTComplete_tmp$EarlyorLate)
WTsummary <- WTComplete_tmp %>% count(WTComplete_tmp$ID)

WTcomplete <- WTcomplete %>%
  arrange(Date)

fwrite(WTcomplete, file = "WTcomplete.csv", row.names=FALSE)

#I'm also going to double check the DO data for any additional missing data

# DOcomplete <- read_csv(here("DOcomplete.csv"))
# 
# DOcomplete_tmp <- DOcomplete
# DOcomplete_tmp$ID <- paste(DOcomplete_tmp$Year, DOcomplete_tmp$Month, DOcomplete_tmp$EarlyorLate)
# DOsummary <- DOcomplete_tmp %>% count(DOcomplete_tmp$ID)
# 
# DOcomplete <- DOcomplete %>%
#   arrange(Year, Month)
# 
# fwrite(DOcomplete, file = "DOcomplete.csv", row.names=FALSE)
