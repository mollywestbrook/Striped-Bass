#Taking a stab at organizing a pipeline for the ten-year comparison

library(here)
library(tidyverse)
library(data.table)

currentyear <- 2021

function (currentyear) {

#Bring in this month's DO and Temp data:

files <- list.files(path=here(), full.names = T)
files <- Filter(function(x) any(grepl("DO|WTEMP", x)), files)
files <- str_subset(files, as.character(currentyear))
files <- str_subset(files, as.character(monthdate))
DOdata <- read_csv(str_subset(files, "DO"))
wtempdata <- read_csv(str_subset(files, "WTEMP"))

#This takes the DO data and transforms it so each layer's DO reading is vertically stored
#And paired with its depth data:
DO <- DOdata 
DO[DO==-9] <- NA
DO$ID <- cumsum(!duplicated(DO[1:2]))
DOt <- DO %>%
  mutate(volume_m = EW_dim*NS_dim*Vert_dim) %>%
  gather(key='layer', value='DO', 8:52) %>%
  mutate(layer = as.numeric(str_remove(layer, 'layer_'))) %>%
  arrange(ID) %>% 
  mutate(Sdepth = (layer-1)*0.5)%>%
  filter(!is.na(DO)) %>%
  select("Segment", "UTM_X","UTM_Y", "Sdepth","volume_m","DO")

#Same for Temp:
wtemp<-wtempdata
wtemp[wtemp==-9] <- NA
wtemp$ID <- cumsum(!duplicated(wtemp))
wtempt <- wtemp %>%
  mutate(volume_m = EW_dim*NS_dim*Vert_dim) %>%
  gather(key='layer', value='wtemp', 8:52) %>%
  mutate(layer = as.numeric(str_remove(layer, 'layer_'))) %>%
  arrange(ID) %>% 
  mutate(Sdepth = (layer-1)*0.5)%>%
  filter(!is.na(wtemp)) %>%
  select("Segment", "UTM_X","UTM_Y", "Sdepth","volume_m","wtemp")

#This formats the new transposed data into one dataset
#This works as it is because these two datasets are the same length
#If they aren't, it will need to be reformatted
historicwholebaydata <- data.frame(DOt$Segment, 
                           DOt$UTM_X, 
                           DOt$UTM_Y, 
                           DOt$Sdepth, 
                           DOt$volume_m, 
                           DOt$DO, 
                           wtempt$wtemp)
names(historicwholebaydata) <- c("Segment", "UTMX","UTMY", "Sdepth","volume_m","DO", "Wtemp")

historicwholebaydata$year <- rep(currentyear, nrow(historicwholebaydata))

rm(DO, DOdata, DOt, wtemp, wtempdata, wtempt)
}