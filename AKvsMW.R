## Checking my file against Andrew's

library(here)
library(tidyverse)
library(data.table)
library(readxl)

###Cruise Sheets

cruisedata_ak<-read_xlsx("BAY895.xlsx")
cruisedata_mw<-read_csv("BAY895_forbecca.csv")

#note: m's sheet is in F, quick convert to C:
cruisedata_mw$Wtemp <- (cruisedata_mw$Wtemp - 32)/1.8

#check the cols:
setdiff(cruisedata_ak$Station, cruisedata_mw$Station)
setdiff(cruisedata_ak$Date, cruisedata_mw$Date)
setdiff(cruisedata_ak$Sdepth, cruisedata_mw$Sdepth)
setdiff(cruisedata_ak$Wtemp, cruisedata_mw$tempF) #note: MW stored F temps in separate col
setdiff(cruisedata_ak$Ph, cruisedata_mw$Ph)
setdiff(cruisedata_ak$DO, cruisedata_mw$DO)
setdiff(cruisedata_ak$Spcond, cruisedata_mw$Spcond)
setdiff(cruisedata_ak$Salinity, cruisedata_mw$Salinity)
setdiff(cruisedata_ak$Cruise, cruisedata_mw$Cruise)
setdiff(cruisedata_ak$SECCHI, cruisedata_mw$SECCHI)

###DO Sheets
DOdata_ak <- read_csv("DO_2025_08full_LATE.txt")
DOdata_mw <- read_csv("DO_2025_08full_LATE_mw.txt")

#DOdata_mw_new <- read_csv("DO_2025_07full_EARLY_mw_updated.txt")

#convert to vertical, then we'll setdiff:
DO <- DOdata_ak 
DO[DO==-9] <- NA
DO$ID <- cumsum(!duplicated(DO[1:2]))
DOt_ak <- DO %>%
  mutate(volume_m = EW_dim*NS_dim*Vert_dim) %>%
  gather(key='layer', value='DO', 8:52) %>%
  mutate(layer = as.numeric(str_remove(layer, 'layer_'))) %>%
  arrange(ID) %>% 
  mutate(Sdepth = (layer-1)*0.5)%>%
  filter(!is.na(DO)) %>%
  select("Segment", "UTM_X","UTM_Y", "Sdepth","volume_m","DO")
DO <- DOdata_mw
DO[DO==-9] <- NA
DO$ID <- cumsum(!duplicated(DO[1:2]))
DOt_mw <- DO %>%
  mutate(volume_m = EW_dim*NS_dim*Vert_dim) %>%
  gather(key='layer', value='DO', 8:52) %>%
  mutate(layer = as.numeric(str_remove(layer, 'layer_'))) %>%
  arrange(ID) %>% 
  mutate(Sdepth = (layer-1)*0.5)%>%
  filter(!is.na(DO)) %>%
  select("Segment", "UTM_X","UTM_Y", "Sdepth","volume_m","DO")

#and check the cols:
setdiff(DOt_ak$Segment, DOt_mw$Segment) 
#setdiff(DOt_mw$Segment, DOt_ak$Segment) 

setdiff(DOt_ak$UTM_X, DOt_mw$UTM_X)
setdiff(DOt_ak$UTM_Y, DOt_mw$UTM_Y)
setdiff(DOt_ak$Sdepth, DOt_mw$Sdepth)
setdiff(DOt_ak$volume_m, DOt_mw$volume_m)
setdiff(DOt_ak$DO, DOt_mw$DO)


###Temp Sheets

Wtempdata_ak <- read_csv("WTEMP_2025_08full_LATE.txt")
Wtempdata_mw <- read_csv("WTEMP_2025_08full_LATE_mw.txt")

wtemp<-Wtempdata_ak
wtemp[wtemp==-9] <- NA
wtemp$ID <- cumsum(!duplicated(wtemp))
wtempt_ak <- wtemp %>%
  mutate(volume_m = EW_dim*NS_dim*Vert_dim) %>%
  gather(key='layer', value='wtemp', 8:52) %>%
  mutate(layer = as.numeric(str_remove(layer, 'layer_'))) %>%
  arrange(ID) %>% 
  mutate(Sdepth = (layer-1)*0.5)%>%
  filter(!is.na(wtemp)) %>%
  select("Segment", "UTM_X","UTM_Y", "Sdepth","volume_m","wtemp")
wtemp<-Wtempdata_mw
wtemp[wtemp==-9] <- NA
wtemp$ID <- cumsum(!duplicated(wtemp))
wtempt_mw <- wtemp %>%
  mutate(volume_m = EW_dim*NS_dim*Vert_dim) %>%
  gather(key='layer', value='wtemp', 8:52) %>%
  mutate(layer = as.numeric(str_remove(layer, 'layer_'))) %>%
  arrange(ID) %>% 
  mutate(Sdepth = (layer-1)*0.5)%>%
  filter(!is.na(wtemp)) %>%
  select("Segment", "UTM_X","UTM_Y", "Sdepth","volume_m","wtemp")

setdiff(wtempt_ak$Segment, wtempt_mw$Segment)
setdiff(wtempt_ak$UTM_X, wtempt_mw$UTM_X)
setdiff(wtempt_ak$UTM_Y, wtempt_mw$UTM_Y)
setdiff(wtempt_ak$Sdepth, wtempt_mw$Sdepth)
setdiff(wtempt_ak$volume_m, wtempt_mw$volume_m)
setdiff(wtempt_ak$wtemp, wtempt_mw$wtemp)

#note: MW rounded their temp data, likely due to F to C conversion
#if we round AK's data, will it be the same?
wtempt_ak$wtemp <- round(wtempt_ak$wtemp, 1)
setdiff(wtempt_ak$wtemp, wtempt_mw$wtemp)

#one more check on the raw DO and Temp sheets for the segment mismatch:
setdiff(DOdata_mw$Segment, DOdata_ak$Segment)
#output: "CB5MH" "POCMH" "TANMH"

setdiff(Wtempdata_mw$Segment, Wtempdata_ak$Segment)
#output: "CB5MH" "POCMH" "TANMH"






