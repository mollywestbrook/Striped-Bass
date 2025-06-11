#Rewriting the for loops:

library(here)
library(tidyverse)
library(data.table)

#bring in DO file:

DOdata <- read_csv("DO_2023_07full.txt", col_names=T)

#DO (lines 57-76)
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

#Temp (lines 87-106)
Wtempdata <- read_csv("WTEMP_2023_07full.txt", col_names=T)

wtemp<-Wtempdata
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

#and drop the unnecessary files:
rm(DO, wtemp)

#rewrite line 108
wholebaydata <- data.frame(DOdatatransposed$Segment, 
                           DOdatatransposed$UTM_X, 
                           DOdatatransposed$UTM_Y, 
                           DOdatatransposed$Sdepth, 
                           DOdatatransposed$volume_m, 
                           DOdatatransposed$DO, 
                           wtempdatatransposed$wtemp)
names(wholebaydata) <- c("Segment", "UTMX","UTMY", "Sdepth","volume_m","DO", "Wtemp")

#rewrite for lines 198-203
points_within <- st_join(point.sf, shape.utm, join = st_within, left = FALSE)