## Playing around with incorporating the SQL table from the DB for Striped Bass

start.time <- Sys.time()

#Libraries
library(here)
library(tidyverse)
library(data.table)
library(leaflet)
library(sf)
library(sp)
library(ggrepel)
library(plotly)
library(leafgl)
library(htmlwidgets)
library(ragg)
library(RODBC)
library(dbplyr)

tideROconnection <- odbcDriverConnect('driver={SQL Server};
                                    server=10.141.0.18;
                                    database=tide;
                                    trusted_connection=no;
                                    uid=tideRO;
                                    PWD=S8iche~9')

monthdate <- "11"
thisyear <- "2025"

yearmin <- as.numeric(thisyear) - 10
yearrange <- seq(yearmin, thisyear, 1)
yearlist <- as.list(yearrange)

DOdata <- sqlQuery(tideROconnection, paste0("SELECT * FROM [tide].[dbo].[ccnd_DOinterpolatoroutput] WHERE Date like '%%%%-", monthdate, "';"))

DOdata <- DOdata %>%
  group_by(Date) %>%
  filter(substr(Date, 1, 4) %in% yearlist)
DOdata[DOdata==-9] <- NA
DOdata$ID <- cumsum(!duplicated(DOdata[1:2]))

historicbaydata <- DOdata %>%
  group_by(Date) %>%
  mutate(volume_m = EW_dim*NS_dim*Vert_dim) %>%
  gather(key='layer', value='DO', 8:52) %>%
  mutate(layer = as.numeric(str_remove(layer, 'layer_'))) %>%
  arrange(ID) %>% 
  mutate(Sdepth = (layer-0.5)) %>%
  filter(!is.na(DO)) %>%
  select("Segment", "UTM_X","UTM_Y", "Sdepth","volume_m","DO")

# DOlist <- split(DOdata, DOdata$Date)
# 
# formatdata <- function(x) {
#   tryCatch(
#     {
#       DOt <- DO %>%
#         mutate(volume_m = EW_dim*NS_dim*Vert_dim) %>%
#         gather(key='layer', value='DO', 8:52) %>%
#         mutate(layer = as.numeric(str_remove(layer, 'layer_'))) %>%
#         arrange(ID) %>% 
#         mutate(Sdepth = (layer-0.5)) %>%
#         filter(!is.na(DO)) %>%
#         select("Segment", "UTM_X","UTM_Y", "Sdepth","volume_m","DO")
#     },error = \(e) {
#       print(e)
#       return(NULL)
#     }
#   )
# }
# 
# historicbaydata <- lapply(yearrange, formatdata)
# historicbaydata <- do.call(rbind.data.frame, historicbaydata)
names(historicbaydata) <- c("Date", "Segment", "UTMX","UTMY", "Sdepth","volume_m","DO")

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken