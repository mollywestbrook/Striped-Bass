### Calculating Historical Means

#2023 data

#this generates a list of the previous ten years
currentyear <- 2024
monthvector <- c("_01", "_02", "_03", "_04", "_05", "_06_early", "_06_late", "_07_early", "_07_late", "_08_early", "_08_late", "_09", "_10", "_11", "_12")

#this function brings each year's corresponding DO and temp files
#in case we're missing a year, it skips those years
fetcheveryyearsdata <- function (monthmatch) {
  tryCatch(
    {
      
      files <- list.files(path=here(), full.names = T)
      files <- Filter(function(x) any(grepl("DO|WTEMP", x)), files)
      files <- str_subset(files, as.character(currentyear))
      files <- str_subset(files, as.character(monthmatch))
      DOdata <- read_csv(str_subset(files, "DO"))
      wtempdata <- read_csv(str_subset(files, "WTEMP"))
      
      #This takes the DO data and transforms it so each layer's DO reading is vertically stored
      #And paired with its depth data:
      DO <- DOdata 
      DO[DO==-9] <- NA
      DO$ID <- cumsum(!duplicated(DO))
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
      historicwholebaydata <- data.frame(DOt$Segment, 
                                         DOt$UTM_X, 
                                         DOt$UTM_Y, 
                                         DOt$Sdepth, 
                                         DOt$volume_m, 
                                         DOt$DO, 
                                         wtempt$wtemp)
      month <- substr(monthmatch, 2,3)
      earlyorlate <- substr(monthmatch, 5, 9)
      historicwholebaydata <- historicwholebaydata %>%
        mutate(month = rep(as.numeric(month), nrow(historicwholebaydata))) %>%
        mutate(earlyorlate = rep(earlyorlate, nrow(historicwholebaydata)))
    },error = \(e) {
      print(e)
      return(NULL)
    }
  )
}

historicbaydata <- sapply(monthvector, fetcheveryyearsdata)
historicbaydata <- do.call(rbind.data.frame, historicbaydata) #this takes the list of the dfs we generated and makes it a big df
names(historicbaydata) <- c("Segment", "UTMX","UTMY", "Sdepth","volume_m","DO", "Wtemp", "month", "earlyorlate")
#rm(DO, DOdata, DOt, wtemp, wtempdata, wtempt)

#sort into bay segments:
historicbaydata<-historicbaydata[historicbaydata$Segment %in% mdsegments,]

#take our sorter for habitat suitability:

#whole bay
historicbaydatasummary <- historicbaydata %>%
  group_by(month, earlyorlate, Habitat) %>%
  summarize(volume = sum(volume_m, na.rm=TRUE)/1e+9) %>%
  mutate(percent = round(volume/sum(volume)*100, 2)) %>%
  mutate(color = case_when(
    Habitat == "Unsuitable" ~ "black",
    Habitat == "Marginal" ~ "orange",
    Habitat == "Tolerable" ~ "yellow",
    Habitat == "Suitable" ~ "dodgerblue")) %>%
  mutate(level = case_when(
    Habitat == "Unsuitable" ~ 0,
    Habitat == "Marginal" ~ 1,
    Habitat == "Tolerable" ~ 2,
    Habitat == "Suitable" ~ 3)
  ) %>%
  arrange(month) %>%
  filter(Habitat == "Suitable") #only need suitable

fwrite(historicbaydatasummary, "historicbaydatasummary_wholebay.csv")

#fishing coords
historicbaydatasummary_fishing <- historicbaydata_fishingareas %>%
  group_by(month, earlyorlate, Habitat) %>%
  summarize(volume = sum(volume_m, na.rm=TRUE)/1e+9) %>%
  mutate(percent = round(volume/sum(volume)*100, 2)) %>%
  mutate(color = case_when(
    Habitat == "Unsuitable" ~ "black",
    Habitat == "Marginal" ~ "orange",
    Habitat == "Tolerable" ~ "yellow",
    Habitat == "Suitable" ~ "dodgerblue")) %>%
  mutate(level = case_when(
    Habitat == "Unsuitable" ~ 0,
    Habitat == "Marginal" ~ 1,
    Habitat == "Tolerable" ~ 2,
    Habitat == "Suitable" ~ 3)
  ) %>%
  arrange(month) %>%
  filter(Habitat == "Suitable")

fwrite(historicbaydatasummary_fishing, "historicbaydatasummary_fishingspots.csv")