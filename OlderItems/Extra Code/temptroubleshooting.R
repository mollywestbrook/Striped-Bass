# Checkovers for why the surface is so blue:

mddatathiscruise.dd_surfacesummary <- mddatathiscruise.dd %>%
  group_by(uniqueID) %>% 
  filter(Sdepth == 0) %>%
  ungroup() %>%
  group_by(habitat) %>%
  summarize(count = n())

mddatathiscruise.dd_surfacesummary <- mddatathiscruise.dd %>%
  filter(Sdepth == 0)

baymap <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  setView(lng = -76.3, lat = 39.2, zoom = 9) %>%
  addCircles(data = mddatathiscruise.dd_surfacesummary, color = ~color, group = "Whole Bay Suitability",
             label = paste(mddatathiscruise.dd_surfacesummary$Wtemp, fishingareacoords.dd_bottom$Sdepth, "ft", sep=" "))
baymap

######

#rearranging to calculate temperature troubleshoots:

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

wtempt <- wtempt %>%
  arrange(wtemp)

wtemp <- wtemp %>%
  arrange(layer_1)
### what is layer 1's wtemp:

ggplot(wtemp, aes(x=ID, y=layer_1))+
  geom_point(size=1)+
  geom_hline(yintercept=82.4)+
  geom_hline(yintercept=84.2)+
  geom_hline(yintercept=86)+
  theme_classic()

#let's calculate a summary for Tom:

totalbaysummary <- wtemp %>%
  mutate(habitat = case_when(
    layer_1>marginaltemp ~ "Unsuitable",
    layer_1<=marginaltemp & layer_1>tolerabletemp ~ "Marginal",
    layer_1<=tolerabletemp & layer_1>suitabletemp ~ "Tolerable",
    layer_1<=suitabletemp ~ "Suitable",
    TRUE ~ "NA")) %>%
  group_by(habitat) %>%
  summarize(count = n())

######

#we need a map displaying surface critera based on just layer_1

wtempfortom <- wtempdata %>%
  select(UTM_X, UTM_Y, Segment, layer_1) %>%
  rename(lat = UTM_X, long = UTM_Y) %>%
  st_as_sf(coords = c("lat","long"),crs=32618) %>%
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  mutate(habitat = case_when(
    layer_1>marginaltemp ~ "Unsuitable",
    layer_1<=marginaltemp & layer_1>tolerabletemp ~ "Marginal",
    layer_1<=tolerabletemp & layer_1>suitabletemp ~ "Tolerable",
    layer_1<=suitabletemp ~ "Suitable",
    TRUE ~ "NA"
  )) %>%
  mutate(color = case_when(
    habitat == "Unsuitable" ~ "black",
    habitat == "Marginal" ~ "orange",
    habitat == "Tolerable" ~ "yellow",
    habitat == "Suitable" ~ "dodgerblue",
    habitat == "NA" ~ "grey90")) %>%
  mutate(ID = row_number(.))

#filter the MD segments:

mdsegments<-c("BACOH","BIGMH","BOHOH"
              ,"BSHOH","C&DOH","CB1TF","CB2OH","CB3MH","CB4MH","CB5MH_MD","CB5MH"
              ,"CHOMH1","CHOMH2","CHOOH","CHOTF","CHSMH","CHSOH","CHSTF"
              ,"EASMH","ELKOH","FSBMH","GUNOH","HNGMH","LCHMH","MAGMH"
              ,"MANMH","MATTF","MIDOH","NANMH","NANOH","NANTF","NORTF"
              ,"PATMH","PAXMH","PAXOH","PAXTF","PISTF","POCMH","POCOH"
              ,"POCTF","POCMH_MD","POTMH","POTOH","POTTF","RHDMH","SASOH","SEVMH"
              ,"SOUMH","TANMH","TANMH_MD","WICMH","WSTMH")
wtempfortom_md <- wtempfortom[wtempfortom$Segment %in% mdsegments,]

baymap <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  setView(lng = -76.3, lat = 39.2, zoom = 9) %>%
  addCircles(data = wtempfortom_md, color = ~color, group = "Whole Bay Suitability",
             label = paste(wtempfortom_md$layer_1))
baymap

baymap <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  setView(lng = -76.3, lat = 39.2, zoom = 9) %>%
  addCircles(data = wtempfortom, color = ~color, group = "Whole Bay Suitability",
             label = paste(wtempfortom$layer_1))
baymap

wtempsummary <- wtempfortom_md %>%
  group_by(habitat) %>%
  summarize(count = n())

ggplot(wtempfortom_md, aes(x=ID, y=layer_1))+
  geom_point(size=1)+
  geom_hline(yintercept=82.4, color = "yellow")+
  geom_hline(yintercept=84.2, color = "orange")+
  geom_hline(yintercept=86, color = "black")+
  theme_classic()

#######################################################################################

#raw data from the cruise--let's do the same thing. 

wtempfortom_raw <- rawcruisedata %>%
  select(Station, Sdepth, Wtemp) %>%
  filter(Sdepth == 0.5) %>%
  mutate(WtempF = (Wtemp*1.8)+32) %>%
  mutate(habitat = case_when(
    WtempF>marginaltemp ~ "Unsuitable",
    WtempF<=marginaltemp & WtempF>tolerabletemp ~ "Marginal",
    WtempF<=tolerabletemp & WtempF>suitabletemp ~ "Tolerable",
    WtempF<=suitabletemp ~ "Suitable",
    TRUE ~ "NA"
  )) %>%
  mutate(color = case_when(
    habitat == "Unsuitable" ~ "black",
    habitat == "Marginal" ~ "orange",
    habitat == "Tolerable" ~ "yellow",
    habitat == "Suitable" ~ "dodgerblue",
    habitat == "NA" ~ "grey90")) %>%
  mutate(ID = row_number(.))

wtempraw_summary <- wtempfortom_raw %>%
  group_by(habitat) %>%
  summarize(count = n())

ggplot(wtempfortom_raw, aes(x=ID, y=WtempF))+
  geom_point(size=1)+
  geom_hline(yintercept=82.4, color = "yellow")+
  geom_hline(yintercept=84.2, color = "orange")+
  geom_hline(yintercept=86, color = "black")+
  theme_classic()




