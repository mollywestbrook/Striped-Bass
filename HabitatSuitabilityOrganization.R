#Habitat Suitability for Striped Bass

#before/during each run
#update cruise line in 26
#on first run, uncomment lines 369-371 for shape file writes
#update percentsuitable csvs in main directory with the current month's percent suitable habitat

#This script will generate the data tables and figures necessary for a subsequent app file

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

#Define Variables

#What cruise are we working with? Update for this month's
rawcruisedata<-read_csv("BAY913.csv")

#save a copy to the stripd bass habitat:
#fwrite(rawcruisedata, file = here("Striped-Bass-Habitat-Suitability"), "BAY893.csv", row.names=FALSE)

###Ensure the data is confined to a single month, otherwise filter out extraneous dates
startdate<-min(rawcruisedata$Date, na.rm=T)
enddate<-max(rawcruisedata$Date, na.rm=T)

# rawcruisedata <- rawcruisedata %>%
#   filter(Date == startdate)

#This section ID's the cruise date to grab associated DO and Temp files later
monthdate <- as.numeric(substr(startdate,6,7))
monthname <- case_when(monthdate == '1' ~ 'January',
                      monthdate == '2' ~ 'February',
                      monthdate == '3' ~ 'March',
                      monthdate == '4' ~ 'April',
                      monthdate == '5' ~ 'May',
                      monthdate == '6' ~ 'June',
                      monthdate == '7' ~ 'July',
                      monthdate == '8' ~ 'August',
                      monthdate == '9' ~ 'September',
                      monthdate == '10' ~ 'October',
                      monthdate == '11' ~ 'November',
                      monthdate == '12' ~ 'December')
thisyear<-substr(startdate,1,4)

#And define habitat parameters for Striped Bass:
suitabletemp<-82.4
tolerabletemp<-84.2
marginaltemp<-86
unsuitabletemp<-86
suitableDO<-4
tolerableDO<-3
marginalDO<-2
unsuitableDO<-2

#color palette:
suitability_colors <- c(
  "Unsuitable" = "black",
  "Marginal" = "orange",
  "Tolerable" = "yellow",
  "Suitable" = "dodgerblue"
)
#############################################################################################

#Step one: bring in this month's DO and temp files, and format

#Bring in this month's DO and Temp data:
files <- list.files(path=here(), full.names = T)
files <- Filter(function(x) any(grepl("DO|WTEMP", x)), files)
files <- str_subset(files, thisyear)
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
  mutate(Sdepth = (layer-0.5)) %>%
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
  mutate(Sdepth = (layer-0.5)) %>%
  filter(!is.na(wtemp)) %>%
  select("Segment", "UTM_X","UTM_Y", "Sdepth","volume_m","wtemp")

#This formats the new transposed data into one dataset
#This works as it is because these two datasets are the same length
#If they aren't, it will need to be reformatted
wholebaydata <- data.frame(DOt$Segment, 
                           DOt$UTM_X, 
                           DOt$UTM_Y, 
                           DOt$Sdepth, 
                           DOt$volume_m, 
                           DOt$DO, 
                           wtempt$wtemp)
#uncomment this line if the dfs are two different lengths
#historicwholebaydata <- full_join(DOt, wtempt, by=c("Segment", "UTM_X", "UTM_Y", "Sdepth", "volume_m"), relationship = "many-to-many")
names(wholebaydata) <- c("Segment", "UTMX","UTMY", "Sdepth","volume_m","DO", "Wtemp")

#Cleanup
rm(DO, wtemp, DOdata, wtempdata, DOt, wtempt)

#Generate a unique ID for each measurement:
wholebaydata$uniqueID<-paste(wholebaydata$UTMX,wholebaydata$UTMY)
#uniqueIDs<-unique(wholebaydata$uniqueID)
#Transform depth to feet
wholebaydata$Sdepthm<-wholebaydata$Sdepth
wholebaydata$Sdepth<-wholebaydata$Sdepthm*3.28084  

#mutate in our parameters:
wholebaydata <- wholebaydata %>%
  mutate(habitat = case_when(
    Wtemp>marginaltemp | DO<marginalDO ~ "Unsuitable",
    Wtemp<=marginaltemp & Wtemp>tolerabletemp | DO>=marginalDO & DO<tolerableDO ~ "Marginal",
    Wtemp<=tolerabletemp & Wtemp>suitabletemp | DO>=tolerableDO & DO<suitableDO ~ "Tolerable",
    Wtemp<=suitabletemp | DO>=suitableDO ~ "Suitable",
    TRUE ~ "NA")) %>%
  mutate(color = case_when(
    habitat == "Unsuitable" ~ "black",
    habitat == "Marginal" ~ "orange",
    habitat == "Tolerable" ~ "yellow",
    habitat == "Suitable" ~ "dodgerblue",
    habitat == "NA" ~ "grey90"))

#filter for MD segments:
mdsegements <- fread("mdsegments.csv", header = F)
mddatathiscruise<-wholebaydata %>%
  filter(Segment %in% mdsegements$V1)

###############################

#Now that we have our cruise data, we need to identify the fishing hotspots using GIS data

#bring in fishing areas
fishingareapolygons <- st_read(here())
fishingareapolygons.utm <-st_transform(fishingareapolygons, crs=32618) #transform to UTM

#MW can come back to this later, but for now we drop Lighthouse Lump and Loon Hill
#As the GIS data doesn't overlap with bay data:
fishingareapolygons.utm <- fishingareapolygons.utm %>%
  filter(!name %in% c("Light House Lump", "Loon Hill"))

#Transform to simple features shapes
wholebaydatacoords <- data.frame("long"=wholebaydata$UTMX,"lat"=wholebaydata$UTMY)
wholebaydatacoords_sf <- st_as_sf(wholebaydatacoords, coords = c("long","lat"),crs=32618)

#this takes the point data and filters to only include the fishing areas with labels:
fishingareacoords <- st_join(wholebaydatacoords_sf, fishingareapolygons.utm, join = st_within, left = FALSE)

#transform the coordinates from simple features to dataframe numbers
fishingareacoords_df <- as.data.frame(fishingareacoords)
fishingareacoords_df$X <- substring(fishingareacoords_df$geometry,3,nchar(fishingareacoords_df))
fishingareacoords_df$X <- gsub("\\,.*","",fishingareacoords_df$X)
fishingareacoords_df$Y <- gsub(".*,","",fishingareacoords_df$geometry)
fishingareacoords_df$Y <- substring(fishingareacoords_df$Y,1,nchar(fishingareacoords_df$Y)-1)
fishingareacoords_df$X <- as.numeric(as.character(fishingareacoords_df$X))
fishingareacoords_df$Y <- as.numeric(as.character(fishingareacoords_df$Y))
fishingareacoords_df <- fishingareacoords_df[c("X","Y","name")]
colnames(fishingareacoords_df)[which(names(fishingareacoords_df) == "X")] <- "UTMX"
colnames(fishingareacoords_df)[which(names(fishingareacoords_df) == "Y")] <- "UTMY"

#merge the fishing areas to the water quality
fishingareadatathiscruise <- left_join(fishingareacoords_df, mddatathiscruise, by=c("UTMX","UTMY"), relationship = "many-to-many")

#cleanup
rm(fishingareapolygons, wholebaydatacoords, wholebaydatacoords_sf)

################

#additional stuff for the leaflet map, we need to convert the fishing areas and whole bay data to decimal degree
fishingareapolygons.dd <- st_transform(fishingareapolygons.utm, crs = "+proj=longlat +datum=WGS84") #transform to DD
fishingareacoords.sf <- st_as_sf(x=fishingareacoords, coords = c("long","lat"),crs=32618)
fishingareacoords.dd <- st_transform(fishingareacoords.sf, crs = "+proj=longlat +datum=WGS84") #transform to DD
fishingareacoords.dd$UTMX <- fishingareacoords_df$UTMX 
fishingareacoords.dd$UTMY <- fishingareacoords_df$UTMY
fishingareacoords.dd <- left_join(fishingareacoords.dd, wholebaydata, by=c("UTMX","UTMY"), relationship = "many-to-many")

names(mddatathiscruise)[names(mddatathiscruise) == "UTMX"] <- "lat"
names(mddatathiscruise)[names(mddatathiscruise) == "UTMY"] <- "long"
mddatathiscruise.sf <- st_as_sf(x=mddatathiscruise, coords = c("lat","long"),crs=32618)
mddatathiscruise.dd <- st_transform(mddatathiscruise.sf, crs = "+proj=longlat +datum=WGS84") #transform to DD
mddatathiscruise.dd$UTMX <- mddatathiscruise$lat
mddatathiscruise.dd$UTMY <- mddatathiscruise$long

#################################################################################

#And now we do a similar process for the last ten years:

#Step one: bring in previous ten years DO and Temp filesfor the month we're in, and format 

#this generates a list of the previous ten years
yearmin <- as.numeric(thisyear) - 10
yearrange <- seq(yearmin, thisyear, 1)
yearlist <- as.list(yearrange)

#this function brings each year's corresponding DO and temp files
#in case we're missing a year, it skips those years
fetcheveryyearsdata <- function (currentyear) {
  tryCatch(
    {
      
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
        mutate(Sdepth = (layer-0.5)) %>%
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
        mutate(Sdepth = (layer-0.5)) %>%
        filter(!is.na(wtemp)) %>%
        select("Segment", "UTM_X","UTM_Y", "Sdepth","volume_m","wtemp")
      
      #This formats the new transposed data into one dataset
      #getting rid of this; we've run into a few cases where the df are slightly diff lengths
      #CHECK TO MAKE SURE EVERY FILE MATCHES FIRST
      # historicwholebaydata <- data.frame(DOt$Segment,
      #                                    DOt$UTM_X,
      #                                    DOt$UTM_Y,
      #                                    DOt$Sdepth,
      #                                    DOt$volume_m,
      #                                    DOt$DO,
      #                                    wtempt$wtemp)
      historicwholebaydata <- full_join(DOt, wtempt, by=c("Segment", "UTM_X", "UTM_Y", "Sdepth", "volume_m"), relationship = "many-to-many")
      historicwholebaydata <- historicwholebaydata %>%
        mutate(year = rep(currentyear, nrow(historicwholebaydata)))
    },error = \(e) {
      print(e)
      return(NULL)
    }
  )
}

historicbaydata <- lapply(yearrange, fetcheveryyearsdata)
historicbaydata <- do.call(rbind.data.frame, historicbaydata) #this takes the list of the dfs we generated and makes it a big df
names(historicbaydata) <- c("Segment", "UTMX","UTMY", "Sdepth","volume_m","DO", "Wtemp", "year")
#rm(DO, DOdata, DOt, wtemp, wtempdata, wtempt)

#mutate in habitat parameters:
historicbaydata <- historicbaydata %>%
  mutate(uniqueID = paste(UTMX, UTMY, sep= " ")) %>%
  mutate(Habitat = case_when(
    Wtemp>marginaltemp | DO<marginalDO ~ "Unsuitable",
    Wtemp<=marginaltemp & Wtemp>tolerabletemp | DO>=marginalDO & DO<tolerableDO ~ "Marginal",
    Wtemp<=tolerabletemp & Wtemp>suitabletemp | DO>=tolerableDO & DO<suitableDO ~ "Tolerable",
    Wtemp<=suitabletemp | DO>=suitableDO ~ "Suitable",
    TRUE ~ NA)) %>%
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
  )

#sort into bay segments:
historicbaydata<-historicbaydata %>%
  filter(Segment %in% mdsegements$V1)

rm(yearlist)

#################################

#Step two: get the fishing area coordinates

#we have the fishing area coords from when we did this for just this year
#so we can reuse the same dataframe and filter it for the historical data
historicbaydata_fishingareas <- left_join(fishingareacoords_df, historicbaydata, by=c("UTMX","UTMY"), relationship = "many-to-many")

################################################################################################

### FIGURES ###

#Note: I don't save these figures anymore
#Mostly because it takes up space and is unnecessary. But if needed, uncomment the 'saveWidget' functions.

#Now we want a beautiful leaflet map. 
#It's going to have 3 layers: the fishing hotspots, the suitability of the hotspots, and the suitability of the whole bay

#Filter bottom for the map:
fishingareacoords.dd_bottom <- fishingareacoords.dd %>%
  group_by(uniqueID) %>% 
  filter(Sdepth == max(Sdepth, na.rm=T))
mddatathiscruise.dd_bottom <- mddatathiscruise.dd %>%
  group_by(uniqueID) %>% 
  filter(Sdepth == max(Sdepth, na.rm=T))

baymap <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  setView(lng = -76.3, lat = 39.2, zoom = 9) %>%
  addPolygons(
    data = fishingareapolygons.dd, color = "#8373e2", stroke = 0.2, opacity = 0.8,
    label = fishingareapolygons.dd$name, group = "Fishing Areas") %>%
  addCircles(
    data = fishingareacoords.dd_bottom, color = ~color, group = "Fishing Area Suitability",
    label = paste0(fishingareacoords.dd_bottom$name, fishingareacoords.dd_bottom$Sdepth, "ft")) %>%
  addCircles(data = mddatathiscruise.dd_bottom, color = ~color, group = "Whole Bay Suitability") %>%
  addLayersControl(
    overlayGroups = c("Fishing Areas", "Fishing Area Suitability", "Whole Bay Suitability"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("Fishing Areas", "Whole Bay Suitability"))
baymap

#save the shape files for the app:

# commented for subsequent runs; st_write doesn't overwrite, so uncomment this line on first run
# 
st_write(fishingareapolygons.dd, here("Striped-Bass-Habitat-Suitability", "FishingAreaPolygons", paste(monthname, thisyear, "fishingareapolygons.dd.shp", sep="")))
st_write(fishingareacoords.dd_bottom, here("Striped-Bass-Habitat-Suitability", "FishingAreaQuality", paste(monthname, thisyear, "fishingareacoords.dd_bottom.shp", sep="")))
st_write(mddatathiscruise.dd_bottom, here("Striped-Bass-Habitat-Suitability", "WholeBayQuality", paste(monthname, thisyear, "mddatathiscruise_dd_bottom.shp", sep="")))

###################################################################################

#Pie Charts Habitat Suitability

################MD Bay

#calculate bottom summary
wholebaybottomsummary <- mddatathiscruise.dd_bottom %>%
  st_drop_geometry() %>%
  group_by(habitat, color) %>%
  summarize(volume = sum(volume_m, na.rm=TRUE)/1e+9) %>%
  ungroup() %>%
  mutate(volumetotal = sum(volume)) %>%
  mutate(percent = volume/volumetotal*100)

#write this df out for the shiny app:
fwrite(wholebaybottomsummary, file = here("Striped-Bass-Habitat-Suitability", paste(monthname, thisyear, "wholebaysummary.csv", sep="")), row.names=FALSE)

#automatically write percent suitable to csv
# MW -- fix this later, this doesn't work!
percentages_wholebay <- fread("percentsuitable_wholebay_currentyear.csv")
percentages_wholebay <- percentages_wholebay %>%
  mutate(percentsuitablewb = case_when(months == monthname ~ wholebaybottomsummary[wholebaybottomsummary$habitat == "Suitable"]$percent,
                                          .default = percentsuitablewb))
fwrite(percentages_wholebay, "percentsuitable_wholebay_currentyear.csv")

#generate a figure
wholebaysummaryplot <- plot_ly(wholebaybottomsummary, labels = ~habitat, values = ~percent, type = 'pie',
             textposition = 'outside',
             textinfo = 'label+percent',
             marker = list(colors = wholebaybottomsummary$color)) %>%
  layout(title = 'Whole Bay Habitat Suitability',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
wholebaysummaryplot

#saveWidget(as_widget(wholebaysummaryplot), paste(here("AppFigures"),"/PieChart_WholeBay",monthdate,thisyear,".html", sep=""))

################Hot Spots

fishinghotspotsummary <- fishingareacoords.dd_bottom %>%
  st_drop_geometry() %>%
  group_by(habitat, color) %>%
  summarize(volume = sum(volume_m, na.rm=TRUE)/1e+9) %>%
  ungroup() %>%
  mutate(volumetotal = sum(volume)) %>%
  mutate(percent = volume/volumetotal*100)

#write out for the app
fwrite(fishinghotspotsummary, file = here("Striped-Bass-Habitat-Suitability", paste(monthname, thisyear, "fishinghotspotsummary.csv", sep="")), row.names=FALSE)

#automatically write percent suitable to csv
percentages_hotspot <- fread("percentsuitable_hotspots_currentyear.csv")
percentages_hotspot <- percentages_wholebay %>%
  mutate(percentsuitablehs = case_when(months == monthname ~ fishinghotspotsummary[fishinghotspotsummary$habitat == "Suitable"]$percent,
                                       .default = percentsuitablehs))
fwrite(percentages_hotspot, "percentsuitable_hotspots_currentyear.csv")

fishinghotspotsplot <- plot_ly(fishinghotspotsummary, labels = ~habitat, values = ~percent, type = 'pie',
                               textposition = 'outside',
                               textinfo = 'label+percent',
                               marker = list(colors = fishinghotspotsummary$color)) %>%
  layout(title = 'Fishing Hotspots Habitat Suitability',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fishinghotspotsplot

#saveWidget(as_widget(fishinghotspotsplot), paste(here("AppFigures"),"/PieChart_FishingHotspots",monthdate,thisyear,".html", sep=""))

###################################################################################

#Cross-section figures

##############MD cross-section

#bring in cross section file
crosssectionmain<-read_csv("mainchannelpointsCLEAN.csv")
crosssectionmain<-crosssectionmain[c("UTMX","UTMY")]
crosssectionmain$keep<-"YES"

#df organization and calculate distance from mouth
names(mddatathiscruise)[names(mddatathiscruise) == "lat"] <- "UTMX"
names(mddatathiscruise)[names(mddatathiscruise) == "long"] <- "UTMY"
mainchanneldata<-merge(mddatathiscruise,crosssectionmain,by=c("UTMX","UTMY"),allow.cartesian=TRUE)
mainchanneldata<-unique(mainchanneldata)
mainchanneldata$milesX<-mainchanneldata$UTMX*0.000621371
mainchanneldata$milesY<-mainchanneldata$UTMY*0.000621371
mainchanneldata$distfrommouth<-mainchanneldata$milesY-min(mainchanneldata$milesY)

#write out for the app
fwrite(mainchanneldata, file = here("Striped-Bass-Habitat-Suitability", paste(monthname, thisyear, "mainchanneldata.csv", sep="")), row.names=FALSE)

#hand coding in the geographic labels
labels <- read_csv("labels.csv")

#organize labels
labels_mainstem <- labels %>%
  filter(label == "bay") %>%
  mutate(milesY = y*0.000621371) %>%
  mutate(distfrommouth = milesY - min(mainchanneldata$milesY))

fwrite(labels_mainstem, file = here("Striped-Bass-Habitat-Suitability", paste(monthname, thisyear, "labels_mainstem.csv", sep="")), row.names=FALSE)

#mainstem cross-section
mainchannelplotly<-plot_ly()%>%
  config(displayModeBar=T, modeBarButtonsToRemove = c("zoom", "autoScale2d","toggleSpikelines","select2d","lasso2d")) %>%
  add_trace(x=mainchanneldata$distfrommouth ,y=mainchanneldata$Sdepth
            ,type='scatter',mode='markers'
            ,marker=list(color=mainchanneldata$color))%>%
  add_annotations(x=labels_mainstem$distfrommouth, #annotations for city labels
                 y=0,
                 text=labels_mainstem$name,
                 xref = "x",
                 yref = "y",
                 showarrow = T,
                 arrowhead = 0,
                 arrowsize = 0.5,
                 ax = 20,
                 ay = -30,
                 textposition="top" )%>%
  layout(xaxis=list(title="Distance from mouth of Bay (miles)",autorange="reversed", zeroline=F, showgrid=F))%>%
  layout(yaxis=list(title="Depth (ft)",autorange="reversed", zeroline=F, showgrid=F))
mainchannelplotly

#saveWidget(as_widget(mainchannelplotly), paste(here("AppFigures"),"/MainChannel",monthdate,thisyear,".html", sep=""))

##############Potomac cross-section

crosssectionpotomac<-read.csv(here("potomacmainstem.csv"))
crosssectionpotomac<-crosssectionpotomac[c("UTMX","UTMY")]
crosssectionpotomac$keep<-"YES"

#generating potomac axis labels, calculate distance from mouth
potomacchanneldata<-merge(mddatathiscruise,crosssectionpotomac,by=c("UTMX", "UTMY"),allow.cartesian=TRUE)
potomacchanneldata<-unique(potomacchanneldata)
potomacchanneldata <- potomacchanneldata %>%
  mutate(milesx = UTMX*0.000621371,
         milesy = UTMY*0.000621371)
potomacchanneldata <- potomacchanneldata %>%
  mutate(distfrommouth = milesy - min(potomacchanneldata$milesy, na.rm=T))

fwrite(potomacchanneldata, file = here("Striped-Bass-Habitat-Suitability", paste(monthname, thisyear, "potomacchanneldata.csv", sep="")), row.names=FALSE)

#organize labels:
labels_potomac <- labels %>%
  filter(label == "potomac") %>%
  mutate(milesY = y*0.000621371) %>%
  mutate(distfrommouth = milesY - min(potomacchanneldata$milesy, na.rm=T))

fwrite(labels_potomac, file = here("Striped-Bass-Habitat-Suitability", paste(monthname, thisyear, "labels_potomac.csv", sep="")), row.names=FALSE)

#potomac cross-section
potomacchannelplotly <- plot_ly()%>%
  config(displayModeBar=T, modeBarButtonsToRemove = c("autoScale2d","hoverCompareCartesian","toggleSpikelines","select2d","lasso2d")) %>%
  add_trace(x=potomacchanneldata$distfrommouth,y=potomacchanneldata$Sdepth
            ,type='scatter',mode='markers'
            ,marker=list(color=potomacchanneldata$color))%>%
  add_annotations(x=labels_potomac$distfrommouth, #annotations for city labels
                  y=0,
                  text=labels_potomac$name,
                  xref = "x",
                  yref = "y",
                  showarrow = T,
                  arrowhead = 0,
                  arrowsize = 0.5,
                  ax = 20,
                  ay = -30,
                  textposition="top" )%>%
  layout(xaxis=list(title="Distance from mouth of Potomac (miles)",autorange="reversed", zeroline=F, showgrid=F))%>%
  layout(yaxis=list(title="Depth (ft)",autorange="reversed", zeroline=F, showgrid=F)) %>%
  layout(title = 'Potomac River Main Channel Cross Section')
potomacchannelplotly

#saveWidget(as_widget(potomacchannelplotly), paste(here("AppFigures"),"/PotomacChannel",monthdate,thisyear,".html", sep=""))

####################################################################################

#Stacked bar plots 

################Whole Bay

#calculate summary for entire water column to update the historical means:
historicbay_wholebay_wholecolumnsummary <- historicbaydata %>%
  group_by(year, Habitat) %>%
  summarize(volume = sum(volume_m, na.rm=TRUE)/1e+9) %>%
  mutate(percent = round(volume/sum(volume)*100, 2))

#making our summary sheets for pie charts
#first we sort suitability
historicbaydatasummary <- historicbaydata %>%
  group_by(uniqueID) %>%
  filter(Sdepth == max(Sdepth, na.rm=T)) %>%
  group_by(year, Habitat) %>%
  summarize(volume = sum(volume_m, na.rm=TRUE)/1e+9) %>%
  mutate(percent = round(volume/sum(volume)*100, 2))

#code the habitat suitability order
historicbaydatasummary$Habitat <- factor(historicbaydatasummary$Habitat, levels = c("Suitable", "Tolerable", "Marginal", "Unsuitable"))

#write out for the app
fwrite(historicbaydatasummary, file = here("Striped-Bass-Habitat-Suitability", paste(monthname, thisyear, "historicbaydatasummary.csv", sep="")), row.names=FALSE)

#plotly
historicbaydata_wholebay_plot <- 
  plot_ly(historicbaydatasummary, x = ~year, y = ~percent, color = ~Habitat, colors = suitability_colors,
          type = 'bar') %>%
  layout(title = 'Whole Bay Suitability for the Previous Ten Years',
         yaxis = list(title = 'Percent of Habitat'),
         barmode = 'stack',
         annotations = list(x = 0.5, y = -0.1, text = "Data not collected in 2020 due to Covid-19 Pandemic.", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=12)))
historicbaydata_wholebay_plot

#saveWidget(as_widget(historicbaydata_wholebay_plot), paste(here("AppFigures"),"/LastTenYears_WholeBay",monthdate,thisyear,".html", sep=""))

##############Fishing hotspots:

#calculate summary for entire water column to update the historical means:
historicbay_fishingareas_wholecolumnsummary <- historicbaydata_fishingareas %>%
  group_by(year, Habitat) %>%
  summarize(volume = sum(volume_m, na.rm=TRUE)/1e+9) %>%
  mutate(percent = round(volume/sum(volume)*100, 2))

#bottoms for the plot
#again, first we sort suitability
historicbaydata_fishingareas_summary <- historicbaydata_fishingareas %>%
  group_by(uniqueID) %>%
  filter(Sdepth == max(Sdepth, na.rm=T)) %>%
  group_by(year, Habitat) %>%
  summarize(volume = sum(volume_m, na.rm=TRUE)/1e+9) %>%
  mutate(percent = round(volume/sum(volume)*100, 2))

#code the habitat suitability order
historicbaydata_fishingareas_summary$Habitat <- factor(historicbaydata_fishingareas_summary$Habitat, levels = c("Suitable", "Tolerable", "Marginal", "Unsuitable"))

fwrite(historicbaydata_fishingareas_summary, file = here("Striped-Bass-Habitat-Suitability", paste(monthname, thisyear, "historicbaydatafishingareassummary.csv", sep="")), row.names=FALSE)

#plotly
historicbaydata_fishingareas_plot <- 
  plot_ly(historicbaydata_fishingareas_summary, x = ~year, y = ~percent, color = ~Habitat, colors = suitability_colors,
          type = 'bar') %>%
  layout(title = 'Fishing Hot Spot Suitability for the Previous Ten Years',
         yaxis = list(title = 'Percent of Habitat'),
         barmode = 'stack',
         annotations = list(x = 0.5, y = -0.1, text = "Data not collected in 2020 due to Covid-19 Pandemic.", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=12)))
historicbaydata_fishingareas_plot

#saveWidget(as_widget(historicbaydata_fishingareas_plot), paste(here("AppFigures"),"/LastTenYears_FishingAreas",monthdate,thisyear,".html", sep=""))

#################################################################################

#Historic Percent Suitable Volume

#note: make sure to update the csvs of percentsuitable_hotspots_currentyear and percentsuitable_wholebay_currentyear
#by taking the percent suitable volume of the current year as calculated in either the pie chart or stacked bar plot

#at year-rollover, calculate new min/max and new average -- use formula m_n = m_n-1 + (a_n - m_n-1)/n
#where m is avg, a is new value (outlined here: https://math.stackexchange.com/questions/106700/incremental-averaging)
#and n is calculated from 1984 (as of 2026, currently 42)

###########Fishing Hot Spots:

#historic suitability -- based on prev years, no modification needed
historicalmeans_hotspots<-read_csv("historicalmeans85-24_hotspots.csv")

#this: you modify the csv by inserting this month's percent suitable in excel
#once you've done that, you can load it in.
percentsuitable_hs <- read_csv("percentsuitable_hotspots_currentyear.csv") 

#now we got to organize these a little bit:
historicalmeans_hs <- full_join(historicalmeans_hotspots, percentsuitable_hs, by='months')
rm(historicalmeans_hotspots, percentsuitable_hs)
historicalmeans_hs$monthseq <- as.numeric(rownames(historicalmeans_hs))
names(historicalmeans_hs)[names(historicalmeans_hs) == "meandatahs"] <- "historic"
names(historicalmeans_hs)[names(historicalmeans_hs) == "percentsuitablehs"] <- "currentyear"
historicalmeans_hs <- gather(historicalmeans_hs, key="timekey", value="meansuitability", 4:5)
labels <- historicalmeans_hs$months
breaks <- historicalmeans_hs$monthseq

fwrite(historicalmeans_hs, file = here("Striped-Bass-Habitat-Suitability", paste(monthname, thisyear, "historicalmeans_hs.csv", sep="")), row.names=FALSE)

historicmeans_hs_plot <-ggplot(historicalmeans_hs, aes(x=as.factor(monthseq)))+
  geom_ribbon(mapping=aes(x=monthseq, ymin=mindatahs, ymax=maxdatahs, fill='ribbon'), alpha=0.1)+
  geom_point(mapping=aes(y=meansuitability, color=timekey))+
  geom_line(mapping=aes(x=monthseq, y=meansuitability, color=timekey))+
  theme_classic()+
  scale_x_discrete(breaks=c(breaks), labels=c(labels))+
  scale_color_manual(name="Suitability Dataset", values=c("dodgerblue", "darkgreen"), labels=c("Current Year", "Historic Mean (1985-22)"))+
  scale_fill_manual(name="Suitability Dataset", values=c("darkgreen"), labels=c("Historic Range (1985-22)"))+
  xlab("Month")+
  ylab("Suitable Percent of Habitat")+
  theme(text=element_text(size=14),
        legend.position = c(0.15, 0.2),
        legend.text=element_text(size=10),
        legend.title=element_text(size=11),
        axis.text.x=element_text(angle=45, hjust=1))
historicmeans_hs_plot

#ggsave(paste(monthname, thisyear, 'historicmeans_hs_plot.png', sep=""), path=here("AppFigures"), width = 10, height = 6)

#######Entire Bay:

#historic suitability -- based on prev years, no modification needed
historicalmeans_wholebay<-read_csv("historicalmeans85-24_wholebay.csv")

#this: you modify the csv by inserting this month's percent suitable in excel
#once you've done that, you can load it in.
percentsuitable_wb <- read_csv("percentsuitable_wholebay_currentyear.csv") 

#now we got to organize these a little bit:
historicalmeans_wb <- full_join(historicalmeans_wholebay, percentsuitable_wb, by='months')
rm(historicalmeans_wholebay, percentsuitable_wb)
historicalmeans_wb$monthseq <- as.numeric(rownames(historicalmeans_wb))
names(historicalmeans_wb)[names(historicalmeans_wb) == "meandatawb"] <- "historic"
names(historicalmeans_wb)[names(historicalmeans_wb) == "percentsuitablewb"] <- "currentyear"
historicalmeans_wb <- gather(historicalmeans_wb, key="timekey", value="meansuitability", 4:5)
labels <- historicalmeans_wb$months
breaks <- historicalmeans_wb$monthseq

fwrite(historicalmeans_wb, file = here("Striped-Bass-Habitat-Suitability", paste(monthname, thisyear, "historicalmeans_wb.csv", sep="")), row.names=FALSE)

historicmeans_wb_plot <-ggplot(historicalmeans_wb, aes(x=as.factor(monthseq)))+
  geom_ribbon(mapping=aes(x=monthseq, ymin=mindatawb, ymax=maxdatawb, fill='ribbon'), alpha=0.1)+
  geom_point(mapping=aes(y=meansuitability, color=timekey))+
  geom_line(mapping=aes(x=monthseq, y=meansuitability, color=timekey))+
  theme_classic()+
  scale_x_discrete(breaks=c(breaks), labels=c(labels))+
  scale_color_manual(name="Suitability Dataset", values=c("dodgerblue", "darkgreen"), labels=c("Current Year", "Historic Mean (1985-22)"))+
  scale_fill_manual(name="Suitability Dataset", values=c("darkgreen"), labels=c("Historic Range (1985-22)"))+
  xlab("Month")+
  ylab("Suitable Percent of Habitat")+
  theme(text=element_text(size=14),
        legend.position=c(0.15, 0.2),
        legend.text=element_text(size=10),
        legend.title=element_text(size=11),
        axis.text.x=element_text(angle=45, hjust=1))
historicmeans_wb_plot

#ggsave(paste(monthname, thisyear, 'historicmeans_hs_plot.png', sep=""), path=here("AppFigures"), width = 10, height = 6)


