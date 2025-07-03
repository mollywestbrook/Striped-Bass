# Striped Bass Habitat MW

library(here)
library(tidyverse)
library(data.table)
library(leaflet)
library(sf)
library(sp)

cruisedata_sept24 <- read_csv(here("BAY873.csv"), col_names = T)
cruisedata_july22 <- read_csv(here("BAY821.csv"), col_names = T)
cruisedata_july23 <- read_csv(here("BAY844.csv"), col_names = T)

#plotting the shape data for the fishing hotspots, just to visualize
ggplot()+
  geom_sf(data=shape)+
  coord_sf(crs = st_crs(3347))

#step one: assembling the 10 year data for analysis

DO2012 <- read_csv('DO_2012_07_26_26_full.txt', col_names=T)

#going back to DO2012, visualizing the coordinates to begin understanding the overlap
#first, we need to transform the stations back into polygons so I can plot them together I think

DO2012_sf <- st_as_sf(DO2012, coords = c('UTM_Y', 'UTM_X'), crs=3347)

#now, we can plot:
ggplot()+
  geom_sf(data=DO2012_sf)+
  geom_sf(data=shape)+
  coord_sf(crs = st_crs(3347))

##just trying to make sense of the cruise data
ggplot(mddatathiscruise, aes(x=UTMX, y=UTMY, color=DO))+
  geom_point(size=1)+
  theme_bw()

#why don't we start messing around with a leaflet map?
cruisedata <- mddatathiscruise

coordinates(cruisedata)= ~UTMY + UTMX
proj4string(cruisedata) <- CRS("+proj=utm +zone=10")
cruisedatadd <- spTransform(cruisedata, CRS("+proj=longlat")) 
cruisedatadd <- as.data.frame(cruisedatadd)
names(cruisedatadd)[8] <- 'lng'
names(cruisedatadd)[9] <- 'lat'

#transform to dd
mddatathiscruise <- mddatathiscruise %>%
  mutate(DDX = spTransform(UTMX, CRS("+proj=longlat +datum=WGS84"))) %>%
  mutate(DDY = spTransform(UTMY, CRS("+proj=longlat +datum=WGS84")))

#this is a map of the fishing hotspots. This could be a fun layer to have in the end map
leaflet() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  setView(lng = -76.07556, lat = 39.54306, zoom = 14) %>%
  addPolygons(data = sfc, color = "red", stroke = 0.2, opacity = 0.8)

#transform UTMS into DD
#save as an sf
shape_sf <- st_as_sf(x = shape.utm,                         
                  coords = c("UTMY", "UTMX"),
                  crs = "+proj=utm +zone=10")
#project into DD
sfc = st_transform(shape_sf, crs = "+proj=longlat +datum=WGS84")

#Trying to fix the leaflet map

#just as a tmp, I'm going to select like 100 random points just to make sure this issue isn't something else

wholebaydata_randomslice <- wholebaydata.dd_surface %>%
  slice_sample(n = 100)

leaflet() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  setView(lng = -76.3, lat = 39.2, zoom = 9) %>%
  addCircles(data = wholebaydata_randomslice, color = ~color)

#####################################################################################

FishingAreas <- leaflet() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  setView(lng = -76.3, lat = 39.2, zoom = 9) %>%
  addPolygons(data = fishingareapolygons.dd, color = "#8373e2", stroke = 0.2, opacity = 0.8,
              label = fishingareapolygons.dd$name)
FishingAreas

saveWidget(as_widget(FishingAreas), paste(here("App Figures"),"/Leaflet_FishingHotspotlocations",monthdate,thisyear,".html", sep=""))

fishingareacoords.dd_surface <- fishingareacoords.dd %>%
  filter(Sdepth == 0) %>%
  arrange(OBJECTID)

FishingHotspotSuitability <- leaflet() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  setView(lng = -76.3, lat = 39.2, zoom = 9) %>%
  addCircles(data = fishingareacoords.dd_surface, color = ~color,
             label = paste(fishingareacoords.dd_surface$name, fishingareacoords.dd_surface$habitat, sep=", "))
FishingHotspotSuitability

saveWidget(as_widget(FishingHotspotSuitability), paste(here("App Figures"),"/Leaflet_FishingHotspots",monthdate,thisyear,".html", sep=""))

#this one is too slow, will probably have to do this in python

wholebaydata.dd_surface <- wholebaydata.dd %>%
  filter(Sdepth == 0)

leaflet() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  setView(lng = -76.3, lat = 39.2, zoom = 9) %>%
  addCircles(data = wholebaydata.dd_surface, color = ~color)

#now that we've got a functional map, I want to ensure we can click through the layers. 

multilayermap_try1 <- leaflet() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  setView(lng = -76.3, lat = 39.2, zoom = 9) %>%
  addPolygons(
    data = fishingareapolygons.dd, color = "#8373e2", stroke = 0.2, opacity = 0.8,
    label = fishingareapolygons.dd$name, group = "Fishing Areas") %>%
  addCircles(
    data = fishingareacoords.dd_surface, color = ~color, group = "Fishing Area Suitability",
    label = paste(fishingareacoords.dd_surface$name, fishingareacoords.dd_surface$habitat, sep=", ")) %>%
  addCircles(data = wholebaydata.dd_surface, color = ~color, group = "Whole Bay Suitability") %>%
  addLayersControl(
    overlayGroups = c("Fishing Areas", "Fishing Area Suitability", "Whole Bay Suitability"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("Fishing Areas", "Whole Bay Suitability"))
multilayermap_try1

leaflet() %>%
  addTiles() %>%
  addPolygons(data = polygon, group = "Polygons", fillColor = "gray", weight = 1, fillOpacity = 0.4) %>%
  addCircleMarkers(data = points_a, color = ~color, group = "Points A", radius = 5, label = ~color) %>%
  addCircleMarkers(data = points_b, color = ~color, group = "Points B", radius = 5, label = ~color) %>%
  addLayersControl(
    overlayGroups = c("Polygons", "Points A", "Points B"),
    options = layersControlOptions(collapsed = FALSE)
  )

##################################################################################

#Mark wanted to clean up the cross-section figures...hm

plot_ly()%>%
  add_trace(x=crosssectionmain$UTMX,y=crosssectionmain$UTMY,
            hoverinfo="none",
            type='scatter',mode='markers',
            marker=list(color=mainchanneldata$color))%>%
  layout(xaxis=list(title="Distance from mouth of Bay (miles)",autorange="reversed"))%>%
  layout(yaxis=list(title="Depth (ft)",autorange="reversed"))

##################################################################################

#issue with prev year's data:

tmp <- read_csv(here("DO_2015_05.txt"), col_names = T)

##################################################################################

#Cleaning up the historical averages figure first. 

#hot spots
percentsuitablehs<-c(100,100,100,100,88.25,78.68,87.54,12.92,48.79
                     ,81.06,75.02,87.92,NA,NA,NA)
months<-c("January","February","March","April","May","Early June","Late June","Early July",
          "Late July","Early August","Late August","September","October","November","December")

#This is hard-coded, but ideally I want to take this from a file:
maxdatahs<-c(100,100,100,100,99.9,91.8,89.1,88.6,85.5,86.3,91.4,97.9,100,100,100)
mindatahs<-c(98.6,100,99.9,96.3,71.6,67.9,59.5,11.7,2.52,4.31,26.2,77.1,88.4,97,95.3)
meandatahs<-c(99.9,100,100,99.4,90.8,78.3,76.1,64.5,56.9,61,72.1,87.9,97,99.8,99.8)

historicalmeans_hotspots <- data.frame(months, maxdatahs, mindatahs, meandatahs)

fwrite(historicalmeans_hotspots, "historicalmeans85-22_hotspots.csv", row.names=FALSE)

#whole bay
percentsuitablewb<-c(100,99.98915472,100,99.96483655,94.82371189,89.67849548,92.61107918,59.5016,NA,NA,NA,NA,NA,NA,NA)
###mean data
maxdatawb<-c(100.00000,100.00000,100.00000,100.00000,99.94289, 91.80321,89.09262,88.55161,85.45735,86.25828,91.38191,97.89752,100.00000,100.00000,100.00000)
mindatawb<-c(98.627215,99.964205,99.853254,96.259686,71.554099,67.889627,59.486843,11.739094,2.516686,4.312487,26.203638,77.126050,88.448145,97.011848,95.315226)
meandatawb<-c(99.93728,99.99834,99.99126,99.44283,90.78836,78.31424,76.06932,64.54837,56.85114,60.98641,72.08090,87.94222,97.02661,99.83884,99.77699)

historicalmeans_wholebay <- data.frame(months, maxdatawb, mindatawb, meandatawb)

fwrite(historicalmeans_wholebay, "historicalmeans85-22_wholebay.csv", row.names=FALSE)


