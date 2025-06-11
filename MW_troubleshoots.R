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
