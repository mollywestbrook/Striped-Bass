## Redoing the cross sections

#Current version of the code:
#bring in cross section file:
crosssectionmain<-read_csv("mainchannelpointsCLEAN.csv")
crosssectionmain<-crosssectionmain[c("UTMX","UTMY")]
crosssectionmain$keep<-"YES"

#some extra visualization to make sense of this:

ggplot(crosssectionmain, aes(x=UTMX, y=UTMY))+
  geom_point(size=1)+
  theme_classic()

#convert full cruise to a plotly to identify some filter boundaries
#I think the best way to do this is to recalculate the bounds of the xsection

plot_ly()%>%
  add_trace(x=mddatathiscruise$UTMX,y=mddatathiscruise$UTMY
            ,type='scatter',mode='markers')%>%
  layout(
    xaxis = list(range=c(300000, 450000)),
    yaxis = list(range=c(4150000, 4400000))
  )

#series of filters to narrow into the center channel

channelfilter <- mddatathiscruise %>%
  filter(365000 < UTMX ) %>% #removes potomac and patuxent
  filter(UTMX < 415000) %>% #removes most of the eastern tribs
  filter(!(UTMX < 379000 & UTMY < 4254000)) %>% #removes remaining slices of potomac and patuxent
  filter(!(UTMX > 384000 & UTMY < 4312000 & UTMY > 4259000)) %>% #removes Nanticoke
  filter(!(UTMX > 390000 & UTMY < 4336000 & UTMY > 4311000)) %>% #removes most of the Choptank
  filter(!(UTMX > 407400 & UTMY < 4344800 & UTMY > 4335500)) %>% #removes rest of Choptank
  filter(!(UTMY > 4298500 & UTMX < 371000)) %>% #Patapsco slice 1
  filter(!(UTMX < 374000 & UTMY > 4310000)) %>% #Patapsco slice 2 #Patapsco Slice 3
  filter(!(UTMY > 4321750 & UTMX < 376750)) %>% #Patapsco Slice 3
  filter(!UTMX > 410000) %>% #Remove NE river and the rest of Tangier Sound
  filter(!(UTMY > 4349500 & UTMX < 389000)) %>% #Patapsco Slice 4
  filter(!(UTMX > 392100 & UTMY < 4250500 & UTMY > 4238800)) %>%
  filter(!(UTMX > 396500 & UTMY < 4239000 & UTMY > 4232500)) %>%
  filter(!(UTMX > 407000 & UTMY < 4232500)) %>%
  filter(!(UTMY > 4353000 & UTMX < 395000 & UTMX > 389000)) %>% #from here on out, couple of point removals
  filter(!(UTMX < 372000 & UTMY < 4259000)) %>%
  filter(!(UTMX > 386000 & UTMY > 4255000 & UTMY < 4266500)) %>%
  filter(!(UTMX < 370000 & UTMY > 4293000 & UTMY < 4299000)) %>%
  filter(!(UTMX > 405000 & UTMY > 4352000 & UTMY < 4354000)) %>%
  filter(!(UTMX > 382000 & UTMY < 4316000 & UTMY > 4291500))

plot_ly()%>%
  add_trace(x=channelfilter$UTMX,y=channelfilter$UTMY
            ,type='scatter',mode='markers') %>%
  layout(
    xaxis = list(range=c(300000, 450000)),
    yaxis = list(range=c(4150000, 4400000))
  )

##OKay. Now, for each point on the Y axis, we want the middle point along the x axis.

calculatebaycenter <- function(ypoints) {
  ypointrange <- ypoints %>%
    summarize(xmin = min(UTMX, na.rm=T),
              xmax = max(UTMX, na.rm=T),
              xrange = xmax - xmin,
              midpoint = xmin + xrange/2,
              midpointcheck = xmax - xrange/2,
              UTMY = unique(UTMY))
}

UTMYlist <- split(channelfilter, channelfilter$UTMY)
midpoints <- lapply(UTMYlist, calculatebaycenter) %>% bind_rows()
midpoints_modulo <- midpoints %>%
  filter(UTMY %% 1000 == 0)

#final check:

ggplot(midpoints, aes(x=midpoint, y=UTMY))+
  geom_point(size=1)+
  theme_classic()+
  coord_cartesian(xlim=c(300000, 440000), ylim=c(4150000, 4400000))

ggplot(midpoints_modulo, aes(x=midpoint, y=UTMY))+
  geom_point(size=1)+
  theme_classic()+
  coord_cartesian(xlim=c(300000, 440000), ylim=c(4150000, 4400000))

ggplot(mainchanneldata, aes(x=UTMX, y=UTMY))+
  geom_point(size=1)+
  theme_classic()+
  coord_cartesian(xlim=c(300000, 440000), ylim=c(4150000, 4400000))

#let's reorganize to be from the head of the bay, to make things easier

bayhead <- max(midpoints$UTMY)
names(midpoints)[names(midpoints)=="midpoint"] <- "UTMX"
mainchanneldata <- left_join(midpoints, mddatathiscruise, by=c("UTMX", "UTMY"))

mainchanneldata <- mainchanneldata %>%
  arrange(UTMY, Sdepth)

ggplot(mainchanneldata, aes(x=UTMY, y=Sdepth, color=color))+
  geom_point(size=1)+
  theme_classic()

plot_ly()%>%
  add_trace(x=mainchanneldata$UTMY, y=mainchanneldata$Sdepth, type='scatter', mode='markers', marker = list(color=mainchanneldata$color))

mainchanneldata <- mainchanneldata %>%
  mutate(yfromhead = bayhead - UTMY) %>%
  mutate(milesfromhead = yfromhead*0.000621371) %>%
  mutate(habitat = case_when(
    Wtemp>marginaltemp | DO<marginalDO ~ "Unsuitable",
    Wtemp<=marginaltemp & Wtemp>tolerabletemp | DO>=marginalDO & DO<tolerableDO ~ "Marginal",
    Wtemp<=tolerabletemp & Wtemp>suitabletemp | DO>=tolerableDO & DO<suitableDO ~ "Tolerable",
    Wtemp>=suitabletemp | DO>=suitableDO ~ "Suitable",
    TRUE ~ "NA")) %>%
  mutate(color = case_when(
    habitat == "Unsuitable" ~ "black",
    habitat == "Marginal" ~ "orange",
    habitat == "Tolerable" ~ "yellow",
    habitat == "Suitable" ~ "dodgerblue",
    habitat == NA ~ "grey90"))

mainchannelplotly<-plot_ly()%>%
  config(displayModeBar=FALSE, modeBarButtonsToRemove = c("autoScale2d","hoverCompareCartesian","toggleSpikelines","select2d","lasso2d")) %>%
  add_trace(x=mainchanneldata$milesfromhead,y=mainchanneldata$Sdepth
            ,hoverinfo="none"
            ,type='scatter',mode='markers'
            ,marker=list(color=mainchanneldata$color))%>%
  # add_annotations(x=citylabels$distfrommouth, #annotations for city labels
  #                 y=0,
  #                 text=citylabels$name, 
  #                 xref = "x",
  #                 yref = "y",
  #                 showarrow = T,
  #                 arrowhead = 0,
  #                 arrowsize = 0.5,
  #                 ax = 20,
  #                 ay = -30,
  #                 textposition="top" )%>%
  layout(xaxis=list(title="Distance from head of Bay (miles)"))%>%
  layout(yaxis=list(title="Depth (ft)",autorange="reversed"))
mainchannelplotly

# #defining parameters for the center channel
# xrange<-c(4410000,4065000)
# xrangemiles<-xrange*0.000621371
# xrangemiles<-c(2750,2526)
# xrangemiless<-xrangemiles-min(xrangemiles)
# xrange<-xrangemiless/0.000621371
# xrangeoriginal<-c(4410000,4065000)

names(mddatathiscruise)[names(mddatathiscruise) == "lat"] <- "UTMX"
names(mddatathiscruise)[names(mddatathiscruise) == "long"] <- "UTMY"
mainchanneldata<-merge(mddatathiscruise,crosssectionmain,by=c("UTMX","UTMY"),allow.cartesian=TRUE)
mainchanneldata<-unique(mainchanneldata)

mainchanneldata$milesX<-mainchanneldata$UTMX*0.000621371
mainchanneldata$milesY<-mainchanneldata$UTMY*0.000621371
mainchanneldata$milesY<-mainchanneldata$milesY-min(xrangemiles)

mainchanneldata <- mainchanneldata %>%
  mutate(color = case_when(
    habitat == "Unsuitable" ~ "black",
    habitat == "Marginal" ~ "orange",
    habitat == "Tolerable" ~ "yellow",
    habitat == "Suitable" ~ "dodgerblue"))

#write out for the app
fwrite(mainchanneldata, file = here("Striped-Bass-Habitat-Suitability", paste(monthname, thisyear, "mainchanneldata.csv", sep="")), row.names=FALSE)

#hand coding in the city lables...let's see if it works:
citylabels<-NULL
citylabels$x<-c(355000,323385)
citylabels$y<-c(4350100,4330000)
citylabels$z<-c(5,5)
citylabels$name<-c("Baltimore","Washington, D.C.")
citylabels<-as.data.frame(citylabels)
minmiles <- min(xrangemiles)
citylabels <- citylabels %>%
  mutate(milesx = x*0.000621371,
         milesy = y*0.000621371) %>%
  mutate(distfrommouth = milesy - minmiles)

mainchannelplotly<-plot_ly()%>%
  config(displayModeBar=FALSE, modeBarButtonsToRemove = c("autoScale2d","hoverCompareCartesian","toggleSpikelines","select2d","lasso2d")) %>%
  add_trace(x=mainchanneldata$milesY,y=mainchanneldata$Sdepth
            ,hoverinfo="none"
            ,type='scatter',mode='markers'
            ,marker=list(color=mainchanneldata$color))%>%
  add_annotations(x=citylabels$distfrommouth, #annotations for city labels
                  y=0,
                  text=citylabels$name, 
                  xref = "x",
                  yref = "y",
                  showarrow = T,
                  arrowhead = 0,
                  arrowsize = 0.5,
                  ax = 20,
                  ay = -30,
                  textposition="top" )%>%
  layout(xaxis=list(title="Distance from mouth of Bay (miles)",autorange="reversed"))%>%
  layout(yaxis=list(title="Depth (ft)",autorange="reversed"))
mainchannelplotly

saveWidget(as_widget(mainchannelplotly), paste(here("App Figures"),"/MainChannel",monthdate,thisyear,".html", sep=""))
