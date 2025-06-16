library(plotly)
library(dplyr)

Sys.setenv("plotly_username" = "agkeppel")
Sys.setenv("plotly_api_key" = "Qg4EkuDiOww3xZVM8Tpu")

###import cruisedata
setwd("D:/Desktop/Vol3d/")

###use this if necessary to figure out which old file to use
# list.files("C:/Users/AKeppel/Desktop/Vol3d/Second July 2022")

###UPDATE HERE with cruise name
rawcruisedata<-read.csv("BAY844.csv")

###check that these are not including extraneous va trib dates
startdate<-min(rawcruisedata$Date)
enddate<-max(rawcruisedata$Date)

###CHECK HERE identify month of interest for calculation in case month starts/ends in other month
# thismonth<-"09"
thismonth<-substr(startdate,6,7)
thiscruise<-ifelse(thismonth=="01","January",(ifelse(thismonth=="02","February",(ifelse(thismonth=="03","March",(ifelse(thismonth=="04","April",(ifelse(thismonth=="05","May",(ifelse(thismonth=="06","June",(ifelse(thismonth=="07","July",(ifelse(thismonth=="08","August",(ifelse(thismonth=="09","September",(ifelse(thismonth=="10","October",(ifelse(thismonth=="11","November","December")))))))))))))))))))))

###UPDATE HERE 
# earlyorlate<-"_EARLY"
# earlyorlate<-"_LATE"
earlyorlate<-""

###CHECK HERE identify year of interest for calculation
# thisyear<-"2024"
thisyear<-substr(startdate,1,4)

###sets up list of files to import
# DOidpattern<-glob2rx(paste("DO*",thisyear,thismonth,"full_",earlyorlate,".txt",sep=""))
DOidpattern<-glob2rx(paste("DO*",thisyear,"_",thismonth,"full",earlyorlate,".txt",sep="")) #this line is broken, MW brought in manually below
DOdata<-list.files(pattern=DOidpattern)
DOdata<-read.csv(DOdata)

DOdata <- read_csv("DO_2023_07full.txt", col_names=T)

Wtempidpattern<-glob2rx(paste("WTEMP*",thisyear,"_",thismonth,"full",earlyorlate,".txt",sep="")) #see above note
Wtempdata<-list.files(pattern=Wtempidpattern)
Wtempdata<-read.csv(Wtempdata)

Wtempdata <- read_csv("WTEMP_2023_07full.txt", col_names=T)

DO<-DOdata
DO[DO==-9] <- NA
DO$volume_m<-DO$EW_dim*DO$NS_dim*DO$Vert_dim
DOtrimmed<-DO[,-c(58,57,56,55,54,53,7,6,5,4,3,2,1)]

###create empty dataset to fill with transposed data
DOdatatransposed<-NULL

###for loop to break up data frame and transpose to vertical
for (o in 1:nrow(DO)){ #note: this for loop takes minutes to run. These two loops slow the code down significantly.
  tryCatch({
    rowout<-DOtrimmed[o,]
    colout<-t(rowout)
    colout<-colout[!is.na(colout)]
    Sdepth<-seq(0,100,0.5)
    Sdepth<-head(Sdepth,length(colout))
    data<-cbind(colout,Sdepth)
    data<-as.data.frame(data)
    UTMX<-DO[o,1]
    UTMY<-DO[o,2]
    volume_m<-DO[o,58]
    Segment<-DO[o,3]
    data$UTMX<-UTMX
    data$UTMY<-UTMY
    data$volume_m<-volume_m
    data$Segment<-Segment
    DOdatatransposed<-rbind(DOdatatransposed,data)
  },error=function(e){})
}

wtemp<-Wtempdata
wtemp[wtemp==-9] <- NA
wtemp$volume_m<-wtemp$EW_dim*wtemp$NS_dim*wtemp$Vert_dim
wtemptrimmed<-wtemp[,-c(58,57,56,55,54,53,7,6,5,4,3,2,1)]

###create empty dataset to fill with transposed data
wtempdatatransposed<-NULL

###for loop to break up data frame and transpose to vertical #this one took 2 minutes
for (p in 1:nrow(wtemp)){
  tryCatch({
    rowout<-wtemptrimmed[p,]
    colout<-t(rowout)
    colout<-colout[!is.na(colout)]
    Sdepth<-seq(0,100,0.5)
    Sdepth<-head(Sdepth,length(colout))
    data<-cbind(colout,Sdepth)
    data<-as.data.frame(data)
    UTMX<-wtemp[p,1]
    UTMY<-wtemp[p,2]
    volume_m<-wtemp[p,58]
    Segment<-wtemp[p,3]
    data$UTMX<-UTMX
    data$UTMY<-UTMY
    data$volume_m<-volume_m
    data$Segment<-Segment
    wtempdatatransposed<-rbind(wtempdatatransposed,data)
  },error=function(e){})
}

names(DOdatatransposed)[names(DOdatatransposed) == "colout"] <- "DO"
names(DOdatatransposed) <- c("DO", "Sdepth","UTMX","UTMY","volume_m","Segment") #adding this in so I can do a more efficient join
names(wtempdatatransposed)[names(wtempdatatransposed) == "colout"] <- "Wtemp"

# wholebaydata<-merge(pH,DO,by=c("Segment","UTMX","UTMY","Sdepth","volume_m"),all=TRUE)
wholebaydata<-merge(DOdatatransposed,wtempdatatransposed,by=c("Segment","UTMX","UTMY","Sdepth","volume_m"),all=TRUE) #this merge took so long I had to abort it (>15min)
# wholebaydata<-merge(data,salinity,by=c("Segment","UTMX","UTMY","Sdepth","volume_m"),all=TRUE)

#because the temp and DO dataframes are the same lenght, this is a much more efficient way of doing the above code
#if this is not always the case there are a few other more efficient merges than merge()
wholebaydata <- data.frame(DOdatatransposed$Segment, 
                              DOdatatransposed$UTM_X, 
                              DOdatatransposed$UTM_Y, 
                              DOdatatransposed$Sdepth, 
                              DOdatatransposed$volume_m, 
                              DOdatatransposed$DO, 
                              wtempdatatransposed$wtemp)
names(wholebaydata) <- c("Segment", "UTMX","UTMY", "Sdepth","volume_m","DO", "Wtemp")


wholebaydata$uniqueID<-paste(wholebaydata$UTMX,wholebaydata$UTMY)
uniqueIDs<-unique(wholebaydata$uniqueID)

wholebaydata$Sdepthm<-wholebaydata$Sdepth
wholebaydata$Sdepth<-wholebaydata$Sdepthm*3.28084  

### set parameters for habitat levels###
suitabletemp<-82.4
tolerabletemp<-84.2
marginaltemp<-86
unsuitabletemp<-86
suitableDO<-4
tolerableDO<-3
marginalDO<-2
unsuitableDO<-2

###functions to calculate sums
unsuitable<- function(x) {
  data<-as.data.frame(x)
  data<-data[which(data$habitat==0),]
  unsuitable<-round((sum(data$volume_m)),digits = 0)
  return(unsuitable)
}

marginal<-function(x){
  data<-as.data.frame(x)
  data<-data[which(data$habitat==1),]
  marginal<-round((sum(data$volume_m)),digits = 0)
  return(marginal)
}

tolerable<-function(x){
  data<-as.data.frame(x)
  data<-data[which(data$habitat==2),]
  tolerable<-round(sum(data$volume_m),digits = 0)
  return(tolerable)
}

suitable<-function(x){
  data<-as.data.frame(x)
  data<-data[which(data$habitat==3),]
  suitable<-round(sum(data$volume_m),digits = 0)
  return(suitable)
}

mdsegments<-c("BACOH","BIGMH","BOHOH"
              ,"BSHOH","C&DOH","CB1TF","CB2OH","CB3MH","CB4MH","CB5MH_MD","CB5MH"
              ,"CHOMH1","CHOMH2","CHOOH","CHOTF","CHSMH","CHSOH","CHSTF"
              ,"EASMH","ELKOH","FSBMH","GUNOH","HNGMH","LCHMH","MAGMH"
              ,"MANMH","MATTF","MIDOH","NANMH","NANOH","NANTF","NORTF"
              ,"PATMH","PAXMH","PAXOH","PAXTF","PISTF","POCMH","POCOH"
              ,"POCTF","POCMH_MD","POTMH","POTOH","POTTF","RHDMH","SASOH","SEVMH"
              ,"SOUMH","TANMH","TANMH_MD","WICMH","WSTMH"
)
    
mddatathiscruise<-wholebaydata[wholebaydata$Segment %in% mdsegments,]

###adding a section to do prime/non-prime areas

library(sf)

###importing MD finfish shapefile
shape<-st_read("C:/Users/mwestbrook/Desktop/Projects/Striped Bass") #edited to fetch from my folder

###reprojecting fisheries polygons into UTM
shape.utm<-st_transform(shape,crs=32618)

###lighthouselump appears to be only one not overlapping an interpolated point, trying to move slightly
shape.utm$geometry[[11]]<-shape.utm$geometry[[11]]+st_point(c(-300,226))
###still not working, for now dropping lighthouse lump and loon hill
shape.utm<-shape.utm[-44,]
shape.utm<-shape.utm[-11,]

####turning data into points file and then doing some sort of join
point.df <- data.frame("long"=wholebaydata$UTMX,"lat"=wholebaydata$UTMY)
point.sf <- st_as_sf(point.df, coords = c("long","lat"),crs=32618)

###st_filter chooses only points that are within polygons of fishing areas
###need to loop through and do one by one to keep name info
# nrow(shape.utm)

firstshape<-shape.utm[1,]
firstprimedata<-st_filter(point.sf,firstshape)
firstprimedata$name<-firstshape$name

primedata<-firstprimedata

for (i in 2:nrow(shape.utm)){
  thisshape<-shape.utm[i,]
  thisprimedata<-st_filter(point.sf,thisshape)
  thisprimedata$name<-thisshape$name
  primedata<-rbind(primedata,thisprimedata)
}

###converting from spatial data frame to plotly for the sake of plotting similar to other plots
primedataforplotly<-as.data.frame(primedata)
primedataforplotly$X<-substring(primedataforplotly$geometry,3,nchar(primedataforplotly)) 
primedataforplotly$X<-gsub("\\,.*","",primedataforplotly$X)
primedataforplotly$Y<-gsub(".*,","",primedataforplotly$geometry)
primedataforplotly$Y<-substring(primedataforplotly$Y,1,nchar(primedataforplotly$Y)-1)
primedataforplotly$X<-as.numeric(as.character(primedataforplotly$X))
primedataforplotly$Y<-as.numeric(as.character(primedataforplotly$Y))

primedataforplotly<-primedataforplotly[c("X","Y","name")]
colnames(primedataforplotly)[which(names(primedataforplotly) == "X")] <- "UTMX"
colnames(primedataforplotly)[which(names(primedataforplotly) == "Y")] <- "UTMY"

fishingareadatathiscruise<-merge(wholebaydata,shape.utm,by=c("UTMX","UTMY"))

###background nonsense for plotting
###creating a dataset for geographic labels
riverlabels<-NULL
riverlabels$x<-c(320000,
                 # 435000,
                 423000,
                 # 348000,330000,298000,
                 425000,450183)
riverlabels$y<-c(4275000,
                 # 4090000,
                 4390000,
                 # 4140000,4100000,4195000,
                 4276685,4214363)
riverlabels$z<-c(5,
                 # 5,
                 5,
                 # 5,5,5,
                 5,5)
riverlabels$name<-c("Potomac",
                    # "Atlantic",
                    "Susquehanna",
                    # "York","James","Rappahanock",
                    "Choptank","Pocomoke")

riverlabels<-as.data.frame(riverlabels)
riverlabels$name<-as.character(riverlabels$name)

citylabels<-NULL
citylabels$x<-c(355000,323385)
citylabels$y<-c(4350100,4330000)
citylabels$z<-c(5,5)
citylabels$name<-c("Baltimore","Washington, D.C.")

citylabels<-as.data.frame(citylabels)
citylabels$name<-as.character(citylabels$name)

# paramofinterest<-"Salinity"
# paramofinterest<-"Water Temp"
# paramofinterest<-"DO"
paramofinterest<-"Habitat"

possiblestations<-c("LE5.5-W","WE4.1","WE4.2","WE4.3","WE4.4","CB4.4","CB5.1","CB5.2","CB5.3","CB5.4","CB5.4W","CB5.5","CB6.4","CB7.1","CB7.1N","CB7.1S","CB7.2","CB7.2E","EE3.4","EE3.5","LE2.3","LE5.1","LE5.2","LE5.3","LE5.4","RET5.2","ANA0082","CB3.3C","CB3.3E","CB3.3W","CB4.1C","CB4.1E","CB4.1W","CB4.2C","CB4.2E","CB4.2W","CB4.3C","CB4.3E","CB4.3W","CJB0005","ELD01","ELI2",
                    "LE5.6","LFA01","LFB01","MON0020","POT1184","POT1471","POT1472","POT1595","POT1596","RCM0111","SEN0008","WBB05","WBE1","CB1.1","CB2.1","CB2.2","CB3.1","CB3.2","CB6.1","CB6.2","CB6.3","CB7.3","CB7.3E","EBB01","EBE1","ELE01","LE3.6","LE3.7","SBE2","SBE5","CB7.4","CB7.4N","CB8.1","CB8.1E","LE2.2","ET5.1","ET5.2","MAT0016","MAT0078","PIS0033","PMS10","RET2.1","RET2.2","RET2.4","TF2.1","TF2.2","TF2.3","TF2.4","WT5.1","XFB1986","ET10.1","ET3.1","ET4.1","ET6.1","TF3.1B","TF3.1E","TF3.1F","TF3.2","TF3.2A","WIW0141","XGG8251","CB5.1W","EE1.1","EE2.1","EE2.2","ET4.2","LE1.1","LE1.2","LE1.3","LE1.4","RET1.1","TF1.0","TF1.2","TF1.3","TF1.4","TF1.5","TF1.6","TF1.7","WXT0001","ET1.1","ET2.1","ET2.2","ET2.3","LE3.1","LE3.2","LE3.3","LE3.4","RET3.1","RET3.2","WT1.1","WT2.1","WT3.1","WT4.1","EE3.0","EE3.1","EE3.2","EE3.3","ET6.2","ET7.1","ET8.1","ET9.1","WT6.1","WT7.1","WT8.1","WT8.2","WT8.3","RET4.2","RET5.1A","TF3.3","LE4.1","LE4.2","LE4.3","RET4.1","RET4.3","TF5.6","TF5.5","TF5.5A","TF4.2","TF4.4","BXK0031","MNK0146","POK0087","CCM0069","TRQ0088","TRQ0146","XAK7810","XCI4078","XDJ9007","TF5.2A","TF5.3","TF5.4","TF5.2")

actualstations<-unique(rawcruisedata$Station)
actualstations<-ifelse(actualstations=="PMS 10","PMS10",actualstations)

missedstations<-setdiff(possiblestations,actualstations)

xrange<-c(4410000,4065000)
xrangemiles<-xrange*0.000621371
xrangemiles<-c(2750,2526)
xrangemiless<-xrangemiles-min(xrangemiles)
xrange<-xrangemiless/0.000621371

xrangeoriginal<-c(4410000,4065000)

crosssectionmain<-read_csv("mainchannelpointsCLEAN.csv") #again, modified for me
crosssectionmain<-crosssectionmain[c("UTMX","UTMY")]
crosssectionmain$keep<-"YES"

crosssectionpotomac<-read_csv("potomacmainstem.csv") #again, modified for me
crosssectionpotomac<-crosssectionpotomac[c("UTMX","UTMY")]
crosssectionpotomac$keep<-"YES"

###doing fishing area
samplearea<-"Maryland Fishing Areas"

data<-fishingareadatathiscruise

cruisedata<-data
cruisedata[cruisedata==-9999]<-NA

# minsal<-min(cruisedata$Salinity,na.rm=TRUE)
# maxsal<-max(cruisedata$Salinity,na.rm=TRUE)
minwtemp<-min(cruisedata$Wtemp,na.rm=TRUE)
maxwtemp<-max(cruisedata$Wtemp,na.rm=TRUE)
minDO<-min(cruisedata$DO,na.rm=TRUE)
maxDO<-max(cruisedata$DO,na.rm=TRUE)

data<-subset(cruisedata,DO>=0
             # |Salinity>=0
             |Wtemp>=0)

###habitat calculation
data$habitat<-ifelse(data$Wtemp>marginaltemp | data$DO<marginalDO, 0, 
                     ifelse(data$Wtemp<=marginaltemp &data$Wtemp>tolerabletemp | data$DO>=marginalDO & data$DO<tolerableDO, 1, 
                            ifelse(data$Wtemp<=tolerabletemp &data$Wtemp>suitabletemp | data$DO>=tolerableDO & data$DO<suitableDO, 2,3)))

suitable<-data[data$habitat==3,]
tolerable<-data[data$habitat==2,]
marginal<-data[data$habitat==1,]
unsuitable<-data[data$habitat==0,]

suitablenum<-sum(suitable$volume_m,na.rm=TRUE)/1e+9
tolerablenum<-sum(tolerable$volume_m,na.rm=TRUE)/1e+9
marginalnum<-sum(marginal$volume_m,na.rm=TRUE)/1e+9
unsuitablenum<-sum(unsuitable$volume_m,na.rm=TRUE)/1e+9

thisvolume<-sum(data$volume_m)/1e+9

fishingareasummary<-cbind(suitablenum,tolerablenum,marginalnum,unsuitablenum,thisvolume)

data$paramofinterest<-data$habitat

minvalue<-0
maxvalue<-3

ii<-cut(data$paramofinterest,breaks=seq(minvalue,maxvalue,len=100), include.lowest = TRUE)

### habitat
data$heatcol<-colorRampPalette(c("black","orange","yellow","dodgerblue"))(99)[ii]

library(dplyr)
surfacetest<-data %>% group_by(uniqueID) %>% slice_min(order_by=paramofinterest,with_ties=FALSE)

surfacedata<-unique(surfacetest)

surfacedata$habitatword<-ifelse(surfacedata$habitat==3,"Suitable",
                                ifelse(surfacedata$habitat==2,"Tolerable",
                                       ifelse(surfacedata$habitat==1,"Marginal",
                                              ifelse(surfacedata$habitat==0,"Unsuitable",""))))

write.csv(surfacedata,paste("C:/Users/AKeppel/Desktop/fishingareasurfaceforMark-",earlyorlate," ",thiscruise," ",thisyear,"-",Sys.Date(),".csv",sep=""),row.names = FALSE)

write.csv(surfacedata, 'surfacedata_2023.csv', row.names=F) #for MW

backgroundforsurface<-read.csv("D:/Desktop/Helpful Info/allbaypoints.csv")
backgroundforsurface <- read_csv("allbaypoints.csv") #for MW

backgroundforsurface<-backgroundforsurface[backgroundforsurface$UTMX<=490000&backgroundforsurface$UTMX>=290000,]
backgroundforsurface<-backgroundforsurface[backgroundforsurface$UTMY<=4430000&backgroundforsurface$UTMY>=4160000,]

surfacedataplotly<-plot_ly()%>%
  partial_bundle()%>%
  config(displayModeBar=TRUE, modeBarButtonsToRemove = c("autoScale2d","hoverCompareCartesian","toggleSpikelines","select2d","lasso2d")) %>%
  add_trace(x=backgroundforsurface$UTMY,y=backgroundforsurface$UTMX,hovertext='none',hovertemplate="",hoverinfo="none"
            ,marker=list(color="lightgrey")
  )%>%
  add_trace(x=surfacedata$UTMY, y=surfacedata$UTMX,
            type='scatter',mode='markers',
            marker=list(color=surfacedata$heatcol), text=~paste(surfacedata$name
                                                                ,"<br>",surfacedata$habitatword
            ),
            hovertemplate = paste(      
              "%{text}"
              # "%{text}<br>"
              # ,"%{alttext}<br>"
              ,"<extra></extra>"
            ),
            showlegend=FALSE) %>%
  add_trace(x=citylabels$y,y=citylabels$x,type="scatter",mode="text",text=citylabels$name,textposition="middle right")%>%
  add_trace(x=riverlabels$y,y=riverlabels$x,type="scatter",mode="text",text=riverlabels$name,textposition="middle right")%>%
  layout(yaxis = list(showticklabels=FALSE)) %>%
  layout(xaxis = list(showticklabels=FALSE
                      ,autorange="reversed"
  ))%>%
  config(modeBarButtonsToRemove = c("autoScale2d", "hoverCompareCartesian","toggleSpikelines"))%>%
  layout(xaxis=list(title="")) %>%
  layout(yaxis=list(title="")) %>% 
  layout(showlegend = FALSE) %>%
  # layout(annotations = list(
  # x = max(riverlabels$y),
  # y = min(riverlabels$x),
  # text = "Grey represents areas not identified as fishing areas. \n Hover over plot for names of fishing areas. \n Use the magnifying glass icon in the toolbar at upper right to zoom, \n and home icon to reset image.",
  # xref = "x",
  # yref = "y",
  # showarrow = FALSE))%>%
  layout(yaxis=list(range=c(270000,490000)))%>%
  config(toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                     filename= 'stripedbasshabitatfishingareas',
                                     height= 500,
                                     width= 700,
                                     scale= 1 ))

surfacedataplotly
htmlwidgets::saveWidget(as_widget(surfacedataplotly), paste("D:/Desktop/Habitat Plots/fishingareasurface",earlyorlate,thiscruise,thisyear,Sys.Date(),".html"))


surfacedataplotlyforexport<-plot_ly()%>%
  partial_bundle()%>%
  config(displayModeBar=TRUE, modeBarButtonsToRemove = c("autoScale2d","hoverCompareCartesian","toggleSpikelines","select2d","lasso2d")) %>%
  add_trace(x=backgroundforsurface$UTMY,y=backgroundforsurface$UTMX,hovertext='none',hovertemplate="",hoverinfo="none"
            ,marker=list(color="lightgrey")
  )%>%
  add_trace(x=surfacedata$UTMY, y=surfacedata$UTMX,
            type='scatter',mode='markers',
            marker=list(color=surfacedata$heatcol), text=~paste(surfacedata$name
                                                                ,"<br>",surfacedata$habitatword
            ),
            hovertemplate = paste(      
              "%{text}"
              # "%{text}<br>"
              # ,"%{surfacedata$habitatword}<br>"
              ,"<extra></extra>"
            ),
            showlegend=FALSE) %>%
  add_trace(x=citylabels$y,y=citylabels$x,type="scatter",mode="text",text=citylabels$name,textposition="middle right")%>%
  add_trace(x=riverlabels$y,y=riverlabels$x,type="scatter",mode="text",text=riverlabels$name,textposition="middle right")%>%
  layout(yaxis = list(showticklabels=FALSE)) %>%
  layout(xaxis = list(showticklabels=FALSE
                      ,autorange="reversed"
  ))%>%
  config(modeBarButtonsToRemove = c("autoScale2d", "hoverCompareCartesian","toggleSpikelines"))%>%
  layout(xaxis=list(title="")) %>%
  layout(yaxis=list(title="")) %>% 
  layout(showlegend = FALSE) %>%
  layout(annotations = list(
    x = max(riverlabels$y),
    y = min(riverlabels$x),
    text = "Grey represents areas not identified as fishing areas. \n Hover over plot for names of fishing areas. \n Use the magnifying glass icon in the toolbar at upper right to zoom, \n and home icon to reset image.",
    xref = "x",
    yref = "y",
    showarrow = FALSE))%>%
  layout(yaxis=list(range=c(270000,490000)))%>%
  config(toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                     filename= paste(thiscruise,'Striped Bass Habitat - Fishing Areas',sep=" "),
                                     height= 500,
                                     width= 700,
                                     scale= 1 ))

htmlwidgets::saveWidget(as_widget(surfacedataplotlyforexport), paste("D:/Desktop/Habitat Plots/fishingareasurfaceforexport",earlyorlate,thiscruise,thisyear,Sys.Date(),".html"))

totalvolforpie<-unsuitablenum+marginalnum+tolerablenum+suitablenum

forpie<-c(unsuitablenum/totalvolforpie,marginalnum/totalvolforpie,tolerablenum/totalvolforpie,suitablenum/totalvolforpie)

forpie<-as.data.frame(forpie)

labels<-c("Unsuitable","Marginal","Tolerable","Suitable")

plotlypie<-plot_ly(forpie,values=~forpie,labels=labels,type='pie',
                   domain=list(x=c(0,1),y=c(0,1)),
                   showlegend=FALSE, hoverinfo='none',
                   marker=list(colors=c("black","orange","yellow","dodgerblue")),
                   text = ~forpie,
                   texttemplate = '%{text:.1p}')%>%
  partial_bundle()

plotly_IMAGE(plotlypie,format="png",out_file=paste("C:/Users/AKeppel/Desktop/fishingareapie",earlyorlate,thiscruise,thisyear,".png",sep=""))

###doing MD waters
samplearea<-"Maryland Waters"

data<-mddatathiscruise

cruisedata<-data
cruisedata[cruisedata==-9999]<-NA

data<-subset(cruisedata,DO>=0
             # |Salinity>=0
             |Wtemp>=0)

###habitat calculation
data$habitat<-ifelse(data$Wtemp>marginaltemp | data$DO<marginalDO, 0, 
                     ifelse(data$Wtemp<=marginaltemp &data$Wtemp>tolerabletemp | data$DO>=marginalDO & data$DO<tolerableDO, 1, 
                            ifelse(data$Wtemp<=tolerabletemp &data$Wtemp>suitabletemp | data$DO>=tolerableDO & data$DO<suitableDO, 2,3)))

data$habitatword<-ifelse(data$habitat==3,"Suitable",
                         ifelse(data$habitat==2,"Tolerable",
                                ifelse(data$habitat==1,"Marginal",
                                       ifelse(data$habitat==0,"Unsuitable",""))))

suitable<-data[data$habitat==3,]
tolerable<-data[data$habitat==2,]
marginal<-data[data$habitat==1,]
unsuitable<-data[data$habitat==0,]

suitablenum<-sum(suitable$volume_m,na.rm=TRUE)/1e+9
tolerablenum<-sum(tolerable$volume_m,na.rm=TRUE)/1e+9
marginalnum<-sum(marginal$volume_m,na.rm=TRUE)/1e+9
unsuitablenum<-sum(unsuitable$volume_m,na.rm=TRUE)/1e+9

mdvolume<-sum(data$volume_m)/1e+9

mdbaysummary<-cbind(suitablenum,tolerablenum,marginalnum,unsuitablenum,mdvolume)

sumtable<-rbind(
  # wholebaysummary,
  mdbaysummary,
  fishingareasummary)
rowsforsumtable<-c(
  # "WholeBay",
  "MD",
  "MDFishingArea")
yearforsumtable<-c(
  # thisyear,
  thisyear,
  thisyear)

sumtable<-cbind(yearforsumtable,rowsforsumtable,sumtable)

colnames(sumtable)<-c("Year","Region","Suitable Habitat Volume", "Tolerable Habitat Volume", "Marginal Habitat Volume", "Unsuitable Habitat Volume", "Total Volume")

###fishing area
###calculate from sumtable
###monthly data for 2023
# percentsuitable<-c(100,100,100,100,84.26402893,80.18000723,85.68977343,63.011
# ,64.96383,91.39042,80.36637,NA,NA,NA,NA)
###monthly data for 2024
percentsuitable<-c(100,100,100,100,84.48,
                   77.00,
                   89.51,
                   15.99,
                   65.56,
                   80.33,
                   78.16,
                   85.32,
                   NA,NA,NA)
months<-c("January","February","March","April","May","Early June","Late June","Early July",
          "Late July","Early August","Late August","September","October","November","December")

###mean data
###from HabitatHistoricRangeCalc.R
maxdata<-c(100,100,100,100,100,90.8,86.6,88.4,91.9,84,97.7,95.1,100,100,100)
mindata<-c(94.7,100,99.9,87.9,66.8,62.2,62.1,16.9,0.819,3.69,36.2,68.7,78.4,89.6,91.5)
meandata<-c(99.8,100,100,98.1,82.8,73.8,72.4,64.3,57.9,60.3,71.1,83,94.3,99.5,99.7)

thisyearfilledplotly<-plot_ly()%>%
  partial_bundle()%>%
  config(displayModeBar=FALSE, modeBarButtonsToRemove = c("autoScale2d","hoverCompareCartesian","toggleSpikelines","select2d","lasso2d")) %>%
  add_trace(x=months,y=maxdata,type='scatter',mode='lines',line = list(color = 'white')
            ,showlegend = FALSE
            # ,name = 'Mean High 1985-2022'
  ) %>%
  add_trace(x=months,y = mindata, type = 'scatter', mode = 'lines',
            fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'white'),
            # showlegend = FALSE, 
            name = '1985-2023 Range')%>%
  add_trace(x=months,y=percentsuitable,type="scatter",mode='lines+markers',name="Percent Suitable Volume 2024",line=list(color="dodgerblue"),marker=list(color="dodgerblue"))%>%
  add_trace(x=months,y=meandata,type="scatter",mode='lines',name="Mean Percent Suitable Volume 1985-2023",line=list(color="darkgreen"))%>%
  layout(xaxis = list(categoryorder = "array",
                      categoryarray = months))%>%
  layout(legend =list(x=0,y=0.05))%>%
  layout(showlegend = TRUE, legend = list(font = list(size = 8)))

thisyearfilledplotly

plotly_IMAGE(thisyearfilledplotly,format="png",out_file=paste("C:/Users/AKeppel/Desktop/fishingareathisyearfilled",earlyorlate,thiscruise,thisyear,".png",sep=""))

htmlwidgets::saveWidget(as_widget(thisyearfilledplotly), paste("D:/Desktop/Habitat Plots/fishingareathisyearfilled",earlyorlate,thiscruise,thisyear,Sys.Date(),".html"))

years<-c("2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")
# years<-c("2011","2012","2013")
# years<-c(
# "2020"
# ,
# "2021","2022")
# years<-c("2014")

results<-NULL

pb<-txtProgressBar(min=0,max=length(years),initial=0)

for (i in 1:length(years)){
  tryCatch({
    
    setTxtProgressBar(pb,i)
    
    DOidpattern<-glob2rx(paste("DO*",years[i],"_",thismonth,"*.txt",sep=""))
    # DOdata<-list.files(path="C:/Users/AKeppel/Desktop/Vol3d/Historic Habitat 8620/",pattern=DOidpattern)
    DOdata<-list.files(path="D:/Desktop/Vol3d/Historic Habitat Last 10 Years for Comparison/",pattern=DOidpattern)
    DOdata<-DOdata[length(DOdata)]
    # DOdata<-read.csv
    DOdata<-read.csv(paste("D:/Desktop/Vol3d/Historic Habitat Last 10 Years for Comparison/",DOdata,sep=""))
    
    
    Wtempidpattern<-glob2rx(paste("WTEMP*",years[i],"_",thismonth,"*.txt",sep=""))
    # Wtempdata<-list.files(path="C:/Users/AKeppel/Desktop/Vol3d/Historic Habitat 8620/",pattern=Wtempidpattern)
    Wtempdata<-list.files(path="D:/Desktop/Vol3d/Historic Habitat Last 10 Years for Comparison/",pattern=Wtempidpattern)
    Wtempdata<-Wtempdata[length(Wtempdata)]
    # Wtempdata<-read.csv(paste("C:/Users/AKeppel/Desktop/Vol3d/Historic Habitat 8620/",Wtempdata,sep=""))
    Wtempdata<-read.csv(paste("D:/Desktop/Vol3d/Historic Habitat Last 10 Years for Comparison/",Wtempdata,sep=""))
    
    DO<-DOdata
    
    DO[DO==-9] <- NA
    
    DO$volume_m<-DO$EW_dim*DO$NS_dim*DO$Vert_dim
    
    DOtrimmed<-DO[,-c(58,57,56,55,54,53,7,6,5,4,3,2,1)]
    
    ###create empty dataset to fill with transposed data
    DOdatatransposed<-NULL
    
    ###for loop to break up data frame and transpose to vertical
    for (o in 1:nrow(DO)){
      tryCatch({
        rowout<-DOtrimmed[o,]
        colout<-t(rowout)
        colout<-colout[!is.na(colout)]
        Sdepth<-seq(0,100,0.5)
        Sdepth<-head(Sdepth,length(colout))
        data<-cbind(colout,Sdepth)
        data<-as.data.frame(data)
        UTMX<-DO[o,1]
        UTMY<-DO[o,2]
        volume_m<-DO[o,58]
        Segment<-DO[o,3]
        data$UTMX<-UTMX
        data$UTMY<-UTMY
        data$volume_m<-volume_m
        data$Segment<-Segment
        DOdatatransposed<-rbind(DOdatatransposed,data)
      },error=function(e){})
    }
    
    wtemp<-Wtempdata
    
    wtemp[wtemp==-9] <- NA
    
    wtemp$volume_m<-wtemp$EW_dim*wtemp$NS_dim*wtemp$Vert_dim
    
    wtemptrimmed<-wtemp[,-c(58,57,56,55,54,53,7,6,5,4,3,2,1)]
    
    ###create empty dataset to fill with transposed data
    wtempdatatransposed<-NULL
    
    ###for loop to break up data frame and transpose to vertical
    for (p in 1:nrow(wtemp)){
      tryCatch({
        rowout<-wtemptrimmed[p,]
        colout<-t(rowout)
        colout<-colout[!is.na(colout)]
        Sdepth<-seq(0,100,0.5)
        Sdepth<-head(Sdepth,length(colout))
        data<-cbind(colout,Sdepth)
        data<-as.data.frame(data)
        UTMX<-wtemp[p,1]
        UTMY<-wtemp[p,2]
        volume_m<-wtemp[p,58]
        Segment<-wtemp[p,3]
        data$UTMX<-UTMX
        data$UTMY<-UTMY
        data$volume_m<-volume_m
        data$Segment<-Segment
        wtempdatatransposed<-rbind(wtempdatatransposed,data)
      },error=function(e){})
    }
    
    names(DOdatatransposed)[names(DOdatatransposed) == "colout"] <- "DO"
    names(wtempdatatransposed)[names(wtempdatatransposed) == "colout"] <- "Wtemp"
    
    wholebaydata<-merge(DOdatatransposed,wtempdatatransposed,by=c("Segment","UTMX","UTMY","Sdepth","volume_m"),all=TRUE)
    
    ###habitat calculation
    wholebaydata$habitat<-ifelse(wholebaydata$Wtemp>marginaltemp | wholebaydata$DO<marginalDO, 0, 
                                 ifelse(wholebaydata$Wtemp<=marginaltemp &wholebaydata$Wtemp>tolerabletemp | wholebaydata$DO>=marginalDO & wholebaydata$DO<tolerableDO, 1, 
                                        ifelse(wholebaydata$Wtemp<=tolerabletemp &wholebaydata$Wtemp>suitabletemp | wholebaydata$DO>=tolerableDO & wholebaydata$DO<suitableDO, 2,3)))
    
    mdsegments<-c("BACOH","BIGMH","BOHOH"
                  ,"BSHOH","C&DOH","CB1TF","CB2OH","CB3MH","CB4MH","CB5MH_MD","CB5MH"
                  ,"CHOMH1","CHOMH2","CHOOH","CHOTF","CHSMH","CHSOH","CHSTF"
                  ,"EASMH","ELKOH","FSBMH","GUNOH","HNGMH","LCHMH","MAGMH"
                  ,"MANMH","MATTF","MIDOH","NANMH","NANOH","NANTF","NORTF"
                  ,"PATMH","PAXMH","PAXOH","PAXTF","PISTF","POCMH","POCOH"
                  ,"POCTF","POCMH_MD","POTMH","POTOH","POTTF","RHDMH","SASOH","SEVMH"
                  ,"SOUMH","TANMH","TANMH_MD","WICMH","WSTMH"
    )
    
    mddata<-wholebaydata[wholebaydata$Segment %in% mdsegments,]
    
    data<-subset(mddata,DO>=0
                 # |Salinity>=0
                 |Wtemp>=0)
    
    suitable<-data[data$habitat==3,]
    tolerable<-data[data$habitat==2,]
    marginal<-data[data$habitat==1,]
    unsuitable<-data[data$habitat==0,]
    
    suitablenum<-sum(suitable$volume_m,na.rm=TRUE)/1e+9
    tolerablenum<-sum(tolerable$volume_m,na.rm=TRUE)/1e+9
    marginalnum<-sum(marginal$volume_m,na.rm=TRUE)/1e+9
    unsuitablenum<-sum(unsuitable$volume_m,na.rm=TRUE)/1e+9
    
    
    mdvolume<-sum(data$volume_m)/1e+9
    
    mdbaysummary<-cbind(paste(years[i]),"MD",suitablenum,tolerablenum,marginalnum,unsuitablenum,mdvolume)
    
    fishingareadata<-merge(wholebaydata,primedataforplotly,by=c("UTMX","UTMY"))
    
    data<-subset(fishingareadata,DO>=0
                 # |Salinity>=0
                 |Wtemp>=0)
    
    suitable<-data[data$habitat==3,]
    tolerable<-data[data$habitat==2,]
    marginal<-data[data$habitat==1,]
    unsuitable<-data[data$habitat==0,]
    
    suitablenum<-sum(suitable$volume_m,na.rm=TRUE)/1e+9
    tolerablenum<-sum(tolerable$volume_m,na.rm=TRUE)/1e+9
    marginalnum<-sum(marginal$volume_m,na.rm=TRUE)/1e+9
    unsuitablenum<-sum(unsuitable$volume_m,na.rm=TRUE)/1e+9
    
    
    fishingareavolume<-sum(data$volume_m)/1e+9
    
    fishingareasummary<-cbind(paste(years[i]),"MDFishingArea",suitablenum,tolerablenum,marginalnum,unsuitablenum,fishingareavolume)
    
    
    results<-rbind(results
                   # ,wholebaysummary
                   ,mdbaysummary,fishingareasummary)
    
    close(pb)
  },error=function(e){})
  
}

# results<-rbind(resultsold,results)

results<-rbind(results,sumtable)

results<-as.data.frame(results)

colnames(results)<-c("Year","Region","SuitableHabitatVolume", "TolerableHabitatVolume", "MarginalHabitatVolume", "UnsuitableHabitatVolume", "TotalVolume")

mdhistoric<-results[results$Region=="MD",]
# wbhistoric<-results[results$Region=="WholeBay",]
fishingareahistoric<-results[results$Region=="MDFishingArea",]

write.csv(results,paste("C:/Users/AKeppel/Desktop/10yearcomparison-",earlyorlate,thiscruise,thisyear,"-",Sys.Date(),".csv",sep=""),row.names=FALSE)

# kable(results)

# kable(mdhistoric,row.names=FALSE)
# kable(wbhistoric, row.names=FALSE)
# kable(fishingareahistoric, row.names = FALSE)

results$SuitableHabitatVolume<-as.numeric(as.character(results$SuitableHabitatVolume))
results$TolerableHabitatVolume<-as.numeric(as.character(results$TolerableHabitatVolume))
results$MarginalHabitatVolume<-as.numeric(as.character(results$MarginalHabitatVolume))
results$UnsuitableHabitatVolume<-as.numeric(as.character(results$UnsuitableHabitatVolume))
results$TotalVolume<-as.numeric(as.character(results$TotalVolume))

results$percentsuitable<-results$SuitableHabitatVolume/(results$SuitableHabitatVolume+results$TolerableHabitatVolume+results$MarginalHabitatVolume+results$UnsuitableHabitatVolume)*100
results$percenttolerable<-results$TolerableHabitatVolume/(results$SuitableHabitatVolume+results$TolerableHabitatVolume+results$MarginalHabitatVolume+results$UnsuitableHabitatVolume)*100
results$percentmarginal<-results$MarginalHabitatVolume/(results$SuitableHabitatVolume+results$TolerableHabitatVolume+results$MarginalHabitatVolume+results$UnsuitableHabitatVolume)*100
results$percentunsuitable<-results$UnsuitableHabitatVolume/(results$SuitableHabitatVolume+results$TolerableHabitatVolume+results$MarginalHabitatVolume+results$UnsuitableHabitatVolume)*100

mdresults<-results[results$Region=="MD",]
# wholebayresults<-results[results$Region=="WholeBay",]
mdfishresults<-results[results$Region=="MDFishingArea",]


stackedmdfish<-plot_ly(mdfishresults,x=~Year,y=~percentsuitable,type='bar',name="Suitable",marker=list(color="dodgerblue")) %>%
  add_bars(x=~Year,y=~percenttolerable,name="Tolerable",marker=list(color="yellow")) %>%
  add_bars(x=~Year,y=~percentmarginal,name="Marginal",marker=list(color="orange")) %>%
  add_bars(x=~Year,y=~percentunsuitable,name="Unsuitable",marker=list(color="black")) %>%
  layout(yaxis=list(title=paste("Percent of Total Volume",sep="")),barmode="stack") %>%
  partial_bundle()
# %>%
# layout(title=list(text=paste("Maryland Fishing Area Striped Bass Habitat Volume<br>Second June Cruise 2023 Compared to Preceding 10 Years",sep=""),y=0.975,yanchor="top"))
#   layout(legend = list(y = 0.8)) 

plotly_IMAGE(stackedmdfish,format="png",out_file =paste("C:/Users/akeppel/Desktop/stackedmdfish",earlyorlate,thiscruise,thisyear,".png",sep=""))

htmlwidgets::saveWidget(as_widget(stackedmdfish), paste("D:/Desktop/Habitat Plots/stackedmdfish",earlyorlate,thiscruise,thisyear,Sys.Date(),".html"))

# stackedmdfish

###choose area of interest, just MD or whole bay
samplearea<-"Maryland Waters"

data<-mddatathiscruise

# ###which dataset is this?
# dataset<-"BAY828"

cruisedata<-data
cruisedata[cruisedata==-9999]<-NA

data<-subset(cruisedata,DO>=0
             # |Salinity>=0
             |Wtemp>=0)

###habitat calculation
data$habitat<-ifelse(data$Wtemp>marginaltemp | data$DO<marginalDO, 0, 
                     ifelse(data$Wtemp<=marginaltemp &data$Wtemp>tolerabletemp | data$DO>=marginalDO & data$DO<tolerableDO, 1, 
                            ifelse(data$Wtemp<=tolerabletemp &data$Wtemp>suitabletemp | data$DO>=tolerableDO & data$DO<suitableDO, 2,3)))

suitable<-data[data$habitat==3,]
tolerable<-data[data$habitat==2,]
marginal<-data[data$habitat==1,]
unsuitable<-data[data$habitat==0,]

suitablenum<-sum(suitable$volume_m,na.rm=TRUE)/1e+9
tolerablenum<-sum(tolerable$volume_m,na.rm=TRUE)/1e+9
marginalnum<-sum(marginal$volume_m,na.rm=TRUE)/1e+9
unsuitablenum<-sum(unsuitable$volume_m,na.rm=TRUE)/1e+9

mdvolume<-sum(data$volume_m)/1e+9

mdbaysummary<-cbind(suitablenum,tolerablenum,marginalnum,unsuitablenum,mdvolume)

data$paramofinterest<-data$habitat

minvalue<-0
maxvalue<-3

ii<-cut(data$paramofinterest,breaks=seq(minvalue,maxvalue,len=100), include.lowest = TRUE)

### habitat
data$heatcol<-colorRampPalette(c("black","orange","yellow","dodgerblue"))(99)[ii]

surfacetest<-data %>% group_by(uniqueID) %>% slice_min(order_by=paramofinterest,with_ties=FALSE)

surfacedata<-unique(surfacetest)
###this seems like it shouldnt be here 20230815
# surfacedata<-data

surfacedata$habitatword<-ifelse(surfacedata$habitat==3,"Suitable",
                                ifelse(surfacedata$habitat==2,"Tolerable",
                                       ifelse(surfacedata$habitat==1,"Marginal",
                                              ifelse(surfacedata$habitat==0,"Unsuitable",""))))

write.csv(surfacedata,paste("C:/Users/AKeppel/Desktop/MDsurfacedataforMark-",earlyorlate," ",thiscruise," ",thisyear,"-",Sys.Date(),".csv",sep=""),row.names = FALSE)

#  backgroundforsurface<-read.csv("D:/Desktop/Helpful Info/allbaypoints.csv")
#  backgroundforsurface<-backgroundforsurface[backgroundforsurface$UTMX<=490000&backgroundforsurface$UTMX>=290000,]
# backgroundforsurface<-backgroundforsurface[backgroundforsurface$UTMY<=4430000&backgroundforsurface$UTMY>=4160000,]

surfacedataplotly<-plot_ly()%>%
  partial_bundle()%>%
  config(displayModeBar=TRUE, modeBarButtonsToRemove = c("autoScale2d","hoverCompareCartesian","toggleSpikelines","select2d","lasso2d")) %>%
  add_trace(x=backgroundforsurface$UTMY,y=backgroundforsurface$UTMX,hovertext='none',hovertemplate="",hoverinfo="none"
            ,marker=list(color="lightgrey")
  )%>%
  add_trace(x=surfacedata$UTMY, y=surfacedata$UTMX,
            type='scatter',mode='markers',
            marker=list(color=surfacedata$heatcol),
            text=~paste(
              #           surfacedata$name
              # ,"<br>",
              surfacedata$habitatword
            ),
            hovertemplate = paste(      
              "%{text}"
              # "%{text}<br>"
              # ,"%{alttext}<br>"
              ,"<extra></extra>"
            )
            ,marker=list(color="lightgrey")
  )%>%
  add_trace(x=citylabels$y,y=citylabels$x,type="scatter",mode="text",text=citylabels$name,textposition="middle right")%>%
  add_trace(x=riverlabels$y,y=riverlabels$x,type="scatter",mode="text",text=riverlabels$name,textposition="middle right")%>%
  layout(yaxis = list(showticklabels=FALSE)) %>%
  layout(xaxis = list(showticklabels=FALSE
                      ,autorange="reversed"
  ))%>%
  config(modeBarButtonsToRemove = c("autoScale2d", "hoverCompareCartesian","toggleSpikelines"))%>%
  layout(xaxis=list(title="")) %>%
  layout(yaxis=list(title="")) %>% 
  layout(showlegend = FALSE) %>% 
  layout(annotations = list(
    x = max(riverlabels$y),
    y = min(riverlabels$x),
    text = "Grey represents areas with no data",
    xref = "x",
    yref = "y",
    showarrow = FALSE)) %>%
  layout(yaxis=list(range=c(270000,490000)))

surfacedataplotly
htmlwidgets::saveWidget(as_widget(surfacedataplotly),'july2023',".html")

# plotly_IMAGE(surfacedataplotly,format="png",out_file="C:/Users/AKeppel/Desktop/mdsurfacedata.png")

### main channel data

mainchanneldata<-merge(data,crosssectionmain,by=c("UTMX","UTMY"),allow.cartesian=TRUE)
mainchanneldata<-unique(mainchanneldata)

mainchanneldata$milesX<-mainchanneldata$UTMX*0.000621371
mainchanneldata$milesY<-mainchanneldata$UTMY*0.000621371

mainchanneldata$milesY<-mainchanneldata$milesY-min(xrangemiles)

mainchannelplotly<-plot_ly()%>%
  partial_bundle()%>%
  config(displayModeBar=FALSE, modeBarButtonsToRemove = c("autoScale2d","hoverCompareCartesian","toggleSpikelines","select2d","lasso2d")) %>%
  add_trace(x=mainchanneldata$milesY,y=mainchanneldata$Sdepth
            ,hoverinfo="none"
            ,type='scatter',mode='markers'
            ,marker=list(color=mainchanneldata$heatcol))%>%
  layout(xaxis=list(title="Distance from mouth of Bay (miles)",autorange="reversed"))%>%
  layout(yaxis=list(title="Depth (ft)",autorange="reversed"))

# mainchannelplotly
htmlwidgets::saveWidget(as_widget(mainchannelplotly), paste("D:/Desktop/Habitat Plots/MDmainchannel",earlyorlate,thiscruise,thisyear,Sys.Date(),".html"))

plotly_IMAGE(mainchannelplotly,format="png",out_file=paste("C:/Users/akeppel/Desktop/mdmainchannel",earlyorlate,thiscruise,thisyear,".png",sep=""))


### potomac channel data

potomacchanneldata<-merge(data,crosssectionpotomac,by=c("UTMX","UTMY"),allow.cartesian=TRUE)
potomacchanneldata<-unique(potomacchanneldata)

potomacchanneldata$milesX<-potomacchanneldata$UTMX*0.000621371
potomacchanneldata$milesY<-potomacchanneldata$UTMY*0.000621371

# potomacchanneldata$milesY<-potomacchanneldata$milesY-min(xrangemiles)
potomacchanneldata$milesY<-potomacchanneldata$milesY-min(potomacchanneldata$milesY)

potomacchannelplotly<-plot_ly()%>%
  partial_bundle()%>%
  config(displayModeBar=FALSE, modeBarButtonsToRemove = c("autoScale2d","hoverCompareCartesian","toggleSpikelines","select2d","lasso2d")) %>%
  add_trace(x=potomacchanneldata$milesY,y=potomacchanneldata$Sdepth
            ,hoverinfo="none"
            ,type='scatter',mode='markers'
            ,marker=list(color=potomacchanneldata$heatcol))%>%
  layout(xaxis=list(title="Distance from mouth of Potomac (miles)",autorange="reversed",zeroline=FALSE))%>%
  layout(yaxis=list(title="Depth (ft)",autorange="reversed")) 
# %>%
#             layout(title = 'Potomac River Main Channel Cross Section')

plotly_IMAGE(potomacchannelplotly,format="png",out_file=paste("C:/Users/akeppel/Desktop/potomacchannel",earlyorlate,thiscruise,thisyear,".png",sep=""))

htmlwidgets::saveWidget(as_widget(potomacchannelplotly), paste("D:/Desktop/Habitat Plots/potomacchannel",earlyorlate,thiscruise,thisyear,Sys.Date(),".html"))

# potomacchannelplotly

totalvolforpie<-unsuitablenum+marginalnum+tolerablenum+suitablenum

forpie<-c(unsuitablenum/totalvolforpie,marginalnum/totalvolforpie,tolerablenum/totalvolforpie,suitablenum/totalvolforpie)

forpie<-as.data.frame(forpie)

labels<-c("Unsuitable","Marginal","Tolerable","Suitable")

plotlypie<-plot_ly(forpie,values=~forpie,labels=labels,type='pie',
                   domain=list(x=c(0,1),y=c(0,1)),
                   showlegend=FALSE, hoverinfo='none',
                   marker=list(colors=c("black","orange","yellow","dodgerblue")),
                   text = ~forpie,
                   texttemplate = '%{text:.1p}')%>%
  partial_bundle()

plotly_IMAGE(plotlypie,format="png",out_file=paste("C:/Users/akeppel/Desktop/mdpie",earlyorlate,thiscruise,thisyear,".png",sep=""))
htmlwidgets::saveWidget(as_widget(plotlypie), paste("D:/Desktop/Habitat Plots/mdpie",earlyorlate,thiscruise,thisyear,Sys.Date(),".html"))

# 
# surfacedataplotly
# mainchannelplotly
# plotlypie
# potomacchannelplotly
# 
# knitr::include_graphics("C:/Users/akeppel/Desktop/Legend.jpg")

# surfacedataplotly

# knitr::include_graphics("C:/Users/akeppel/Desktop/MDsurfacedata.png")

stackedmd<-plot_ly(mdresults,x=~Year,y=~percentsuitable,type='bar',name="Suitable",marker=list(color="dodgerblue")) %>%
  add_bars(x=~Year,y=~percenttolerable,name="Tolerable",marker=list(color="yellow")) %>%
  add_bars(x=~Year,y=~percentmarginal,name="Marginal",marker=list(color="orange")) %>%
  add_bars(x=~Year,y=~percentunsuitable,name="Unsuitable",marker=list(color="black")) %>%
  layout(yaxis=list(title=paste("Percent of Total Volume",sep="")),barmode="stack") %>%
  partial_bundle()
# %>%
#   layout(title=list(text=paste("Maryland Bay Striped Bass Habitat Volume<br>Second June Cruise 2023 Compared to Preceding 10 Years",sep=""),y=0.975,yanchor="top"))
#   layout(legend = list(y = 0.8)) 

plotly_IMAGE(stackedmd,format="png",out_file=paste("C:/Users/akeppel/Desktop/stackedmd",earlyorlate,thiscruise,thisyear,".png",sep=""))
htmlwidgets::saveWidget(as_widget(stackedmd), paste("D:/Desktop/Habitat Plots/stackedmd",earlyorlate,thiscruise,thisyear,Sys.Date(),".html"))

###whole MD
###monthly data for 2023
# percentsuitable<-c(100,100,100,100,84.26402893,80.18000723,85.68977343,63.011
# ,64.96383,91.39042,80.36637,NA,NA,NA,NA)
###monthly data for 2024
percentsuitable<-c(100,100,100,100,88.25,78.68,87.54,12.92
                   ,48.79
                   ,81.06
                   ,75.02
                   ,87.92
                   ,NA,NA,NA)
months<-c("January","February","March","April","May","Early June","Late June","Early July",
          "Late July","Early August","Late August","September","October","November","December")

###mean data
###from HabitatHistoricRangeCalc.R
maxdata<-c(100,100,100,100,99.9,91.8,89.1,88.6,85.5,86.3,91.4,97.9,100,100,100)
mindata<-c(98.6,100,99.9,96.3,71.6,67.9,59.5,11.7,2.52,4.31,26.2,77.1,88.4,97,95.3)
meandata<-c(99.9,100,100,99.4,90.8,78.3,76.1,64.5,56.9,61,72.1,87.9,97,99.8,99.8)

# thisyearplotly<-plot_ly() %>%
#   add_trace(x=months,y=percentsuitable,type="bar",name="Percent Suitable Volume 2022",marker=list(color="dodgerblue")) %>%
#   add_trace(x=months,y=meandata,type="scatter",mode="lines+markers",name="Historical Mean Percent Suitable Volume (1985-2021)",line=list(color="darkred"),marker=list(color="darkred"))%>%
#   layout(xaxis = list(categoryorder = "array",
#                       categoryarray = months))
# 
# thisyearplotly

###playing with filled area plot

thisyearfilledplotly<-plot_ly()%>%
  partial_bundle()%>%
  config(displayModeBar=FALSE, modeBarButtonsToRemove = c("autoScale2d","hoverCompareCartesian","toggleSpikelines","select2d","lasso2d")) %>%
  add_trace(x=months,y=maxdata,type='scatter',mode='lines',line = list(color = 'white')
            ,showlegend = FALSE
            # ,name = 'Mean High 1985-2020'
  ) %>%
  add_trace(x=months,y = mindata, type = 'scatter', mode = 'lines',
            fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'white'),
            # showlegend = FALSE, 
            name = '1985-2022 Range')%>%
  add_trace(x=months,y=percentsuitable,type="scatter",mode='lines+markers',name="Percent Suitable Volume 2023",line=list(color="dodgerblue"),marker=list(color="dodgerblue"))%>%
  add_trace(x=months,y=meandata,type="scatter",mode='lines',name="Mean Percent Suitable Volume 1985-2022",line=list(color="darkgreen"))%>%
  layout(xaxis = list(categoryorder = "array",
                      categoryarray = months)) %>%
  layout(legend =list(x=0,y=0.05))%>%
  layout(showlegend = TRUE, legend = list(font = list(size = 8)))

thisyearfilledplotly

plotly_IMAGE(thisyearfilledplotly,format="png",out_file=paste("C:/Users/akeppel/Desktop/mdthisyearfilled",earlyorlate,thiscruise,thisyear,".png",sep=""))
htmlwidgets::saveWidget(as_widget(thisyearfilledplotly), paste("D:/Desktop/Habitat Plots/mdthisyearfilled",earlyorlate,thiscruise,thisyear,Sys.Date(),".html"))

