#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#In order to run this app, first source /HabitatSuitabilityOrganization.R
#This script generates the necessary data objects and labels for the app. It takes ~2 minutes to run.
#It will generate the summary files this app loads in.

#Load libraries 

library(shiny)
library(bslib)
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
library(shinyjs)
library(ragg)

#### UPDATE THIS WITH CURRENT CRUISE FILE

thiscruise <- "BAY915.csv"

#late or early? Update
lateorearly <- "Late"

setwd(here("Striped-Bass-Habitat-Suitability"))

#########################################

#variables the app needs

#What cruise are we working with? Update for this month's
rawcruisedata<-fread(thiscruise)

###Ensure the data is confined to a single month, otherwise filter out extraneous dates
startdate<-min(rawcruisedata$Date, na.rm=T)
enddate<-max(rawcruisedata$Date, na.rm=T)

# rawcruisedata <- rawcruisedata %>%
#   filter(Date == startdate)

#This section ID's the cruise date to grab associated DO and Temp files later
monthdate <- as.numeric(substr(enddate,6,7))
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

#And define parameters for Striped Bass:
suitabletemp<-82.4
tolerabletemp<-84.2
marginaltemp<-86
unsuitabletemp<-86
suitableDO<-4
tolerableDO<-3
marginalDO<-2
unsuitableDO<-2

suitability_colors <- c(
  "Unsuitable" = "black",
  "Marginal" = "orange",
  "Tolerable" = "yellow",
  "Suitable" = "dodgerblue"
)

verdana <- 'verdana'

#Bring in objects

files <- list.files(pattern = "wholebaysummary")
wholebaybottomsummary <- fread(files[1])

files <- list.files(pattern = "fishinghotspotsummary")
fishinghotspotssummary <- fread(files[1])

files <- list.files(pattern = "historicbaydatafishingareassummary")
historicbaydata_fishingareas_summary <- fread(files[1])
historicbaydata_fishingareas_summary$Habitat <- factor(historicbaydata_fishingareas_summary$Habitat, levels = c("Suitable", "Tolerable", "Marginal", "Unsuitable"))

files <- list.files(pattern = "historicbaydatasummary")
historicbaydata_summary <- fread(files[1])
historicbaydata_summary$Habitat <- factor(historicbaydata_summary$Habitat, levels = c("Suitable", "Tolerable", "Marginal", "Unsuitable"))

files <- list.files(pattern = "mainchanneldata")
mainchanneldata <- fread(files[1])

files <- list.files(pattern = "labels_mainstem")
labels_mainstem <- fread(files[1])

files <- list.files(pattern = "potomacchanneldata")
potomacchanneldata <- fread(files[1])

files <- list.files(pattern = "labels_potomac")
labels_potomac <- fread(files[1])

files <- list.files(pattern = "historicalmeans_hs")
historicalmeans_hs <- fread(files[1])
labels <- historicalmeans_hs$months
breaks <- historicalmeans_hs$monthseq

files <- list.files(pattern = "historicalmeans_wb")
historicalmeans_wb <- fread(files[1])
labels <- historicalmeans_wb$months
breaks <- historicalmeans_wb$monthseq

fishingareapolygons.dd <- st_read(here("Striped-Bass-Habitat-Suitability", "FishingAreaPolygons"))

fishingareacoords.dd_bottom <- st_read(here("Striped-Bass-Habitat-Suitability", "FishingAreaQuality"))

#transform this into a df so ADA users can download
fishingareacoordinates <- fortify(fishingareacoords.dd_bottom)

mddatathiscruise.dd_bottom <- st_read(here("Striped-Bass-Habitat-Suitability", "WholeBayQuality"))
#transform this into a df so ADA users can download
chesapeakebaycoordinates <- fortify(mddatathiscruise.dd_bottom)


############ UI ######################

#This generates the layout of our app
#And all of the interactive features of the widgets

ui <- fluidPage(
  tags$head(includeHTML(here("stripedbassanalytics.html"))),
  tags$html(lang="en"),
  theme = bs_theme(preset = "flatly"),
  
  #styling the info tab to be a light blue
  tags$style(HTML("
    .accordion {
      --bs-accordion-bg: #d1e6fc
    }
    .accordion-item {
      --bs-accordion-bg: #fcfcfc
    }
                  ")),
  
  # Application title
  tags$div(
    style = "display: flex; justify-content: space-between; align-items: center; margin-top: 20px; margin-bottom: 20px;",
    tags$h2(paste("Maryland Striped Bass Habitat Suitability for", lateorearly, monthname, thisyear, sep=' '), style = "margin: 0;"),
    tags$img(src = "DNR_logo_final.png", height = "60px", 
             alt = "The MD DNR logo, featuring a heron flying across a sun towards a pine tree over water. It is drawn with heavy lines.")
  ),
  
  hr(),
  
  layout_columns(
    accordion(
      accordion_panel(
        title = HTML(paste("<b>", "App Information and Instructions" ,"</b>")),
        layout_columns(
          card(
            card_header("App Instructions"),
            p(uiOutput("HowToUse"))
          ),
          navset_card_tab( 
              nav_panel("Legend", 
                        img(src = 'Bass_Suitable_Criteria.png', 
                            alt = "Suitable habitat criteria for Striped Bass,
                            Blue supports normal growth (less than 82.4F and greater than 4mg/L DO),
                            Yellow indicates tolerable habitat (between 82.4 and 84.2F and 3-4mg/L DO,
                            ORange indicates marginal habitat (between 84.2 and 86F and 2-3 mg/L DO,
                            Black indicates unsuitable habitat (greater than 86F and less than 2 mg/L DO.")), 
              nav_panel("Striped Bass Squeeze", 
                        img(src = 'Striped_Bass_Squeeze.png', 
                            alt = "Striped bass squeeze is a hypothesis 
                            that when temperatures on the surface of the water heat, 
                            and O2 is depleted on the bottom, striped bass will be
                            squeezed into the middle of the habitat.")), 
            nav_panel("More Info", uiOutput('DNRLinks'))
          )
        )
      ),
      open=F
    )
  ),
  
  layout_columns(
    accordion(
      accordion_panel(
        title = HTML(paste("<b>", "ADA Data Downloads", "</b>")),
        card(
          card_header("ADA Data Downloads"),
          uiOutput("AdaExplainer"),
          downloadButton("downloadData1", "Download Chesapeake Bay Pie Chart Summary Data", `aria-label` = "Download suitability summary charts for the entire MD Chesapeake Bay."),
          downloadButton("downloadData2", "Download Fishing Hot Spot Pie Chart Summary Data", `aria-label` = "Download suitability summary charts for popular fishing spots."),
          downloadButton("downloadData3", "Download Chesapeake Bay Depth Data", `aria-label` = "Download suitability by depth for the entire MD Chesapeake Bay."),
          downloadButton("downloadData4", "Download Potomac River Depth Data", `aria-label` = "Download suitability by depth for the Potomac River."),
          downloadButton("downloadData5", "Download This Year Hot Spot Mean Percent Suitability Data",  `aria-label` = "Download the percentage of water of suitable quality for popular fishing spots throughout this year comapred to historical ranges."),
          downloadButton("downloadData6", "Download Hot Spot Suitability Historic Comparison Data",  `aria-label` = "Download the comparison of fishing spot suitability for this month for the previous ten years."),
          downloadButton("downloadData7", "Download This Year Bay Mean Percent Suitability Data",  `aria-label` = "Download the percentage of water of suitable quality for the MD Chesapeake Bay throughout this year comapred to historical ranges."),
          downloadButton("downloadData8", "Download Bay Suitability Historic Comparison Data",  `aria-label` = "Download the comparison of the MD Chesapeake Bay suitability for this month for the previous ten years."),
          downloadButton("MapData1", "Download Fishing Area Spatial Suitability Coordinates",  `aria-label` = "Download the spatial suitability  of popular fishing areas in the Chesapeake Bay."),
          downloadButton("MapData2", "Download Chesapeake Bay Spatial Suitability Coordinates",  `aria-label` = "Download the spatial suitability of the MD Chesapeake Bay.")
        )
      ),
      open=F
    )
  ),
  
  card(
    card_header(HTML(paste("<b>", "Chesapeake Bay Bottom Map" ,"</b>"))),
    fluidRow(
      column(8, leafletOutput("BayMap", height = 600)),
      column(4,
             selectInput("layer", "Toggle to Change Layer", choices = c("Fishing Area Habitat Suitability", "Maryland Bay Habitat Suitability"), selected = "Fishing Area Habitat Suitability"),
             conditionalPanel("input.layer == 'Fishing Area Habitat Suitability'", plotlyOutput("HotSpotPie")),
             conditionalPanel("input.layer == 'Maryland Bay Habitat Suitability'", plotlyOutput("WholeBayPie"))
      )
    )
  ),
  
  layout_columns(
    navset_card_tab(
      nav_panel(HTML(paste("<b>", "Cross-Section of the MD Mainstem" ,"</b>")), plotlyOutput(outputId = 'WholeBayCrossSection')), 
      nav_panel(HTML(paste("<b>", "Cross-Section of the Potomac" ,"</b>")), plotlyOutput(outputId = 'PotomacCrossSection'))
    )
  ),
  
  layout_columns(
    navset_card_tab(
      nav_panel(HTML(paste("<b>", "Fishing Area Mean Habitat Suitability vs Historical Average" ,"</b>")), plotOutput(outputId = "HotSpotVolume")),
      nav_panel(HTML(paste("<b>", "Fishing Area Habitat Suitability, Last 10 Yrs" ,"</b>")), plotlyOutput(outputId = "HotSpot10yrs")) 
    ),
    navset_card_tab(
      nav_panel(HTML(paste("<b>", "Maryland Bay Mean Habitat Suitability vs Historical Average" ,"</b>")), plotOutput(outputId = "WholeBayVolume")),
      nav_panel(HTML(paste("<b>", "Maryland Bay Habitat Suitability, Last 10 Yrs" ,"</b>")), plotlyOutput(outputId = "WholeBay10yrs"))
    )
  )
)

############### SERVER ##########################

#This generates all the data and figures

server <- function(input, output, session) {
  
  ##############################################################################
  
  #this first section contains the app instructions + information
  
  #app instructions
  output$HowToUse <- renderUI({
    HTML(paste(
      "<p>This dashboard provides the most recent striped bass habitat suitability information 
      based on bottom dissolved oxygen conditions and surface water temperatures measured during 
      Maryland DNR’s monthly (rivers) and twice monthly (Chesapeake Bay) summer monitoring cruises. 
      Low bottom dissolved oxygen and high surface temperatures can constrict the habitable areas 
      as well as produce other stressors on the ecosystem.</p>",
      
      "<p>The following list walks through each panel of information to best predict where you may find striped bass from the most recent monitoring data.</p>",
      
      "<ul>",
      "<li> The map displays up to three layers: common fishing locations, habitat suitability in fishing areas, and habitat suitability across the entire bay.</li>",
      "<li> By default, the fishing area habitat suitability is displayed. Select the other layers as desired from the upper right box on the map. 
      Note: the Maryland bay suitability layer will take 30 seconds or longer to load and display. </li>",
      "<li> You can also filter locations on the map by suitability criteria, by selecting the corresponding slice on the summary pie chart to the right of the map. </li>",
      "<li> For legend information on how we define suitable habitat for striped bass, see the legend in the panel to the right.</li>",
      "<li> The map displays bottom data only. Beneath the map find the main bay channel and Potomac river channel depth habitat suitability cross sections.</li>",
      "<li> Finally, find how this year's data corresponds to historical results in the bar charts at the bottom of the app.</li>",
      "</ul>",
      
      "For more information, see the links to other DNR resources in the panel to the right.",
      sep=""
    ))
  })
  
  #more information
  output$DNRLinks <- renderUI({
    HTML(paste(
      "<p>DNR Links for More Information:</p>",
      "<ul>",
      "<li> <a href=", "https://eyesonthebay.dnr.maryland.gov/", " target=", "_gap", " rel=", "noreferrer", "> Water Quality Information: Eyes on the Bay</a></li>",
      "</ul>",
      "<p>Striped Bass Habitat Criteria:</p>",
      "<ul>",
      "<li> <a href=", "https://eyesonthebay.dnr.maryland.gov/eyesonthebay/documents/DevelopmentOfTemperatureAndDOBasedHabitatRequirements.pdf", " target=", "_gap", " rel=", "noreferrer", "> Development of Habitat Conditions</a>, pg 136-144</li>",
      "<li> <a href=", "https://eyesonthebay.dnr.maryland.gov/eyesonthebay/documents/ImpactsOfClimateChangeOnStripedBassHabitat2023.pdf", " target=", "_gap", " rel=", "noreferrer", "> Climate Change and Resident Chesapeake Bay Striped Bass Habitat</a>, slides 1-15</li>",
      "</ul>",
      sep=""
    ))
  })
  
  ##############################################################################
  
  #Ada Information
  
  output$AdaExplainer <- renderUI({
    HTML(paste0(
      "<p>This app utilizes interactive charts and maps to best predict where to find Striped Bass within the MD Chesapeake Bay. </p>",
      "<p>If you are utilizing a screen reader, these data are available to download in chart form via the buttons below. </p>",
      "<p>This app is keyboard navigable. Links are navigable via the 'tab' button. To select a button, hit 'enter' and your download should begin. </p>"
    ))
  })
  
  #these downloaders allow a user to download the sheets pulled in from the beginning. 
  output$downloadData1 <- downloadHandler(
    filename = function() { paste(monthname, "_", thisyear, "_", "wholebaybottomsummary", ".csv") },
    content = function(file) { fwrite(wholebaybottomsummary, file) }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() { paste0(monthname, "_", thisyear, "_", "fishinghotspotssummary", ".csv") },
    content = function(file) { fwrite(fishinghotspotssummary, file) }
  )
  
  output$downloadData3 <- downloadHandler(
    filename = function() { paste0(monthname, "_", thisyear, "_", "mainchanneldata", ".csv") },
    content = function(file) { fwrite(mainchanneldata, file) }
  )
  
  output$downloadData4 <- downloadHandler(
    filename = function() { paste0(monthname, "_", thisyear, "_", "potomacchanneldata", ".csv") },
    content = function(file) { fwrite(potomacchanneldata, file) }
  )
  
  output$downloadData5 <- downloadHandler(
    filename = function() { paste0(monthname, "_", thisyear, "_", "historicalmeans_hs", ".csv") },
    content = function(file) { fwrite(historicalmeans_hs, file) }
  )
  
  output$downloadData6 <- downloadHandler(
    filename = function() { paste0(monthname, "_", thisyear, "_", "historicbaydata_fishingareas_summary", ".csv") },
    content = function(file) { fwrite(historicbaydata_fishingareas_summary, file) }
  )
  
  output$downloadData7 <- downloadHandler(
    filename = function() { paste0(monthname, "_", thisyear, "_", "historicalmeans_wb", ".csv") },
    content = function(file) { fwrite(historicalmeans_wb, file) }
  )
  
  output$downloadData8 <- downloadHandler(
    filename = function() { paste0(monthname, "_", thisyear, "_", "historicbaydata_summary", ".csv") },
    content = function(file) { fwrite(historicbaydata_summary, file) }
  )
  
  output$MapData1 <- downloadHandler(
    filename = function() { paste0(monthname, "_", thisyear, "_", "fishingareacoordinates", ".csv") },
    content = function(file) { fwrite(fishingareacoordinates, file) }
  ) 
  
  output$MapData2 <- downloadHandler(
    filename = function() { paste0(monthname, "_", thisyear, "_", "chesapeakebaycoordinates", ".csv") },
    content = function(file) { fwrite(chesapeakebaycoordinates, file) }
  )
  
  ##############################################################################

  #this second section operates the leaflet map and pie chart interactivity:
  
  #make the map reactive
  rv <- reactiveValues(selected_color = NULL, active_layer = "Fishing Area Suitability")
  
  #this makes our hot spot pie chart  
  output$HotSpotPie <- renderPlotly({
    p <- plot_ly(fishinghotspotssummary, 
                 labels = ~habitat, 
                 values = ~percent, 
                 type = 'pie', 
                 source = 'HotSpotPie', 
                 textposition = 'outside', 
                 textinfo = 'label+percent', 
                 customdata = ~color,
                 marker = list(colors = fishinghotspotssummary$color)) %>%
      config(displayModeBar=T, displaylogo=F, 
             toImageButtonOptions= list(filename = 'Fishing Area Pie Chart', width = 500, height = 500)) %>%
      layout(title = "Fishing Area Habitat Suitability",
             margin = list(l=50, r=50, b=50, t=50, pad=20),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    event_register(p, "plotly_click")
    p
  })
  
  #this makes our whole bay pie chart
  output$WholeBayPie <- renderPlotly({
    p <- plot_ly(wholebaybottomsummary, 
                 labels = ~habitat, 
                 values = ~percent, 
                 type = 'pie',
                 textposition = 'inside',
                 source = 'WholeBayPie', 
                 textinfo = 'label+percent',
                 customdata = ~color,
                 marker = list(colors = wholebaybottomsummary$color)) %>%
      config(displayModeBar=T, displaylogo=F, 
             toImageButtonOptions= list(filename = 'Maryland Bay Pie Chart', width = 500, height = 500)) %>%
      layout(title = 'Maryland Bay Habitat Suitability',
             margin = list(l=50, r=50, b=50, t=50, pad=70),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    event_register(p, "plotly_click")
    p
  })
  
  #this generates our map
  output$BayMap <- renderLeaflet({
    baymap <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(lng = -76.3, lat = 38.5, zoom = 8) %>%
      addPolygons(
        data = fishingareapolygons.dd, color = "#8373e2", stroke = 0.2, opacity = 0.8,
        label = fishingareapolygons.dd$name, group = "Fishing Areas") %>%
      addCircles(
        data = fishingareacoords.dd_bottom, color = ~color, group = "Fishing Area Habitat Suitability",
        label = paste(fishingareacoords.dd_bottom$name, fishingareacoords.dd_bottom$Sdepth, "ft", sep=" ")) %>%
      addCircles(data = mddatathiscruise.dd_bottom, color = ~color, group = "Maryland Bay Habitat Suitability") %>%
      addLayersControl(
        overlayGroups = c("Fishing Areas", "Fishing Area Habitat Suitability", "Maryland Bay Habitat Suitability"),
        options = layersControlOptions(collapsed = T)) %>%
      hideGroup(c("Fishing Areas", "Maryland Bay Habitat Suitability"))
    baymap
  })
  
  #this observes when we click on the pie slice we want to see for the hot spot pie
  observeEvent(event_data("plotly_click", source = "HotSpotPie"), {
    click_data <- event_data("plotly_click", source = "HotSpotPie")
    rv$selected_color <- click_data[["customdata"]]
  })
  
  #this observes when we click on the pie slice we want to see for the whole bay pie
  observeEvent(event_data("plotly_click", source = "WholeBayPie"), {
    click_data <- event_data("plotly_click", source = "WholeBayPie")
    rv$selected_color <- click_data[["customdata"]]
  })
  
  #observes for the map layers:
  observeEvent(input$layer, {
    rv$active_layer <- input$layer
    rv$selected_color <- NULL  # Optionally reset filter when switching layers

    leafletProxy("BayMap") %>%
      hideGroup(c("Fishing Area Habitat Suitability", "Maryland Bay Habitat Suitability")) %>%
      showGroup(input$layer)
  })
  
  #this observes which layer we're on -- the fishing hotspots, or the whole bay
  #and adjusts which points are shown
  observeEvent({
    rv$selected_color
    rv$active_layer
  }, {
    leafletProxy("BayMap") %>%
      clearGroup("Fishing Area Habitat Suitability") %>%
      clearGroup("Maryland Bay Habitat Suitability")

    if (rv$active_layer == "Fishing Area Habitat Suitability") {
      pts <- fishingareacoords.dd_bottom
      if (!is.null(rv$selected_color)) {
        pts <- pts %>% filter(color == rv$selected_color)
      }
      leafletProxy("BayMap") %>%
        addCircles(data = pts, color = ~color, group = "Fishing Area Habitat Suitability", label = paste(fishingareacoords.dd_bottom$name, fishingareacoords.dd_bottom$Sdepth, "ft", sep=" "))
    } else if (rv$active_layer == "Maryland Bay Habitat Suitability") {
      pts <- mddatathiscruise.dd_bottom
      if (!is.null(rv$selected_color)) {
        pts <- pts %>% filter(color == rv$selected_color)
      }
      leafletProxy("BayMap") %>%
        addCircles(., data = pts, color = ~color, group = "Maryland Bay Habitat Suitability", label = ~habitat)
    }

  })
  
  ##############################################################################
  
  #this next section contains the cross-section images:
  
  output$WholeBayCrossSection <- renderPlotly({
    mainchannelplotly<-plot_ly()%>%
      config(displayModeBar=T, displaylogo=F, modeBarButtonsToRemove = c("zoom", "autoScale2d","toggleSpikelines","select2d","lasso2d", "zoomin", "zoomOut", "pan"),
             toImageButtonOptions= list(filename = 'Maryland Bay Cross-Section', width = 1000, height = 750)) %>%
      add_trace(x=mainchanneldata$distfrommouth ,y=mainchanneldata$Sdepth
                ,type='scatter',mode='markers'
                ,text = paste0(mainchanneldata$Sdepth, "ft ", mainchanneldata$Wtemp, "F ", mainchanneldata$DO, "mg/L ")
                ,hoverinfo = 'text'
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
      layout(xaxis=list(title="Distance from MD-VA Line (miles)",autorange="reversed", zeroline=F, showgrid=F))%>%
      layout(yaxis=list(title="Depth (ft)",autorange="reversed", zeroline=F, showgrid=F)) %>%
      layout(title = 'MD Bay Mainstem Cross Section', margin = list(l=50, r=50, b=50, t=50, pad=20)
             # ,annotations = list(x = 0.25, y = 0.05, text = "Data not collected [-] for [Month] due to [-].",
             #                    showarrow = F, xref='paper', yref='paper',
             #                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
             #                    font=list(size=12))
             )
    mainchannelplotly
  })
  
  output$PotomacCrossSection <- renderPlotly({
    potomacchannelplotly <- plot_ly()%>%
      config(displayModeBar=T, displaylogo=F, modeBarButtonsToRemove = c("zoom", "autoScale2d","toggleSpikelines","select2d","lasso2d", "zoomin", "zoomOut", "pan"),
             toImageButtonOptions= list(filename = 'Potomac Cross-Section', width = 1000, height = 750)) %>%
      add_trace(x=potomacchanneldata$distfrommouth,y=potomacchanneldata$Sdepth
                ,type='scatter',mode='markers'
                ,text = paste0(potomacchanneldata$Sdepth, "ft ", potomacchanneldata$Wtemp, "F ", potomacchanneldata$DO, "mg/L ")
                ,hoverinfo = 'text'
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
      layout(title = 'Potomac River Cross Section', margin = list(l=50, r=50, b=50, t=50, pad=20)
             # ,annotations = list(x = 0.3, y = 0, text = "Data not collected [-] for [Month] due to [-].",
             #                    showarrow = F, xref='paper', yref='paper',
             #                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
             #                    font=list(size=12))
             )
    potomacchannelplotly
  })
  
  ##############################################################################

  #final section is the historical data
  
  output$HotSpot10yrs <- renderPlotly({
    plot_ly(historicbaydata_fishingareas_summary, x = ~year, y = ~percent, color = ~Habitat, colors = suitability_colors,
            type = 'bar') %>%
      config(displayModeBar=T, displaylogo=F, 
             modeBarButtonsToRemove = c("zoom", "autoScale2d","toggleSpikelines","select2d","lasso2d", "zoomin", "zoomOut", "pan"),
             toImageButtonOptions= list(filename = 'Hot Spot Suitability 10 Yrs', width = 800, height = 500)) %>%
      layout(title = paste('Fishing Area Habitat Suitability for', lateorearly, monthname, 'Ten Year History', sep = " "),
             yaxis = list(title = 'Percent of Habitat'),
             barmode = 'stack',
             margin = list(b = 100)
             ,annotations = list(x = 1, y = -0.27, text = "Data not collected in 2020 due to Covid-19 Pandemic.",
                                showarrow = F, xref='paper', yref='paper',
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=11))
             )
  })
  
  output$HotSpotVolume <- renderPlot({
    historicmeans_hs_plot <-ggplot(historicalmeans_hs, aes(x=as.factor(monthseq)))+
      geom_ribbon(mapping=aes(x=monthseq, ymin=mindatahs, ymax=maxdatahs, fill='ribbon'), alpha=0.1)+
      geom_point(mapping=aes(y=meansuitability, color=timekey))+
      geom_line(mapping=aes(x=monthseq, y=meansuitability, color=timekey))+
      theme_classic()+
      scale_x_discrete(breaks=c(breaks), labels=c(labels))+
      scale_color_manual(name="Suitability Dataset", values=c("dodgerblue", "darkgreen"), labels=c("Current Year", "Historic Mean (1985-24)"))+
      scale_fill_manual(name="Suitability Dataset", values=c("darkgreen"), labels=c("Historic Range (1985-24)"))+
      xlab("Month")+
      ylab("Percent of Suitable Habitat")+
      theme(text=element_text(size=14, family=verdana),
            legend.position=c(0.15, 0.2),
            legend.text=element_text(size=10),
            legend.title=element_text(size=11),
            axis.text.x=element_text(angle=45, hjust=1))
    historicmeans_hs_plot
  })
  
  output$WholeBay10yrs <- renderPlotly({
    plot_ly(historicbaydata_summary, x = ~year, y = ~percent, color = ~Habitat, colors = suitability_colors,
            type = 'bar') %>%
      config(displayModeBar=T, displaylogo=F, 
             modeBarButtonsToRemove = c("zoom", "autoScale2d","toggleSpikelines","select2d","lasso2d", "zoomin", "zoomOut", "pan"),
             toImageButtonOptions= list(filename = 'Maryland Bay Habitat Suitability 10 Yrs', width = 800, height = 500)) %>%
      layout(title = paste('Maryland Bay Habitat Suitability for', lateorearly, monthname, 'Ten Year History', sep = " "),
             yaxis = list(title = 'Percent of Habitat'),
             barmode = 'stack',
             margin = list(b = 100)
             ,annotations = list(x = 1.1, y = -0.27, text = "Data not collected in 2020 due to Covid-19 Pandemic.",
                                showarrow = F, xref='paper', yref='paper',
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=11))
             )
  })
  
  output$WholeBayVolume <- renderPlot({
    historicmeans_wb_plot <-ggplot(historicalmeans_wb, aes(x=as.factor(monthseq)))+
      geom_ribbon(mapping=aes(x=monthseq, ymin=mindatawb, ymax=maxdatawb, fill='ribbon'), alpha=0.1)+
      geom_point(mapping=aes(y=meansuitability, color=timekey))+
      geom_line(mapping=aes(x=monthseq, y=meansuitability, color=timekey))+
      theme_classic()+
      scale_x_discrete(breaks=c(breaks), labels=c(labels))+
      scale_color_manual(name="Suitability Dataset", values=c("dodgerblue", "darkgreen"), labels=c("Current Year", "Historic Mean (1985-24)"))+
      scale_fill_manual(name="Suitability Dataset", values=c("darkgreen"), labels=c("Historic Range (1985-24)"))+
      xlab("Month")+
      ylab("Percent of Suitable Habitat")+
      theme(text=element_text(size=14, family=verdana),
            legend.position=c(0.15, 0.2),
            legend.text=element_text(size=10),
            legend.title=element_text(size=11),
            axis.text.x=element_text(angle=45, hjust=1))
    historicmeans_wb_plot
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
