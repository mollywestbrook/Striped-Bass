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

#### UPDATE THIS WITH CURRENT CRUISE FILE

thiscruise <- "BAY844.csv"

#########################################

#variables the app needs


#What cruise are we working with? Update for this month's
rawcruisedata<-read_csv(thiscruise)

###Ensure the data is confined to a single month, otherwise filter out extraneous dates
startdate<-min(rawcruisedata$Date)
enddate<-max(rawcruisedata$Date)

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

#Bring in objects


files <- list.files(pattern = "wholebaysummary")
wholebaysurfacesummary <- read_csv(files[1])

files <- list.files(pattern = "fishinghotspotsummary")
fishinghotspotssummary <- read_csv(files[1])

files <- list.files(pattern = "historicbaydatafishingareassummary")
historicbaydata_fishingareas_summary <- read_csv(files[1])

files <- list.files(pattern = "historicbaydatasummary")
historicbaydata_summary <- read_csv(files[1])

files <- list.files(pattern = "mainchanneldata")
mainchanneldata <- read_csv(files[1])

files <- list.files(pattern = "potomacchanneldata")
potomacchanneldata <- read_csv(files[1])

fishingareapolygons.dd <- st_read(here("Striped-Bass-Habitat-Suitability", "FishingAreaPolygons"))

fishingareacoords.dd_surface <- st_read(here("Striped-Bass-Habitat-Suitability", "FishingAreaQuality"))

mddatathiscruise.dd_surface <- st_read(here("Striped-Bass-Habitat-Suitability", "WholeBayQuality"))


############ UI ######################

#This generates the layout of our app
#And all of the interactive features of the widgets

ui <- fluidPage(
    theme = bs_theme(preset = "flatly"),

    # Application title
    titlePanel(paste("Striped Bass Habitat Suitability for", monthname, thisyear, sep=' ')),
    
    card(
    card_header("Chesapeake Bay Map"),
    fluidRow(
      column(8, leafletOutput("BayMap", height = 600)),
      column(4,
             selectInput("layer", "Active Layer", choices = c("Fishing Area Suitability", "Whole Bay Suitability"), selected = "Fishing Area Suitability"),
             conditionalPanel("input.layer == 'Fishing Area Suitability'", plotlyOutput("HotSpotPie")),
             conditionalPanel("input.layer == 'Whole Bay Suitability'", plotlyOutput("WholeBayPie"))
            )
    )
    ),
    
    layout_columns(
      card(
        card_header("How To Use This App"),
        p(uiOutput("HowToUse"))
      ),
      navset_card_tab( 
        nav_panel("Legend", imageOutput('SuitableCriteria')), 
        nav_panel("Striped Bass Squeeze", imageOutput('StripedBassSqueeze')), 
        nav_panel("More Info", "DNR Links", uiOutput('DNRLinks'))
    )
    ),
    
    layout_columns(
      navset_card_tab(
        nav_panel("Hot Spot Suitability, Last 10 Yrs", plotlyOutput(outputId = "HotSpot10yrs"))#, 
        #nav_panel("Hot Spot Mean Suitability vs Historical Average", plotlyOutput(outputId = "HotSpotVolume")) #MW missing data for this line
      ),
      navset_card_tab(
        nav_panel("Total Bay Suitability, Last 10 Yrs", plotlyOutput(outputId = "WholeBay10yrs")), 
        nav_panel("Hot Spot Mean Suitability vs Historical Average", plotlyOutput(outputId = "WholeBayVolume"))
      )
    ),
    
    layout_columns(
      navset_card_tab(
        nav_panel("Cross-Section of the Main Stem", plotlyOutput(outputId = 'WholeBayCrossSection')), 
        nav_panel("Cross-Section of the Potomac", plotlyOutput(outputId = 'PotomacCrossSection'))
      )
    )
  )

############### SERVER ##########################

#This generates all the data and figures

server <- function(input, output, session) {
  
  #make the map reactive
  rv <- reactiveValues(selected_color = NULL, active_layer = "Fishing Area Suitability")
  
  #this generates our map
  output$BayMap <- renderLeaflet({
    leaflet() %>%
      leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      setView(lng = -76.3, lat = 39.2, zoom = 9) %>%
      addPolygons(
        data = fishingareapolygons.dd, color = "#8373e2", stroke = 0.2, opacity = 0.8,
        label = fishingareapolygons.dd$name, group = "Fishing Areas") %>%
      addCircles(
        data = fishingareacoords.dd_surface, color = ~color, group = "Fishing Area Suitability",
        label = paste(fishingareacoords.dd_surface$name, fishingareacoords.dd_surface$habitat, sep=", ")) %>%
      addCircles(data = mddatathiscruise.dd_surface, color = ~color, group = "Whole Bay Suitability") %>%
      addLayersControl(
        overlayGroups = c("Fishing Areas", "Fishing Area Suitability", "Whole Bay Suitability"),
        options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(c("Fishing Areas", "Whole Bay Suitability"))
  })
  
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
      layout(title = 'Fishing Hotspots Habitat Suitability',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    event_register(p, "plotly_click")
    p
  })
  
  #this makes our whole bay pie chart
  output$WholeBayPie <- renderPlotly({
    p <- plot_ly(wholebaysurfacesummary, 
                           labels = ~habitat, 
                           values = ~percent, 
                           type = 'pie',
                           textposition = 'outside',
                           source = 'WholeBayPie', 
                           textinfo = 'label+percent',
                           customdata = ~color,
                           marker = list(colors = wholebaysurfacesummary$color)) %>%
      layout(title = 'Whole Bay Habitat Suitability',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    event_register(p, "plotly_click")
    p
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
      hideGroup(c("Fishing Area Suitability", "Whole Bay Suitability")) %>%
      showGroup(input$layer)
  })
  
  #this observes which layer we're on -- the fishing hotspots, or the whole bay
  #and adjusts which points are shown
  observeEvent({
    rv$selected_color
    rv$active_layer
  }, {
    leafletProxy("BayMap") %>%
      clearGroup("Fishing Area Suitability") %>%
      clearGroup("Whole Bay Suitability")
      
        if (rv$active_layer == "Fishing Area Suitability") {
          pts <- fishingareacoords.dd_surface
          if (!is.null(rv$selected_color)) {
            pts <- pts %>% filter(color == rv$selected_color)
          }
          leafletProxy("BayMap") %>%
            addCircles(data = pts, color = ~color, group = "Fishing Area Suitability", label = ~habitat)
        } else if (rv$active_layer == "Whole Bay Suitability") {
          pts <- mddatathiscruise.dd_surface
          if (!is.null(rv$selected_color)) {
            pts <- pts %>% filter(color == rv$selected_color)
          }
          leafletProxy("BayMap") %>%
            addCircles(., data = pts, color = ~color, group = "Whole Bay Suitability", label = ~habitat)
        }

  })
  
  output$HowToUse <- renderUI({
    HTML(paste(
    "<p>This dashboard provides habitat suitability information for Striped Bass.</p>",
    
    "<p>To understand how Bass habitat may change over the year, select 'Striped Bass Squeeze' in the panel to the right. </p>",
    
    "<p> The following list walks through each panel of information to best predict where you may find Striped Bass at this time of the year. </p>",
    
    "<ul>",
    "<li> The map displays up to three layers: fishing hotspot locations, habitat suitability in fishing hot spots, and habitat suitability across the entire bay.</li>",
    "<li> By default, the fishing hot spot habitat suitability is displayed. </li>",
    "<li> To activate the fishing hot spot locations, hover over the map's layer panel. You can display these over the whole bay data, or just the hot spot data. </li>",
    "<li> Select which layer you'd like to see by selecting the layer under the 'active layer' tab.</li>",
    "<li> You can also filter locations by suitability criteria by selecting the slice of the displayed summary pie chart to display only locations corresponding to that suitability. </li>",
    "<li> Now, anglers can find the best possible locations for fishing for Bass based on measured data!</li>",
    "<li> For legend information on how we define suitable habitat for striped bass, see the legend in the panel to the right. </li>",
    "<li> Beneath the map, find how this year's data corresponds to historical data.</li>",
    "<li> Finally, the map displays surface data (>0.5m) only. To find the full channel depth suitability, scroll to the bottom for the main channel and Potomac river depth suitability. </li>",
    "</ul>",
    
    "For more information, see the links to other DNR resources in the panel to the right.",
    sep=""
    ))
  })
    
  output$SuitableCriteria <- renderImage({
    filename <- normalizePath(file.path(here('Striped-Bass-Habitat-Suitability', 'Bass Suitable Criteria.png')))
    list(src = filename, alt = "Striped Bass Suitability Criteria", width=600)
  }, deleteFile = FALSE)

  output$StripedBassSqueeze <- renderImage({
    filename <- normalizePath(file.path(here('Striped-Bass-Habitat-Suitability', 'Striped Bass Squeeze.png')))
    list(src = filename, alt = "Striped Bass Squeeze", width=600)
  }, deleteFile = FALSE)
  
  output$DNRLinks <- renderUI({
    HTML(paste(
      "<p>DNR Links for More Information:</p>",
      "<ul>",
      "<li> <a href=", "https://eyesonthebay.dnr.maryland.gov/", "> Water Quality Information: Eyes on the Bay</a></li>",
      "</ul>",
      "<p>Striped Bass Habitat Criteria:</p>",
      "<ul>",
      "<li> <a href=", "https://eyesonthebay.dnr.maryland.gov/eyesonthebay/documents/DevelopmentOfTemperatureAndDOBasedHabitatRequirements.pdf", "> Development of Habitat Conditions</a>, pg 136-144</li>",
      "<li> <a href=", "https://eyesonthebay.dnr.maryland.gov/eyesonthebay/documents/ImpactsOfClimateChangeOnStripedBassHabitat2023.pdf", "> Climate CHange and Resident Chesapeake Bay Striped Bass Habitat</a>, slides 1-15</li>",
      "</ul>",
      sep=""
    ))
  })

  output$HotSpot10yrs <- renderPlotly({
    plot_ly(historicbaydata_fishingareas_summary, x = ~year, y = ~percent, color = ~Habitat, colors = suitability_colors,
            type = 'bar') %>%
      layout(title = 'Fishing Hot Spot Suitability for the Previous Ten Years',
             yaxis = list(title = 'Percent of Habitat'),
             barmode = 'stack')
  })
  
  output$WholeBay10yrs <- renderPlotly({
    plot_ly(historicbaydata_summary, x = ~year, y = ~percent, color = ~Habitat, colors = suitability_colors,
            type = 'bar') %>%
      layout(title = 'Whole Bay Suitability for the Previous Ten Years',
             yaxis = list(title = 'Percent of Habitat'),
             barmode = 'stack')
  })
  
  output$WholeBayVolume <- renderPlotly({
    plot_ly()%>%
      partial_bundle()%>%
      config(displayModeBar=FALSE, modeBarButtonsToRemove = c("autoScale2d","hoverCompareCartesian","toggleSpikelines","select2d","lasso2d")) %>%
      add_trace(x=months,y=maxdata,type='scatter',mode='lines',line = list(color = 'white'),showlegend = FALSE) %>%
      add_trace(x=months,y = mindata, type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'white'),
                name = '1985-2022 Range')%>%
      add_trace(x=months,y=percentsuitable,type="scatter",mode='lines+markers',name="Percent Suitable Volume 2023",line=list(color="dodgerblue"),marker=list(color="dodgerblue"))%>%
      add_trace(x=months,y=meandata,type="scatter",mode='lines',name="Mean Percent Suitable Volume 1985-2022",line=list(color="darkgreen"))%>%
      layout(xaxis = list(categoryorder = "array",
                          categoryarray = months)) %>%
      layout(legend =list(x=0,y=0.05))%>%
      layout(showlegend = TRUE, legend = list(font = list(size = 8)))
  })
  
  output$WholeBayCrossSection <- renderPlotly({
    plot_ly()%>%
      config(displayModeBar=FALSE, modeBarButtonsToRemove = c("autoScale2d","hoverCompareCartesian","toggleSpikelines","select2d","lasso2d")) %>%
      add_trace(x=mainchanneldata$milesY,y=mainchanneldata$Sdepth
                ,hoverinfo="none"
                ,type='scatter',mode='markers'
                ,marker=list(color=mainchanneldata$color))%>%
      layout(xaxis=list(title="Distance from mouth of Bay (miles)",autorange="reversed"))%>%
      layout(yaxis=list(title="Depth (ft)",autorange="reversed"))
  })
  
  output$PotomacCrossSection <- renderPlotly({
    plot_ly()%>%
      config(displayModeBar=FALSE, modeBarButtonsToRemove = c("autoScale2d","hoverCompareCartesian","toggleSpikelines","select2d","lasso2d")) %>%
      add_trace(x=potomacchanneldata$milesY,y=potomacchanneldata$Sdepth
                ,hoverinfo="none"
                ,type='scatter',mode='markers'
                ,marker=list(color=potomacchanneldata$color))%>%
      layout(xaxis=list(title="Distance from mouth of Potomac (miles)",autorange="reversed",zeroline=FALSE))%>%
      layout(yaxis=list(title="Depth (ft)",autorange="reversed")) %>%
      layout(title = 'Potomac River Main Channel Cross Section')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
