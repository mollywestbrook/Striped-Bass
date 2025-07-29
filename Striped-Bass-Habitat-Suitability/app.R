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

thiscruise <- "BAY893.csv"

#late or early? Update
lateorearly <- "Late"

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

verdana <- 'verdana'

#Bring in objects

files <- list.files(pattern = "wholebaysummary")
wholebaybottomsummary <- read_csv(files[1])

files <- list.files(pattern = "fishinghotspotsummary")
fishinghotspotssummary <- read_csv(files[1])

files <- list.files(pattern = "historicbaydatafishingareassummary")
historicbaydata_fishingareas_summary <- read_csv(files[1])

historicbaydata_fishingareas_summary$Habitat <- factor(historicbaydata_fishingareas_summary$Habitat, levels = c("Suitable", "Tolerable", "Marginal", "Unsuitable"))

files <- list.files(pattern = "historicbaydatasummary")
historicbaydata_summary <- read_csv(files[1])

historicbaydata_summary$Habitat <- factor(historicbaydata_summary$Habitat, levels = c("Suitable", "Tolerable", "Marginal", "Unsuitable"))

files <- list.files(pattern = "mainchanneldata")
mainchanneldata <- read_csv(files[1])

files <- list.files(pattern = "labels_mainstem")
labels_mainstem <- read_csv(files[1])

files <- list.files(pattern = "potomacchanneldata")
potomacchanneldata <- read_csv(files[1])

files <- list.files(pattern = "labels_potomac")
labels_potomac <- read_csv(files[1])

files <- list.files(pattern = "historicalmeans_hs")
historicalmeans_hs <- read_csv(files[1])

labels <- historicalmeans_hs$months
breaks <- historicalmeans_hs$monthseq

files <- list.files(pattern = "historicalmeans_wb")
historicalmeans_wb <- read_csv(files[1])

labels <- historicalmeans_wb$months
breaks <- historicalmeans_wb$monthseq

fishingareapolygons.dd <- st_read(here("Striped-Bass-Habitat-Suitability", "FishingAreaPolygons"))

fishingareacoords.dd_bottom <- st_read(here("Striped-Bass-Habitat-Suitability", "FishingAreaQuality"))

mddatathiscruise.dd_bottom <- st_read(here("Striped-Bass-Habitat-Suitability", "WholeBayQuality"))


############ UI ######################

#This generates the layout of our app
#And all of the interactive features of the widgets

ui <- fluidPage(
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
  titlePanel(paste("Striped Bass Habitat Suitability for", lateorearly, monthname, thisyear, sep=' ')),
  
  layout_columns(
    accordion(
      accordion_panel(
        title = "App Information and Instructions",
        layout_columns(
          card(
            card_header("App Instructions"),
            p(uiOutput("HowToUse"))
          ),
          navset_card_tab( 
            nav_panel("Legend", imageOutput('SuitableCriteria')), 
            nav_panel("Striped Bass Squeeze", imageOutput('StripedBassSqueeze')), 
            nav_panel("More Info", uiOutput('DNRLinks'))
          )
        )
      ),
      open=F
    )
  ),
  
  card(
    card_header("Chesapeake Bay Bottom Map"),
    fluidRow(
      column(8, leafletOutput("BayMap", height = 600)),
      column(4,
             selectInput("layer", "Toggle to Change Layer", choices = c("Fishing Area Habitat Suitability", "Whole Bay Habitat Suitability"), selected = "Fishing Area Habitat Suitability"),
             conditionalPanel("input.layer == 'Fishing Area Habitat Suitability'", plotlyOutput("HotSpotPie")),
             conditionalPanel("input.layer == 'Whole Bay Habitat Suitability'", plotlyOutput("WholeBayPie"))
      )
    )
  ),
  
  layout_columns(
    navset_card_tab(
      nav_panel("Cross-Section of the Mainstem", plotlyOutput(outputId = 'WholeBayCrossSection')), 
      nav_panel("Cross-Section of the Potomac", plotlyOutput(outputId = 'PotomacCrossSection'))
    )
  ),
  
  layout_columns(
    navset_card_tab(
      nav_panel("Fishing Area Habitat Suitability, Last 10 Yrs", plotlyOutput(outputId = "HotSpot10yrs")), 
      nav_panel("Fishing Area Mean Habitat Suitability vs Historical Average", plotOutput(outputId = "HotSpotVolume"))
    ),
    navset_card_tab(
      nav_panel("Whole Bay Habitat Suitability, Last 10 Yrs", plotlyOutput(outputId = "WholeBay10yrs")), 
      nav_panel("Whole Bay Mean Habitat Suitability vs Historical Average", plotOutput(outputId = "WholeBayVolume"))
    )
  )
)

############### SERVER ##########################

#This generates all the data and figures

server <- function(input, output, session) {
  
  #this first section contains the app instructions + information
  
  #app instructions
  output$HowToUse <- renderUI({
    HTML(paste(
      "<p>This dashboard provides habitat suitability information for striped bass in the Chesapeake Bay.</p>",
      
      "<p>To understand how bass habitat may change over the year, select 'Striped Bass Squeeze' in the panel to the right. </p>",
      
      "<p> The following list walks through each panel of information to best predict where you may find striped bass at this time of the year. </p>",
      
      "<ul>",
      "<li> The map displays up to three layers: fishing area locations, habitat suitability in fishing areas, and habitat suitability across the entire bay.</li>",
      "<li> By default, the fishing area habitat suitability is displayed. </li>",
      "<li> To activate the fishing area locations, you may check 'Fishing Areas' in the layer panel in the upper right corner of the map. You can display these over the whole bay data, or just the fishing area data. </li>",
      "<li> Select which layer (fishing areas or the whole bay) you'd like to see by selecting the layer under the 'Toggle to Change' tab.</li>",
      "<li> You can also filter locations by suitability criteria by selecting the slice of the displayed summary pie chart to display only locations corresponding to that suitability. </li>",
      "<li> With this, anglers can find the best possible locations for fishing for bass based on measured data!</li>",
      "<li> For legend information on how we define suitable habitat for striped bass, see the legend in the panel to the right. </li>",
      "<li> The map displays bottom data (<1ft) only. Beneath the map find the main bay and Potomac river depth habitat suitability.</li>",
      "<li> Finally, find how this year's data corresponds to historical data at the bottom of the app.</li>",
      "</ul>",
      
      "For more information, see the links to other DNR resources in the panel to the right.",
      sep=""
    ))
  })
  
  #render suitable criteria legend
  output$SuitableCriteria <- renderImage({
    filename <- normalizePath(file.path(here('Striped-Bass-Habitat-Suitability', 'Bass Suitable Criteria.png')))
    list(src = filename, alt = "Striped Bass Habitat Suitability Criteria", width="100%")
  }, deleteFile = FALSE)
  
  #render striped bass squeeze image
  output$StripedBassSqueeze <- renderImage({
    filename <- normalizePath(file.path(here('Striped-Bass-Habitat-Suitability', 'Striped Bass Squeeze.png')))
    list(src = filename, alt = "Striped Bass Squeeze", width="100%")
  }, deleteFile = FALSE)
  
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
             toImageButtonOptions= list(filename = 'Whole Bay Pie Chart', width = 500, height = 500)) %>%
      layout(title = 'Fishing Area Habitat Suitability',
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
                 textposition = 'outside',
                 source = 'WholeBayPie', 
                 textinfo = 'label+percent',
                 customdata = ~color,
                 marker = list(colors = wholebaybottomsummary$color)) %>%
      config(displayModeBar=T, displaylogo=F, 
             toImageButtonOptions= list(filename = 'Whole Bay Pie Chart', width = 500, height = 500)) %>%
      layout(title = 'Whole Bay Habitat Suitability',
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
      addCircles(data = mddatathiscruise.dd_bottom, color = ~color, group = "Whole Bay Habitat Suitability") %>%
      addLayersControl(
        overlayGroups = c("Fishing Areas", "Fishing Area Habitat Suitability", "Whole Bay Habitat Suitability"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Fishing Areas", "Whole Bay Habitat Suitability"))
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
      hideGroup(c("Fishing Area Habitat Suitability", "Whole Bay Habitat Suitability")) %>%
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
      clearGroup("Whole Bay Habitat Suitability")

    if (rv$active_layer == "Fishing Area Habitat Suitability") {
      pts <- fishingareacoords.dd_bottom
      if (!is.null(rv$selected_color)) {
        pts <- pts %>% filter(color == rv$selected_color)
      }
      leafletProxy("BayMap") %>%
        addCircles(data = pts, color = ~color, group = "Fishing Area Habitat Suitability", label = paste(fishingareacoords.dd_bottom$name, fishingareacoords.dd_bottom$Sdepth, "ft", sep=" "))
    } else if (rv$active_layer == "Whole Bay Habitat Suitability") {
      pts <- mddatathiscruise.dd_bottom
      if (!is.null(rv$selected_color)) {
        pts <- pts %>% filter(color == rv$selected_color)
      }
      leafletProxy("BayMap") %>%
        addCircles(., data = pts, color = ~color, group = "Whole Bay Habitat Suitability", label = ~habitat)
    }

  })
  
  #this next section contains the cross-section images:
  
  output$WholeBayCrossSection <- renderPlotly({
    mainchannelplotly<-plot_ly()%>%
      config(displayModeBar=T, modeBarButtonsToRemove = c("zoom", "autoScale2d","toggleSpikelines","select2d","lasso2d"),
             toImageButtonOptions= list(filename = 'Whole Bay Cross-Section', width = 1000, height = 750)) %>%
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
      layout(title = 'Bay Mainstem Cross Section', margin = list(l=50, r=50, b=50, t=50, pad=20))
    mainchannelplotly
  })
  
  output$PotomacCrossSection <- renderPlotly({
    potomacchannelplotly <- plot_ly()%>%
      config(displayModeBar=T, modeBarButtonsToRemove = c("autoScale2d","hoverCompareCartesian","toggleSpikelines","select2d","lasso2d"),
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
      layout(title = 'Potomac River Cross Section', margin = list(l=50, r=50, b=50, t=50, pad=20))
    potomacchannelplotly
  })

#final section is the historical data
  
  output$HotSpot10yrs <- renderPlotly({
    plot_ly(historicbaydata_fishingareas_summary, x = ~year, y = ~percent, color = ~Habitat, colors = suitability_colors,
            type = 'bar') %>%
      config(displayModeBar=T, displaylogo=F, 
             modeBarButtonsToRemove = c("autoScale2d","hoverCompareCartesian","toggleSpikelines","select2d","lasso2d"),
             toImageButtonOptions= list(filename = 'Hot Spot Suitability 10 Yrs', width = 800, height = 500)) %>%
      layout(title = paste('Fishing Area Habitat Suitability for', lateorearly, monthname, 'Ten Year History', sep = " "),
             yaxis = list(title = 'Percent of Habitat'),
             barmode = 'stack',
             margin = list(b = 100)
             # ,annotations = list(x = 1.1, y = -0.27, text = "Data not collected in 2020 due to Covid-19 Pandemic.", 
             #                    showarrow = F, xref='paper', yref='paper', 
             #                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
             #                    font=list(size=11))
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
      ylab("Suitable Percent of Habitat")+
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
             modeBarButtonsToRemove = c("autoScale2d","hoverCompareCartesian","toggleSpikelines","select2d","lasso2d"),
             toImageButtonOptions= list(filename = 'Whole Bay Habitat Suitability 10 Yrs', width = 800, height = 500)) %>%
      layout(title = paste('Whole Bay Habitat Suitability for', lateorearly, monthname, 'Ten Year History', sep = " "),
             yaxis = list(title = 'Percent of Habitat'),
             barmode = 'stack',
             margin = list(b = 100)
             # ,annotations = list(x = 1.1, y = -0.27, text = "Data not collected in 2020 due to Covid-19 Pandemic.", 
             #                    showarrow = F, xref='paper', yref='paper', 
             #                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
             #                    font=list(size=11))
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
      ylab("Suitable Percent of Habitat")+
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
