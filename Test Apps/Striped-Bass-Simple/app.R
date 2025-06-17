#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

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

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(preset = "flatly"),
  
  # Application title
  titlePanel(paste("Striped Bass Habitat Suitability for", monthname, thisyear, sep=' ')),

  layout_columns(
    card(
      card_header("How To Use This App"),
      p("This dashboard provides helpful information on the most suitable fishing locations for Striped Bass.")
    ),
    navset_card_tab( 
      nav_panel("Legend", imageOutput('SuitableCriteria')), 
      nav_panel("Striped Bass Squeeze", imageOutput('StripedBassSqueeze')), 
      nav_panel("More Info", "DNR Links", verbatimTextOutput('DNRLinks'))
    )
  ),
  
  layout_columns(
    card(
      card_header("Chesapeake Bay Map"),
      leafletOutput("BayMap")
    )
  ),
  
  layout_columns(
    navset_card_tab(
      nav_panel("Hot Spot Surface Pie Chart", plotlyOutput(outputId = "HotSpotPie")),
      nav_panel("Hot Spot Suitability, Last 10 Yrs", plotlyOutput(outputId = "HotSpot10yrs"))#, 
      #nav_panel("Hot Spot Mean Suitability vs Historical Average", plotlyOutput(outputId = "HotSpotVolume")) #MW missing data for this line
    ),
    navset_card_tab(
      nav_panel("Whole Bay Surface Pie Chart", plotlyOutput(outputId = "WholeBayPie")),
      nav_panel("Total Bay Suitability, Last 10 Yrs", plotlyOutput(outputId = "WholeBay10yrs")), 
      nav_panel("Hot Spot Mean Suitability vs Historical Average", plotlyOutput(outputId = "WholeBayVolume"))
    )
  ),
  
  layout_columns(
    navset_card_tab(
      nav_panel("Cross-Section of the Whole Bay", plotlyOutput(outputId = 'WholeBayCrossSection')), 
      nav_panel("Cross-Section of the Potomac", plotlyOutput(outputId = 'PotomacCrossSection'))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

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
        options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Fishing Areas", "Whole Bay Suitability"))
  })
  
  #this makes our hot spot pie chart  
  output$HotSpotPie <- renderPlotly({
    plot_ly(fishinghotspotssummary, labels = ~habitat, values = ~percent, type = 'pie', 
            textposition = 'outside', 
            textinfo = 'label+percent',
            marker = list(colors = fishinghotspotssummary$color)) %>%
      layout(title = 'Fishing Hotspots Habitat Suitability',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  #this makes our whole bay pie chart
  output$WholeBayPie <- renderPlotly({
    plot_ly(wholebaysurfacesummary, labels = ~habitat, values = ~percent, type = 'pie',
            textposition = 'outside',
            textinfo = 'label+percent',
            marker = list(colors = wholebaysurfacesummary$color)) %>%
      layout(title = 'Whole Bay Habitat Suitability',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
 #striped bass images
  output$SuitableCriteria <- renderImage({
    filename <- normalizePath(file.path(here('Striped-Bass-Habitat-Suitability', 'Bass Suitable Criteria.png')))
    list(src = filename, alt = "Striped Bass Suitability Criteria", width=500)
  }, deleteFile = FALSE)
  
  output$StripedBassSqueeze <- renderImage({
    filename <- normalizePath(file.path(here('Striped-Bass-Habitat-Suitability', 'Striped Bass Squeeze.png')))
    list(src = filename, alt = "Striped Bass Squeeze", width=500)
  }, deleteFile = FALSE)
  
  
  output$HotSpot10yrs <- renderPlotly({
    plot_ly(historicbaydata_fishingareas_summary, x = ~year, y = ~percent, color = ~Habitat, colors = suitability_colors,
            type = 'bar') %>%
      layout(title = 'Fishing Hot Spot Suitability for the Previous Ten Years',
             yaxis = list(title = 'Percent of Habitat'),
             barmode = 'stack')
  })
  
  output$WholeBay10yrs <- renderPlotly({
    plot_ly(historicbaydatasummary, x = ~year, y = ~percent, color = ~Habitat, colors = suitability_colors,
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
