library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)

# Sample polygon and point sf layers
# Create example polygon
polygon <- st_as_sf(st_sfc(st_polygon(list(rbind(
  c(-100, 40), c(-90, 45), c(-85, 40), c(-100, 40)
))), crs = 4326))

# Create points layers with colors
set.seed(123)
points_a <- st_as_sf(data.frame(
  color = sample(c("black", "orange", "yellow", "blue"), 100, replace = TRUE),
  lon = runif(100, -100, -90),
  lat = runif(100, 40, 45)
), coords = c("lon", "lat"), crs = 4326)

points_b <- st_as_sf(data.frame(
  color = sample(c("black", "orange", "yellow", "blue"), 100, replace = TRUE),
  lon = runif(100, -95, -85),
  lat = runif(100, 40, 45)
), coords = c("lon", "lat"), crs = 4326)

# App
ui <- fluidPage(
  fluidRow(
    column(8, leafletOutput("map")),
    column(4,
           conditionalPanel("input.layer == 'Points A'", plotlyOutput("pieA")),
           conditionalPanel("input.layer == 'Points B'", plotlyOutput("pieB"))
    )
  ),
  hidden(
    selectInput("layer", "Active Layer", choices = c("Points A", "Points B"), selected = "Points A")
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    selected_color = NULL,
    active_layer = "Points A"
  )
  
  # Initial map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = polygon, group = "Polygons", fillColor = "gray", weight = 1, fillOpacity = 0.4) %>%
      addCircleMarkers(data = points_a, color = ~color, group = "Points A", radius = 5, label = ~color) %>%
      addCircleMarkers(data = points_b, color = ~color, group = "Points B", radius = 5, label = ~color) %>%
      addLayersControl(
        overlayGroups = c("Polygons", "Points A", "Points B"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # Dynamic filtering of points based on selected color
  observe({
    leafletProxy("map") %>%
      clearGroup("Points A") %>%
      clearGroup("Points B") %>%
      {
        if (rv$active_layer == "Points A") {
          pts <- points_a
          if (!is.null(rv$selected_color)) {
            pts <- pts %>% filter(color == rv$selected_color)
          }
          addCircleMarkers(., data = pts, color = ~color, group = "Points A", radius = 5, label = ~color)
        } else {
          pts <- points_b
          if (!is.null(rv$selected_color)) {
            pts <- pts %>% filter(color == rv$selected_color)
          }
          addCircleMarkers(., data = pts, color = ~color, group = "Points B", radius = 5, label = ~color)
        }
      }
  })
  
  # Pie charts
  output$pieA <- renderPlotly({
    points_a %>%
      st_drop_geometry() %>%
      count(color) %>%
      plot_ly(labels = ~color, values = ~n, type = 'pie', source = "pieA")
  })
  
  output$pieB <- renderPlotly({
    points_b %>%
      st_drop_geometry() %>%
      count(color) %>%
      plot_ly(labels = ~color, values = ~n, type = 'pie', source = "pieB")
  })
  
  # Detect visible map layer
  observe({
    input$map_groups
    isolate({
      visible <- input$map_groups
      if ("Points A" %in% visible) {
        rv$active_layer <- "Points A"
        updateSelectInput(session, "layer", selected = "Points A")
      } else if ("Points B" %in% visible) {
        rv$active_layer <- "Points B"
        updateSelectInput(session, "layer", selected = "Points B")
      }
    })
  })
  
  # Pie slice click detection
  observeEvent(event_data("plotly_click", source = "pieA"), {
    click_data <- event_data("plotly_click", source = "pieA")
    rv$selected_color <- click_data[["label"]]
  })
  
  observeEvent(event_data("plotly_click", source = "pieB"), {
    click_data <- event_data("plotly_click", source = "pieB")
    rv$selected_color <- click_data[["label"]]
  })
}

shinyApp(ui, server)
