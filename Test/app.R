library(shiny)
library(leaflet)
library(sf)
library(plotly)
library(dplyr)

# ---- Sample data ----
# Simulated polygon
polygon <- st_as_sf(st_sfc(
  st_polygon(list(rbind(c(-100, 40), c(-100, 41), c(-99, 41), c(-99, 40), c(-100, 40)))),
  crs = 4326
))

# Simulated point data
generate_points <- function(n) {
  suitability_levels <- c("suitable", "tolerable", "marginal", "unsuitable")
  colors <- c("dodgerblue", "yellow", "orange", "black")
  
  data.frame(
    lon = runif(n, -99.9, -99.1),
    lat = runif(n, 40.1, 40.9),
    suitability = sample(suitability_levels, n, replace = TRUE)
  ) %>%
    mutate(
      color = case_when(
        suitability == "suitable" ~ "dodgerblue",
        suitability == "tolerable" ~ "yellow",
        suitability == "marginal" ~ "orange",
        suitability == "unsuitable" ~ "black"
      )
    ) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
}

points_a <- generate_points(100)
points_b <- generate_points(100)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Leaflet + Plotly Click Filter"),
  sidebarLayout(
    sidebarPanel(
      selectInput("layer", "Active Point Layer", choices = c("Points A", "Points B")),
      plotlyOutput("pieA"),
      plotlyOutput("pieB")
    ),
    mainPanel(
      leafletOutput("map", height = 600),
      verbatimTextOutput("debug")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  rv <- reactiveValues(active_layer = "Points A", selected_color = NULL)
  
  # ---- Map ----
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = polygon, fillColor = "lightgreen", color = "green", weight = 2, group = "Polygon") %>%
      addCircleMarkers(data = points_a, color = ~color, group = "Points A", radius = 5, label = ~suitability) %>%
      addCircleMarkers(data = points_b, color = ~color, group = "Points B", radius = 5, label = ~suitability) %>%
      addLayersControl(
        overlayGroups = c("Polygon", "Points A", "Points B"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      showGroup("Points A") %>%
      hideGroup("Points B")
  })
  
  # ---- Pie A ----
  output$pieA <- renderPlotly({
    pie_data <- points_a %>%
      st_drop_geometry() %>%
      count(suitability, color)
    
    p <- plot_ly(
      data = pie_data,
      labels = ~suitability,
      values = ~n,
      type = "pie",
      source = "pieA",
      customdata = ~color
    )
    event_register(p, "plotly_click")
    p
  })
  
  # ---- Pie B ----
  output$pieB <- renderPlotly({
    pie_data <- points_b %>%
      st_drop_geometry() %>%
      count(suitability, color)
    
    p <- plot_ly(
      data = pie_data,
      labels = ~suitability,
      values = ~n,
      type = "pie",
      source = "pieB",
      customdata = ~color
    )
    event_register(p, "plotly_click")
    p
  })
  
  # ---- Observe pie clicks ----
  observeEvent(event_data("plotly_click", source = "pieA"), {
    click_data <- event_data("plotly_click", source = "pieA")
    rv$selected_color <- click_data[["customdata"]]
  })
  
  observeEvent(event_data("plotly_click", source = "pieB"), {
    click_data <- event_data("plotly_click", source = "pieB")
    rv$selected_color <- click_data[["customdata"]]
  })
  
  # ---- Update map layer selection ----
  observeEvent(input$layer, {
    rv$active_layer <- input$layer
    rv$selected_color <- NULL  # Reset color filter on layer change
    
    leafletProxy("map") %>%
      hideGroup(c("Points A", "Points B")) %>%
      showGroup(input$layer)
  })
  
  # ---- Filter points by selected color ----
  observeEvent({
    rv$selected_color
    rv$active_layer
  }, {
    leafletProxy("map") %>%
      clearGroup("Points A") %>%
      clearGroup("Points B")
    
    if (rv$active_layer == "Points A") {
      pts <- points_a
      if (!is.null(rv$selected_color)) {
        pts <- pts %>% filter(color == rv$selected_color)
      }
      leafletProxy("map") %>%
        addCircleMarkers(data = pts, color = ~color, group = "Points A", radius = 5, label = ~suitability)
    } else if (rv$active_layer == "Points B") {
      pts <- points_b
      if (!is.null(rv$selected_color)) {
        pts <- pts %>% filter(color == rv$selected_color)
      }
      leafletProxy("map") %>%
        addCircleMarkers(data = pts, color = ~color, group = "Points B", radius = 5, label = ~suitability)
    }
  })
  
  # Debug output
  output$debug <- renderPrint({
    list(
      selected_layer = rv$active_layer,
      selected_color = rv$selected_color
    )
  })
  }
  
  shinyApp(ui, server)
  