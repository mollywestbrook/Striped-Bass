library(sf)
library(terra)
library(leaflet)
library(gstat)

# Example data
set.seed(123)
pts <- data.frame(
  lon = runif(20, -75, -74),
  lat = runif(20, 39, 40),
  temp = runif(20, 10, 30)
)

# Convert to sf
pts_sf <- st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)

# Polygon
poly <- st_as_sf(st_sfc(st_polygon(list(rbind(
  c(-75, 39), c(-75, 40), c(-74, 40), c(-74, 39), c(-75, 39)
)))), crs = 4326)

# ---- Interpolation grid ----
# Create prediction grid
r <- rast(ext(poly), resolution = 0.01, crs = "EPSG:4326")
grid <- as.data.frame(as.points(r), geom = "XY")
names(grid) <- c("x", "y")

# gstat requires Spatial objects, so convert
pts_sp <- as(pts_sf, "Spatial")
grid_sp <- SpatialPoints(grid, proj4string = CRS("+proj=longlat +datum=WGS84"))

# Run IDW
idw_res <- gstat::idw(temp ~ 1, pts_sp, newdata = grid_sp, idp = 2)

# Back to raster
grid$temp <- idw_res$var1.pred
r_idw <- rast(grid, type = "xyz", crs = "EPSG:4326")
r_masked <- mask(r_idw, vect(poly))

# ---- Leaflet ----
pal <- colorNumeric("viridis", values(r_masked), na.color = "transparent")

leaflet() %>%
  addTiles() %>%
  addRasterImage(r_masked, colors = pal, opacity = 0.7) %>%
  addPolygons(data = poly, color = "black", weight = 2, fill = FALSE) %>%
  addCircleMarkers(data = pts_sf, radius = 4,
                   color = ~pal(temp), label = ~as.character(round(temp,1))) %>%
  addLegend(pal = pal, values = values(r_masked), title = "Temperature")