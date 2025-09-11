# install.packages(c("leaflet","terra","sf","leafem")) # run once

library(terra)    # raster
library(sf)       # vectors
library(leaflet)  # web map
library(leafem) # helpers for raster in leaflet
library(raster)
library(xml2)


# ---- 1) Read data ----
# Adjust the paths below if your files are in a different folder
bathy    <- terra::rast("bathymetry_0.tif")        # the .tif itself
geomorph <- sf::st_read("geomorphic.geojson", quiet = TRUE)
reef     <- sf::st_read("reefextent.shp",  quiet = TRUE)

bnd <- sf::st_read("boundary.geojson", quiet = TRUE)
# put everything in WGS84 for Leaflet
if (is.na(sf::st_crs(bnd))) sf::st_crs(bnd) <- 4326
bnd <- sf::st_transform(bnd, 4326)
bb  <- sf::st_bbox(bnd)   # xmin, ymin, xmax, ymax

ext(bathy); res(bathy)
is.lonlat(bathy)


# Set the raster’s geographic footprint (xmin, xmax, ymin, ymax)
terra::ext(bathy) <- terra::ext(bb["xmin"], bb["xmax"], bb["ymin"], bb["ymax"])
terra::crs(bathy) <- sf::st_crs(bnd)$wkt

terra::ext(bathy); terra::is.lonlat(bathy)

if (sf::st_crs(geomorph)$epsg != 4326) geomorph <- sf::st_transform(geomorph, 4326)
if (sf::st_crs(reef)$epsg     != 4326) reef     <- sf::st_transform(reef, 4326)
bathy <- terra::flip(bathy, "vertical")
plot(bathy); plot(sf::st_geometry(bnd), add = TRUE, border = "orange", lwd = 2)
plot(sf::st_geometry(reef), add = TRUE, border = 'red', lwd = 2)


# Convert to RasterLayer
rbathy <- raster::raster(bathy)

# palette
rng <- as.numeric(terra::global(bathy, "range", na.rm = TRUE))
pal_bathy <- colorNumeric("Blues", rng, na.color = "transparent")


 
#Rasterize Geomorph
# 1) Make sure CRS match (Leaflet likes 4326)
geomorph <- st_make_valid(geomorph)
geomorph <- st_transform(geomorph, crs(bathy))

# 2) Create integer codes for classes (raster cells store ints)
field <- "class"
classes <- sort(unique(geomorph[[field]]))
lut <- data.frame(code = seq_along(classes), class = classes)

geomorph$code <- lut$code[ match(geomorph[[field]], lut$class) ]

# 3) Rasterise onto bathy grid (same pixel size/extent/CRS)
r_geom <- terra::rasterize(
  x = terra::vect(geomorph),   # convert sf -> SpatVector
  y = bathy,               # TEMPLATE GRID (drives res/extent/CRS)
  field = "code",
  background = NA,
  touches = TRUE               # TRUE = any overlap; FALSE = cell-center
)

# Optional: mask rasterised output to the polygon footprint
r_geom <- terra::mask(r_geom, terra::vect(geomorph))

# --- Quick visual check (base R) ---
plot(bathy); plot(r_geom, add=TRUE, alpha=0.5); plot(st_geometry(geomorph), add=TRUE, border="orange")

# categorical palette for codes
pal_geom_r <- colorFactor("Set2", domain = lut$code)

# convert to RasterLayer for addRasterImage()
r_geom_rl <- raster::raster(r_geom)



m <- leaflet() |>
  addProviderTiles(providers$Esri.OceanBasemap, group = "Esri Ocean") |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri Satellite") |>
  addProviderTiles(providers$CartoDB.Positron,  group = "Light") |>
  
  addRasterImage(rbathy, colors = pal_bathy, opacity = 0.7, project = TRUE,
                 group = "Bathymetry") |>
  addRasterImage(r_geom_rl, colors = pal_geom_r, opacity = 0.6, project = FALSE,
                 group = "Geomorphic (rasterised)") |>
  # Legend with labels mapped to codes
  addLegend(
    position = "bottomleft",
    colors   = pal_geom_r(lut$code),
    labels   = lut$class,
    title    = "Geomorphic class",
    opacity  = 0.8
  ) |>
  addLegend(pal = pal_bathy, values = rng, title = "Depth (m)",
            position = "bottomright") |>
  # addLegend(pal = pal_geom, values = geomorph$class, title = "Depth (m)",
  #           position = "bottomleft") |>
  
  # geomorphic polygons (properly mapped aesthetics)
  addPolygons(
    data = geomorph,
    weight = 1,
    color = ~pal_geom(class),
    fillColor = ~pal_geom(class),
    fillOpacity = 0.6,
    group = "Geomorphic zones"
  ) |>
  
  # reef outline
  addPolygons(
    data = reef,
    weight = 2, color = "#ff6600", fill = FALSE,
    group = "Reef extent"
  ) |>

  addLayersControl(
    baseGroups    = c("Esri Ocean", "Esri Satellite", "Light"),
    overlayGroups = c("Bathymetry", "Geomorphic zones","Geomorphic (rasterised)", "Reef extent"),
    options = layersControlOptions(collapsed = FALSE)
  ) 

m
