# Playground: interactive, zoomable DLR CropTypes map in the EUCROPMAP/CDL
# colours (Shiny + leaflet).
#
# On every pan/zoom only the visible window is read from a stars proxy, at
# about screen resolution, and drawn with leafem::addStarsImage(). GDAL
# overviews (built once into cache/, ~ a few minutes) make zoomed-out views
# fast; zoomed in, the full 10 m data are shown. Click a pixel for its class.
#
# Run interactively from the project root:
#   source("playground/interactive_crop_map.R")

library(shiny)
library(leaflet)
library(leafem)
library(stars)

# legend, colours, paths
source("src/functions.R")

year <- 2024
max_pixels <- 1200 # longest side of the image sent to the browser

# overviews ------------------------------------------------------------------

# VRT wrapper with 0 = no data, so overviews (mode) ignore non-farmland and
# non-farmland stays transparent; the data file itself is not touched
path_vrt <- str_glue("cache/croptypes_dlr_{year}_nodata0.vrt")

if (!file.exists(path_vrt)) {
  sf::gdal_utils(
    "translate",
    source = path_croptypes(year),
    destination = path_vrt,
    options = c("-of", "VRT", "-a_nodata", "0")
  )
}

if (!file.exists(str_c(path_vrt, ".ovr"))) {
  message("Building overviews once, this takes a few minutes ...")
  sf::gdal_addo(
    path_vrt,
    overviews = 2^(1:8),
    method = "MODE",
    read_only = TRUE, # external .ovr file next to the VRT
    config_options = c(COMPRESS_OVERVIEW = "DEFLATE")
  )
}

croptypes <- read_stars(path_vrt, proxy = TRUE)
croptypes_terra <- rast(path_croptypes(year)) # fast point lookups on click

# colours --------------------------------------------------------------------

map_legend <- legend |> filter(!is.na(colour))

palette_crops <- colorFactor(
  palette = map_legend$colour,
  levels = map_legend$value,
  na.color = "transparent"
)

# read the visible window at about screen resolution -------------------------

read_window <- function(bounds) {
  window <- st_bbox(
    c(
      xmin = bounds$west, ymin = bounds$south,
      xmax = bounds$east, ymax = bounds$north
    ),
    crs = st_crs(4326)
  ) |>
    st_as_sfc() |>
    st_transform(st_crs(croptypes)) |>
    st_bbox()

  cropped <- croptypes[window]
  n_cells <- max(dim(cropped)[1:2])
  # stars skips `downsample` cells per row/column; GDAL takes the matching
  # overview, so this reads only what the screen can show
  downsample <- max(0, ceiling(n_cells / max_pixels) - 1)

  cropped <- st_as_stars(cropped, downsample = downsample)
  # class rasters may be read as factors with levels "0", "11", ...
  cropped[[1]] <- as.integer(as.character(cropped[[1]]))
  cropped
}

# app ------------------------------------------------------------------------

ui <- fillPage(
  leafletOutput("map", height = "100%")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = 10.4, lat = 51.2, zoom = 6) |>
      addLegend(
        position = "bottomright",
        colors = map_legend$colour,
        labels = map_legend$class,
        title = str_glue("DLR CropTypes {year}"),
        opacity = 1
      )
  })

  # redraw after panning/zooming has settled
  bounds <- debounce(reactive(input$map_bounds), 400)

  observeEvent(bounds(), {
    window <- read_window(bounds())

    leafletProxy("map") |>
      clearGroup("crops") |>
      addStarsImage(
        window,
        colors = palette_crops,
        project = TRUE,
        method = "near",
        group = "crops",
        opacity = 0.85,
        maxBytes = 16 * 1024^2
      )
  })

  # class of the clicked 10 m pixel
  observeEvent(input$map_click, {
    click <- input$map_click
    point <- st_sfc(st_point(c(click$lng, click$lat)), crs = 4326) |>
      st_transform(crs(croptypes_terra)) |>
      vect()
    value <- terra::extract(croptypes_terra, point)[1, 2]
    class <- coalesce(legend$class[match(value, legend$value)], "No farmland")

    leafletProxy("map") |>
      clearPopups() |>
      addPopups(click$lng, click$lat, str_glue("<b>{class}</b> (code {value})"))
  })
}

runApp(shinyApp(ui, server))
