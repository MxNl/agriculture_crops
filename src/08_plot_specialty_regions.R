# Regions specialised in one crop (grapevine, hops, orchards, vegetables, ...)
# from the Thuenen crop type map (thuenen_ctm in definitions.yml), found with
# simple statistics instead of image recognition:
#
# 1. the map is resampled to specialty_search_res_m (GDAL uses the overviews of
#    the cloud-optimised GeoTIFF) and its classes are counted per
#    specialty_cell_km cell
# 2. core cells of a crop: at least specialty_cell_share of the farmland and
#    specialty_min_lq times its share in Germany (location quotient), so the
#    crop is both dominant and rare nationally. Neighbouring core cells
#    (gaps of one cell bridged) form a region; regions need
#    specialty_min_region_ha of the crop
# 3. greedy selection by crop area x share: at most specialty_max_per_crop
#    regions per crop, centres at least specialty_min_distance_km apart. The
#    window covers the region plus specialty_context of its size on each
#    side, for the contrast with the surroundings
# 4. the windows are read (GDAL picks the overview that fits the plot
#    resolution) and shown as panels, next to a map of Germany with the
#    numbered windows
#
# - plots/specialty_regions_<year>.png
#
# The map is read via HTTP; set the environment variable CTM_PATH to use a
# local copy instead. Run from the project root.

library(patchwork)

# borders, settings, theme and plot text
source("src/functions.R")

# settings ------------------------------------------------------------------

ctm_source <- Sys.getenv("CTM_PATH", definitions$thuenen_ctm)
if (str_starts(ctm_source, "https?://")) ctm_source <- str_c("/vsicurl/", ctm_source)
ctm_year <- definitions$thuenen_year

search_res <- definitions$specialty_search_res_m
cell_px <- round(definitions$specialty_cell_km * 1000 / search_res)

dir.create("cache/thuenen", showWarnings = FALSE, recursive = TRUE)
# the search runs in the reference CRS (metres), whatever the CRS of the map
crs_search <- str_c("EPSG:", definitions$crs_reference)
path_search <- str_glue(
  "cache/thuenen/ctm_{ctm_year}_{search_res}m_epsg{definitions$crs_reference}.tif"
)

# the map's GeoTIFF keys define ETRS89 slightly differently from the EPSG
# registry; use the registry (as GDAL suggests), which also silences its
# warning
gdal_config <- c(GTIFF_SRS_SOURCE = "EPSG")
Sys.setenv(GTIFF_SRS_SOURCE = "EPSG")

caption_ctm <- str_c(
  "Data: Th\u00FCnen crop type map ", ctm_year, " (v302; Blickensd\u00F6rfer ",
  "et al. 2022; Tetteh et al.), CC BY 4.0. Borders and places: Natural Earth."
)

# legend: code, RGBA, name ---------------------------------------------------

ctm_legend <- read_lines(definitions$thuenen_legend) |>
  str_trim() |>
  str_match("^(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(.+)$") |>
  as_tibble(.name_repair = ~ c("line", "code", "r", "g", "b", "a", "class")) |>
  transmute(
    code = as.integer(code),
    class,
    colour = rgb(as.integer(r), as.integer(g), as.integer(b), maxColorValue = 255)
  )

# colours of the matching DLR classes (functions.R), so a crop looks the same
# in all plots; classes without a DLR counterpart keep the Thuenen colour.
# Classes that share a DLR class get a shade of its colour
ctm_to_dlr <- tribble(
  ~code, ~value, ~shade,
  200L,  83L,  0,   # permanent grassland
  1101L, 11L,  0,   # winter wheat
  1102L, 12L,  0,   # winter barley
  1103L, 13L,  0,   # winter rye
  1201L, 22L,  0,   # spring barley
  1202L, 23L,  0,   # spring oat
  1300L, 30L,  0,   # maize
  1401L, 50L,  0,   # potato
  1402L, 60L,  0,   # sugar beet
  1501L, 71L,  0,   # winter rapeseed
  1502L, 72L,  0,   # sunflower
  1602L, 84L,  0,   # cultivated grassland (DLR forage)
  1603L, 120L, 0,   # vegetables
  1611L, 40L,  0,   # peas (DLR legumes)
  1612L, 40L,  25,  # broad bean (DLR legumes, darker)
  1613L, 42L,  0,   # lupin
  1614L, 41L,  0,   # soy
  3002L, 111L, 0,   # other agricultural areas
  4001L, 90L,  0,   # grapevine
  4002L, 110L, 0,   # hops
  4003L, 100L, 0    # orchard
) |>
  left_join(colours, by = "value") |>
  mutate(colour = if_else(shade > 0, scales::col_darker(colour, shade), colour))

ctm_legend <- ctm_legend |>
  left_join(ctm_to_dlr |> select(code, dlr_colour = colour), by = "code") |>
  mutate(colour = coalesce(dlr_colour, colour)) |>
  select(-dlr_colour)

# fill scale in the legend colours; breaks: codes shown in the legend
scale_fill_ctm <- function(breaks = waiver()) {
  scale_fill_manual(
    values = set_names(ctm_legend$colour, ctm_legend$code),
    labels = \(codes) ctm_legend$class[match(codes, ctm_legend$code)],
    breaks = breaks,
    # the same limits in all panels, so patchwork merges their legends
    limits = as.character(ctm_legend$code),
    drop = FALSE,
    name = NULL
  )
}

farmland_codes <- setdiff(ctm_legend$code, definitions$specialty_non_farmland)
candidate_codes <- setdiff(farmland_codes, definitions$specialty_excluded)

# 1. class counts per cell ----------------------------------------------------

if (!file.exists(path_search)) {
  message(str_glue("{format(Sys.time(), '%H:%M')} resampling to {search_res} m"))
  # gdalwarp reads from the overview closest to the target resolution
  sf::gdal_utils(
    "warp", ctm_source, path_search,
    options = c(
      "-t_srs", crs_search, "-tr", search_res, search_res, "-r", "near",
      "-co", "COMPRESS=DEFLATE", "-co", "TILED=YES"
    ),
    config_options = gdal_config
  )
}

ctm_search <- rast(path_search)
n_rows <- ceiling(nrow(ctm_search) / cell_px)
n_cols <- ceiling(ncol(ctm_search) / cell_px)
codes <- ctm_legend$code

# counts[i, j, k]: pixels of class codes[k] in cell row i, column j; read in
# bands of one cell row
cell_col <- (seq_len(ncol(ctm_search)) - 1) %/% cell_px + 1
readStart(ctm_search)
counts <- map(seq_len(n_rows), \(i) {
  first <- (i - 1) * cell_px + 1
  n <- min(cell_px, nrow(ctm_search) - first + 1)
  k <- match(readValues(ctm_search, row = first, nrows = n), codes)
  ok <- !is.na(k)
  matrix(
    tabulate(rep(cell_col, times = n)[ok] + n_cols * (k[ok] - 1), n_cols * length(codes)),
    nrow = n_cols
  )
})
readStop(ctm_search)
counts <- simplify2array(counts) |> aperm(c(3, 1, 2))
dimnames(counts) <- list(NULL, NULL, codes)

farmland <- apply(counts[, , as.character(farmland_codes), drop = FALSE], c(1, 2), sum)
national_share <- apply(counts, 3, sum) / sum(farmland)

# 2. regions: connected cells where a rare crop dominates ---------------------

pixel_ha <- search_res^2 / 1e4
cell_m <- cell_px * search_res
e0 <- ext(ctm_search)
cell_grid <- rast(
  nrows = n_rows, ncols = n_cols, crs = crs_search,
  xmin = xmin(e0), xmax = xmin(e0) + n_cols * cell_m,
  ymin = ymax(e0) - n_rows * cell_m, ymax = ymax(e0)
)
# centre coordinates of cell rows and columns
cell_x <- xmin(e0) + (seq_len(n_cols) - 0.5) * cell_m
cell_y <- ymax(e0) - (seq_len(n_rows) - 0.5) * cell_m

# per crop: core cells (enough farmland, share and location quotient), joined
# into regions; gaps of one cell are bridged (patches of the core cells
# dilated by one cell), the statistics use the core cells only
find_regions <- function(code) {
  crop <- counts[, , as.character(code)]
  share <- crop / pmax(farmland, 1)
  lq <- share / national_share[as.character(code)]
  core <- farmland >= definitions$specialty_min_farmland * cell_px^2 &
    share >= definitions$specialty_cell_share &
    lq >= definitions$specialty_min_lq
  if (!any(core)) return(NULL)

  r <- setValues(cell_grid, as.vector(t(ifelse(core, 1, NA))))
  r <- focal(r, w = 3, fun = "max", na.rm = TRUE)
  id <- matrix(values(patches(r, directions = 8)), nrow = n_rows, byrow = TRUE)
  id[!core] <- NA

  idx <- which(!is.na(id), arr.ind = TRUE)
  tibble(
    region = id[idx], row = idx[, 1], col = idx[, 2],
    crop = crop[idx], farm = farmland[idx]
  ) |>
    group_by(region) |>
    summarise(
      code = code,
      crop_ha = sum(crop) * pixel_ha,
      share = sum(crop) / sum(farm),
      # crop-weighted centre and extent of the region
      x = weighted.mean(cell_x[col], crop),
      y = weighted.mean(cell_y[row], crop),
      size_m = max(diff(range(col)), diff(range(row))) * cell_m + cell_m,
      .groups = "drop"
    ) |>
    mutate(lq = share / national_share[as.character(code)])
}

regions <- candidate_codes |>
  map(find_regions) |>
  list_rbind() |>
  filter(crop_ha >= definitions$specialty_min_region_ha) |>
  # large regions dominated by the crop first
  mutate(score = crop_ha * share)

# 3. greedy selection --------------------------------------------------------

select_regions <- function(regions) {
  selected <- regions[0, ]
  pool <- regions |> arrange(desc(score))
  min_distance <- definitions$specialty_min_distance_km * 1000

  for (i in seq_len(nrow(pool))) {
    if (nrow(selected) >= definitions$specialty_panels) break
    candidate <- pool[i, ]
    too_many <- sum(selected$code == candidate$code) >= definitions$specialty_max_per_crop
    too_close <- any(sqrt((selected$x - candidate$x)^2 + (selected$y - candidate$y)^2) < min_distance)
    if (!too_many && !too_close) selected <- bind_rows(selected, candidate)
  }
  selected
}

# window: the region plus specialty_context of its size on each side, so the
# region stands out against its surroundings; whole km, within the limits
windows <- select_regions(regions) |>
  mutate(
    number = row_number(),
    window_km = pmin(
      pmax(
        ceiling(size_m * (1 + 2 * definitions$specialty_context) / 1000),
        definitions$specialty_window_min_km
      ),
      definitions$specialty_window_max_km
    ),
    extent = pmap(list(x, y, window_km), \(x, y, w) {
      ext(x - w * 500, x + w * 500, y - w * 500, y + w * 500)
    })
  ) |>
  left_join(ctm_legend, by = "code")

# nearest place for the panel titles (Natural Earth populated places)
places <- read_naturalearth("populated_places") |>
  filter(ADM0_A3 == "DEU") |>
  st_transform(crs(ctm_search))

window_polygons <- windows$extent |>
  map(\(e) st_as_sfc(st_bbox(as.vector(e)[c(1, 3, 2, 4)] |> set_names(c("xmin", "ymin", "xmax", "ymax"))))) |>
  do.call(what = c) |>
  st_set_crs(crs(ctm_search))
window_centres <- st_centroid(window_polygons)

compass <- function(dx, dy) {
  c("E", "NE", "N", "NW", "W", "SW", "S", "SE")[round(atan2(dy, dx) / (pi / 4)) %% 8 + 1]
}
nearest <- st_nearest_feature(window_centres, places)
offset <- st_coordinates(window_centres) - st_coordinates(places[nearest, ])
distance_km <- sqrt(rowSums(offset^2)) / 1000
windows$place <- if_else(
  distance_km < 15,
  str_c("near ", places$NAME[nearest]),
  str_c(round(distance_km), " km ", compass(offset[, 1], offset[, 2]), " of ", places$NAME[nearest])
)

# 4. panels and overview map -------------------------------------------------

max_plot_px <- 800 # windows are read at most this many pixels wide

# window (extent in the search CRS) warped from the map with gdalwarp, which
# reads from the overview that fits the plot resolution, so large windows
# stay light
read_window <- function(extent) {
  size <- xmax(extent) - xmin(extent)
  res_out <- max(10, size / max_plot_px)
  path_window <- tempfile(fileext = ".tif")
  on.exit(unlink(path_window))
  sf::gdal_utils(
    "warp", ctm_source, path_window,
    options = c(
      "-t_srs", crs_search,
      "-te", xmin(extent), ymin(extent), xmax(extent), ymax(extent),
      "-tr", res_out, res_out, "-r", "near"
    ),
    config_options = gdal_config
  )
  as.data.frame(rast(path_window), xy = TRUE, na.rm = TRUE) |>
    as_tibble() |>
    set_names(c("x", "y", "code")) |>
    mutate(code = factor(code, levels = ctm_legend$code))
}

windows$cells <- map(windows$extent, read_window)

# classes shown in the legend: those that cover at least 1% of a panel
shown_codes <- windows$cells |>
  map(\(cells) {
    cells |>
      count(code) |>
      filter(n / sum(n) >= 0.01) |>
      pull(code) |>
      as.character()
  }) |>
  unlist() |>
  unique()
legend_breaks <- intersect(as.character(ctm_legend$code), shown_codes)

plot_window <- function(number, class, place, window_km, share, lq, crop_ha, cells, ...) {
  ggplot(cells, aes(x, y, fill = code)) +
    geom_raster() +
    # the legend is drawn once, by the overview map
    scale_fill_ctm(breaks = legend_breaks) +
    guides(fill = "none") +
    coord_equal(expand = FALSE) +
    labs(
      title = str_c(number, "  ", class),
      subtitle = str_glue(
        "{place} \u00B7 {window_km} \u00D7 {window_km} km\n",
        "Region: {scales::label_comma(accuracy = 100)(crop_ha)} ha, ",
        "{scales::label_percent()(share)} of the farmland,\n",
        "{scales::label_number(accuracy = 1, big.mark = ',')(lq)}\u00D7 the German share"
      )
    ) +
    theme_maps() +
    theme(
      plot.title = element_text(size = rel(0.95), hjust = 0, margin = margin(b = 2)),
      plot.subtitle = element_text(size = rel(0.75), hjust = 0, lineheight = 1.1, margin = margin(b = 6)),
      panel.background = element_rect(fill = "grey95", colour = NA),
      plot.margin = margin(4, 8, 4, 8)
    )
}

panels <- windows |> pmap(plot_window)

states_ctm <- st_transform(states, crs(ctm_search))
legend_anchor <- st_coordinates(st_centroid(st_union(states_ctm)))

overview <- ggplot() +
  geom_sf(data = states_ctm, fill = "grey95", colour = "grey70", linewidth = 0.2) +
  # invisible points (size 0) that carry the legend of all panels
  geom_point(
    aes(x, y, fill = code),
    data = tibble(
      x = legend_anchor[1], y = legend_anchor[2],
      code = factor(legend_breaks, levels = ctm_legend$code)
    ),
    shape = 22, size = 0, stroke = 0
  ) +
  scale_fill_ctm(breaks = legend_breaks) +
  guides(fill = guide_legend(ncol = 2, override.aes = list(size = 5))) +
  geom_sf(data = window_polygons, fill = NA, colour = "grey15", linewidth = 0.5) +
  # numbers above the frames, with some space
  geom_label(
    aes(x, y, label = number),
    data = tibble(
      x = map_dbl(windows$extent, \(e) (xmin(e) + xmax(e)) / 2),
      y = map_dbl(windows$extent, ymax) + 9000,
      number = windows$number
    ),
    vjust = 0,
    size = 5,
    fontface = "bold",
    label.padding = unit(0.2, "lines"),
    border.colour = NA,
    fill = alpha("white", 0.8)
  ) +
  coord_sf(datum = NA) +
  theme_maps() +
  theme(plot.margin = margin(4, 12, 4, 4))

# first column: overview map on top, the legend below; panels in 3 columns
n_panel_cols <- 3
n_panel_rows <- max(2, ceiling(length(panels) / n_panel_cols))
# patchwork::area(), terra has an area() too
design <- c(
  patchwork::area(1, 1, n_panel_rows - 1, 1),
  patchwork::area(n_panel_rows, 1, n_panel_rows, 1),
  map(seq_along(panels), \(i) {
    patchwork::area((i - 1) %/% n_panel_cols + 1, (i - 1) %% n_panel_cols + 2)
  }) |> do.call(what = c)
)

plot_specialty <- wrap_plots(c(list(overview, guide_area()), panels), design = design) +
  plot_layout(widths = c(1.4, rep(1, n_panel_cols)), guides = "collect") +
  plot_annotation(
    title = "Regions specialised in one crop",
    subtitle = str_wrap(str_glue(
      "Regions where a crop that is rare in Germany dominates the farmland: ",
      "neighbouring {definitions$specialty_cell_km} km cells with ",
      "\u2265 {scales::label_percent()(definitions$specialty_cell_share)} of the ",
      "farmland and \u2265 {definitions$specialty_min_lq}\u00D7 the German share, ",
      "the largest per crop (at most {definitions$specialty_max_per_crop}). ",
      "Windows show the region and its surroundings, sizes differ. Numbers ",
      "as on the map; colours of the crops as in the DLR crop maps."
    ), 130),
    caption = caption_ctm,
    theme = theme_maps()
  ) &
  theme(
    legend.direction = "vertical",
    legend.text = element_text(size = rel(0.85))
  )

save_plot(
  str_glue("plots/specialty_regions_{ctm_year}.png"),
  plot_specialty,
  width = 16, height = 4.2 * n_panel_rows + 2, dpi = 300, bg = "white"
)

print(windows |> select(number, class, place, window_km, share, lq, crop_ha), n = Inf)
