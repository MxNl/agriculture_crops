# Regions specialised in one crop (grapevine, hops, orchards, vegetables, ...)
# from the Thuenen crop type map (thuenen_ctm in definitions.yml), found with
# simple statistics instead of image recognition:
#
# 1. the map is resampled to specialty_search_res_m (GDAL uses the overviews of
#    the cloud-optimised GeoTIFF) and its classes are counted per
#    specialty_cell_km cell
# 2. for every crop and window size (specialty_window_km) the share of the
#    crop in the farmland of the window and its location quotient (LQ =
#    share in the window / share in Germany) are computed with summed-area
#    tables; score = share * log(LQ), so a window must be dominated by a crop
#    that is rare nationally
# 3. greedy selection: the best window, enlarged as long as the crop share
#    stays above specialty_keep_share of the best share; then the next best
#    window at least specialty_min_distance_km away, at most
#    specialty_max_per_crop windows per crop
# 4. the windows are read at full resolution and shown as panels, next to a
#    map of Germany with the numbered windows
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
window_cells <- definitions$specialty_window_km / definitions$specialty_cell_km

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

# 2. windows: sums over k x k cells with summed-area tables -------------------

window_sum <- function(m, k) {
  sat <- matrix(0, nrow(m) + 1, ncol(m) + 1)
  sat[-1, -1] <- t(apply(apply(m, 2, cumsum), 1, cumsum))
  rows <- seq_len(nrow(m) - k + 1)
  cols <- seq_len(ncol(m) - k + 1)
  sat[rows + k, cols + k] - sat[rows, cols + k] - sat[rows + k, cols] + sat[rows, cols]
}

pixel_ha <- search_res^2 / 1e4

candidates <- expand_grid(code = candidate_codes, k = window_cells) |>
  pmap(\(code, k) {
    crop <- window_sum(counts[, , as.character(code)], k)
    farm <- window_sum(farmland, k)
    share <- crop / pmax(farm, 1)
    lq <- share / national_share[as.character(code)]
    valid <- farm >= definitions$specialty_min_farmland * (k * cell_px)^2 &
      crop * pixel_ha >= definitions$specialty_min_crop_ha &
      share >= definitions$specialty_min_share &
      lq >= definitions$specialty_min_lq
    if (!any(valid)) return(NULL)
    idx <- which(valid, arr.ind = TRUE)
    tibble(
      code = code, k = k, row = idx[, 1], col = idx[, 2],
      share = share[valid], lq = lq[valid], crop_ha = crop[valid] * pixel_ha
    )
  }) |>
  list_rbind() |>
  mutate(score = share * log(lq))

# window extent (map CRS) of top-left cell row, column and size k
cell_m <- cell_px * search_res
window_extent <- function(row, col, k) {
  e <- ext(ctm_search)
  ext(
    xmin(e) + (col - 1) * cell_m, xmin(e) + (col - 1 + k) * cell_m,
    ymax(e) - (row - 1 + k) * cell_m, ymax(e) - (row - 1) * cell_m
  )
}

# 3. greedy selection --------------------------------------------------------

centre <- \(row, col, k) c(col - 1 + k / 2, row - 1 + k / 2) * cell_m

select_windows <- function(candidates) {
  selected <- list()
  pool <- candidates |> arrange(desc(score))

  while (length(selected) < definitions$specialty_panels && nrow(pool) > 0) {
    seed <- pool[1, ]
    # the largest window of the crop that contains the seed and keeps enough
    # of its share
    window <- candidates |>
      filter(
        code == seed$code, k >= seed$k,
        row <= seed$row, row + k >= seed$row + seed$k,
        col <= seed$col, col + k >= seed$col + seed$k,
        share >= definitions$specialty_keep_share * seed$share
      ) |>
      arrange(desc(k), desc(share)) |>
      slice(1)
    selected <- c(selected, list(window))

    # remove candidates near the window and crops with enough windows
    chosen <- bind_rows(selected)
    per_crop <- count(chosen, code)
    c0 <- centre(window$row, window$col, window$k)
    pool <- pool |>
      filter(!code %in% per_crop$code[per_crop$n >= definitions$specialty_max_per_crop]) |>
      mutate(
        dx = (col - 1 + k / 2) * cell_m - c0[1],
        dy = (row - 1 + k / 2) * cell_m - c0[2]
      ) |>
      filter(sqrt(dx^2 + dy^2) >= definitions$specialty_min_distance_km * 1000) |>
      select(-dx, -dy)
  }
  bind_rows(selected)
}

windows <- select_windows(candidates) |>
  mutate(
    number = row_number(),
    extent = pmap(list(row, col, k), window_extent)
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

ctm_full <- rast(ctm_source)
max_plot_px <- 800 # larger windows are resampled to this many pixels

# window (extent in the search CRS) at full resolution: cut from the map in
# its own CRS with a margin, then warped to the search CRS (nearest
# neighbour) on a grid of at most max_plot_px pixels
read_window <- function(extent) {
  size <- xmax(extent) - xmin(extent)
  res_out <- max(10, size / max_plot_px)
  template <- rast(extent, resolution = res_out, crs = crs_search)
  margin <- st_bbox(st_buffer(st_as_sfc(st_bbox(
    c(xmin = xmin(extent), ymin = ymin(extent), xmax = xmax(extent), ymax = ymax(extent)),
    crs = st_crs(crs_search)
  )), size * 0.1))
  window_source <- st_bbox(st_transform(st_as_sfc(margin), crs(ctm_full)))
  r <- crop(ctm_full, ext(window_source[c("xmin", "xmax", "ymin", "ymax")]))
  r <- project(r, template, method = "near")
  as.data.frame(r, xy = TRUE, na.rm = TRUE) |>
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

plot_window <- function(number, class, place, k, share, lq, cells, ...) {
  size_km <- k * definitions$specialty_cell_km
  ggplot(cells, aes(x, y, fill = code)) +
    geom_raster() +
    # the legend is drawn once, by the overview map
    scale_fill_ctm(breaks = legend_breaks) +
    guides(fill = "none") +
    coord_equal(expand = FALSE) +
    labs(
      title = str_c(number, "  ", class),
      subtitle = str_glue(
        "{place} \u00B7 {size_km} \u00D7 {size_km} km\n",
        "{scales::label_percent()(share)} of the farmland, ",
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
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 5))) +
  geom_sf(data = window_polygons, fill = NA, colour = "grey15", linewidth = 0.5) +
  geom_label(
    aes(x, y, label = number),
    data = as_tibble(st_coordinates(window_centres)) |>
      transmute(x = X, y = Y, number = windows$number),
    size = 3.4,
    fontface = "bold",
    label.padding = unit(0.15, "lines"),
    nudge_y = 18000
  ) +
  coord_sf(datum = NA) +
  theme_maps() +
  theme(plot.margin = margin(4, 12, 4, 4))

# overview map in the first column over all rows, panels in 3 columns
n_panel_cols <- 3
n_panel_rows <- ceiling(length(panels) / n_panel_cols)
# patchwork::area(), terra has an area() too
design <- c(
  patchwork::area(1, 1, n_panel_rows, 1),
  map(seq_along(panels), \(i) {
    patchwork::area((i - 1) %/% n_panel_cols + 1, (i - 1) %% n_panel_cols + 2)
  }) |> do.call(what = c)
)

plot_specialty <- wrap_plots(c(list(overview), panels), design = design) +
  plot_layout(widths = c(1.4, rep(1, n_panel_cols)), guides = "collect") +
  plot_annotation(
    title = "Regions specialised in one crop",
    subtitle = str_wrap(str_glue(
      "Windows where a crop that is rare in Germany dominates the farmland, ",
      "found from the crop shares per {definitions$specialty_cell_km} km cell ",
      "(share \u00D7 log of the location quotient, at most ",
      "{definitions$specialty_max_per_crop} windows per crop); window sizes ",
      "differ. Numbers as on the map."
    ), 130),
    caption = caption_ctm,
    theme = theme_maps()
  ) &
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = rel(0.8))
  )

save_plot(
  str_glue("plots/specialty_regions_{ctm_year}.png"),
  plot_specialty,
  width = 16, height = 4.2 * n_panel_rows + 2, dpi = 300, bg = "white"
)

print(windows |> select(number, class, place, k, share, lq, crop_ha), n = Inf)
