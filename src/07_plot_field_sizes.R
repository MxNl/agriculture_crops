# Field size maps and charts from the field tables of
# src/05_field_sizes_stats.R and the DLR crops of src/06_field_crops_dlr.R:
#
# - plots/field_size_map.png: median field size per hexagon
# - plots/field_size_vs_median_map.png: median field size per hexagon in
#   percent of the median of all polygons, binned diverging scale
# - plots/field_size_area_share.png: share of farmland in fields up to a given
#   size per state; the size where the curve crosses 50% is the size of the
#   field the typical hectare lies in
# - plots/field_size_zoom.png: the real field polygons in a few landscapes,
#   coloured like the map
# - plots/field_size_by_crop.png: median field size per crop (DLR) and state
#
# What one polygon is differs between states (fields, reference parcels or
# field blocks, see definitions.yml); field blocks hold several fields and are
# larger. Run from the project root.

library(arrow)
library(patchwork)

# legend, borders, settings and theme
source("src/functions.R")

# settings ------------------------------------------------------------------

collections <- bind_rows(definitions$field_collections)
block_collections <- bind_rows(definitions$field_block_collections)
min_ha <- definitions$field_min_ha
min_count <- definitions$field_min_count
zoom_km <- definitions$field_zoom_km
min_share <- definitions$field_crop_min_share
dlr_year <- max(years)

units <- c("field", "reference parcel", "field block")
unit_labels <- c(
  "field" = "fields",
  "reference parcel" = "reference parcels",
  "field block" = "field blocks"
)

caption_fields <- str_c(
  "Data: Fields of the World, harmonised field boundaries of the state ",
  "agricultural administrations (source.coop/ftw/harmonized-field-data), ",
  "DL-DE-BY-2.0, DL-DE-ZERO-2.0 and CC BY 4.0."
)
caption_maps <- str_c(caption_fields, " Borders: Natural Earth.")

# one colour per state, used in the charts
state_colours <- set_names(
  palette.colors(nrow(collections), "Tableau 10"),
  collections$short
)

# agricultural polygons of a set of collections
read_fields <- function(collections) {
  collections |>
    mutate(
      fields = map(collection, \(c) {
        read_parquet(str_glue("results/fields_ftw_{c}.parquet"), col_select = -collection)
      })
    ) |>
    unnest(fields) |>
    filter(land_use != "other", area_ha >= min_ha) |>
    mutate(unit = factor(unit, levels = units))
}

fields <- read_fields(collections)
national_median <- median(fields$area_ha)

label_ha <- scales::label_number(accuracy = 0.1, suffix = " ha")
label_ha_short <- \(x) str_c(scales::label_number(drop0trailing = TRUE)(x), " ha")

# hexagons --------------------------------------------------------------------

# hexagons of field_hex_km2; st_make_grid() takes the distance between
# opposite edges, area = sqrt(3) / 2 * d^2
hex_d <- sqrt(2 * definitions$field_hex_km2 * 1e6 / sqrt(3))
hexagons <- st_make_grid(states, cellsize = hex_d, square = FALSE)

# each field to the hexagon with the nearest centre, i.e. the one it lies in
fields$hex <- st_nearest_feature(
  st_as_sf(fields, coords = c("x", "y"), crs = definitions$crs_reference),
  st_centroid(hexagons)
)

hex_stats <- fields |>
  group_by(hex) |>
  summarise(n = n(), median_ha = median(area_ha), .groups = "drop") |>
  filter(n >= min_count) |>
  mutate(geometry = hexagons[hex]) |>
  st_as_sf()

# state labels at the median field position, with the unit below
state_labels <- fields |>
  group_by(short, unit) |>
  summarise(x = median(x), y = median(y), .groups = "drop") |>
  mutate(label = str_c(short, "\n", unit_labels[as.character(unit)]))

geom_state_labels <- function() {
  geom_label(
    aes(x, y, label = label),
    data = state_labels,
    inherit.aes = FALSE,
    size = 2.6,
    lineheight = 0.9,
    colour = "grey15",
    fill = alpha("white", 0.75),
    border.colour = NA
  )
}

# colour scales -----------------------------------------------------------------

# field size on a log scale, binned; shared by the map, the zoom windows and
# the crop chart. Colours are taken at the bin centres of the scico palette
size_breaks <- c(0.5, 1, 2, 3, 5, 10, 20, 50)
size_limits <- c(0.1, 500)
scale_fill_size <- function(name = "Field size", ...) {
  scale_fill_stepsn(
    colours = scico::scico(256, palette = definitions$field_palette_size, direction = -1),
    transform = "log10",
    breaks = size_breaks,
    labels = label_ha_short,
    limits = size_limits,
    oob = scales::squish,
    name = name,
    ...
  )
}

# text colour on the size colours: white on the dark half of the palette
text_on_size <- function(area_ha) {
  position <- scales::rescale(log10(pmin(pmax(area_ha, size_limits[1]), size_limits[2])), from = log10(size_limits))
  if_else(position > 0.55, "white", "grey10")
}

# field size relative to the median of all polygons. The bins are symmetric
# on a log scale: 80% and 125% are both 1.25 times smaller or larger than the
# median, as are 50% and 200%
ratio_breaks <- c(1 / 4, 1 / 2, 4 / 5, 5 / 4, 2, 4)
scale_fill_ratio <- function(...) {
  scale_fill_stepsn(
    # dark blue = larger, dark brown = smaller
    colours = scico::scico(256, palette = definitions$field_palette_ratio, direction = -1),
    transform = "log2",
    breaks = ratio_breaks,
    labels = scales::label_percent(),
    limits = c(1 / 8, 8),
    oob = scales::squish,
    name = "Median field size in percent\nof the median of all polygons",
    ...
  )
}

theme_legend_bottom <- function() {
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2.5, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.title = element_text(vjust = 0.9, margin = margin(r = 10))
  )
}

# maps ---------------------------------------------------------------------------

not_covered <- str_c(
  "No comparable open data for Baden-W\u00FCrttemberg, Rhineland-Palatinate ",
  "and Mecklenburg-Western Pomerania."
)

plot_hex_map <- function(fill, fill_scale, title, subtitle) {
  hex_stats |>
    mutate(fill = fill) |>
    ggplot() +
    geom_sf(aes(fill = fill), colour = NA) +
    fill_scale +
    geom_borders() +
    geom_state_labels() +
    coord_sf(crs = definitions$crs_reference, datum = NA) +
    labs(
      title = title,
      subtitle = str_wrap(subtitle, 88),
      caption = str_wrap(caption_maps, 110)
    ) +
    theme_maps() +
    theme_legend_bottom()
}

# 1. median field size --------------------------------------------------------

plot_size <- plot_hex_map(
  fill = hex_stats$median_ha,
  fill_scale = scale_fill_size(),
  title = "How large are Germany's agricultural fields?",
  subtitle = str_glue(
    "Median size of the fields (\u2265 {min_ha} ha) per hexagon of ",
    "{definitions$field_hex_km2} km\u00B2; hexagons with < {min_count} ",
    "fields are blank. Where states publish no fields, reference parcels ",
    "or field blocks are shown; field blocks hold several fields and are ",
    "larger. {not_covered}"
  )
)

ggsave(
  "plots/field_size_map.png",
  plot_size,
  width = 9, height = 11.5, dpi = 300, bg = "white"
)

# 2. larger or smaller than the median -----------------------------------------

plot_ratio <- plot_hex_map(
  fill = hex_stats$median_ha / national_median,
  fill_scale = scale_fill_ratio(),
  title = "Larger or smaller than the typical field?",
  subtitle = str_glue(
    "Median field size per hexagon in percent of the median of all ",
    "{scales::label_comma()(nrow(fields))} polygons ",
    # non-breaking space, so "1.4 ha" stays on one line
    "({str_replace(label_ha(national_median), ' ', '\u00A0')}). Blue = larger, brown = smaller; the ",
    "bins are symmetric in ratios (80% and 125% are both a factor of 1.25). ",
    "Field blocks (SH, SN, ST, TH) hold several fields and are larger. ",
    "{not_covered}"
  )
)

ggsave(
  "plots/field_size_vs_median_map.png",
  plot_ratio,
  width = 9, height = 11.5, dpi = 300, bg = "white"
)

# 3. share of farmland by field size -------------------------------------------

# cumulative share of the agricultural area in polygons up to a size, thinned
# to 400 points per collection
area_share_curve <- function(area_ha) {
  area_ha <- sort(area_ha)
  keep <- unique(round(seq(1, length(area_ha), length.out = 400)))
  tibble(
    size_ha = area_ha[keep],
    share = cumsum(area_ha)[keep] / sum(area_ha)
  )
}

curves <- bind_rows(fields, read_fields(block_collections)) |>
  group_by(collection, short, unit) |>
  reframe(area_share_curve(area_ha))

# size where half of the farmland lies in smaller and half in larger polygons
area_medians <- curves |>
  group_by(collection, short, unit) |>
  summarise(size_ha = approx(share, size_ha, 0.5, ties = "ordered")$y, .groups = "drop") |>
  left_join(
    bind_rows(collections, block_collections) |> select(collection, state),
    by = "collection"
  ) |>
  mutate(
    share = 0.5,
    label = str_c(state, " (", unit_labels[as.character(unit)], ")"),
    label = fct_reorder(label, size_ha)
  )

scale_colour_states <- scale_colour_manual(values = state_colours, guide = "none")
scale_linetype_units <- scale_linetype_manual(
  values = c("field" = "solid", "reference parcel" = "longdash", "field block" = "dotted"),
  labels = unit_labels,
  name = NULL
)

plot_curves <- curves |>
  ggplot(aes(size_ha, share, colour = short, linetype = unit)) +
  geom_hline(yintercept = 0.5, colour = "grey60", linewidth = 0.3) +
  geom_line(linewidth = 0.7) +
  geom_point(data = area_medians, size = 2.2, show.legend = FALSE) +
  scale_x_log10(
    breaks = c(0.1, 0.3, 1, 3, 10, 30, 100, 300),
    labels = label_ha_short
  ) +
  scale_y_continuous(labels = scales::label_percent(), expand = expansion(0.01)) +
  scale_colour_states +
  scale_linetype_units +
  labs(
    x = "Field size (log scale)",
    y = "Share of agricultural area in fields up to this size"
  ) +
  theme_charts() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.key.width = unit(1.4, "cm"),
    legend.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(0, 10, 0, 0)
  )

# the 50% crossings of the curves, sorted
plot_medians <- area_medians |>
  ggplot(aes(size_ha, label, colour = short)) +
  geom_segment(aes(x = 0, xend = size_ha, linetype = unit), linewidth = 0.7) +
  geom_point(size = 2.6) +
  geom_text(
    aes(label = label_ha_short(signif(size_ha, 2))),
    hjust = 0, nudge_x = 1.5, size = 3.6, show.legend = FALSE
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  scale_colour_states +
  scale_linetype_units +
  guides(linetype = "none") +
  labs(x = "Size of the field the typical hectare lies in", y = NULL) +
  theme_charts() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.margin = margin(0, 0, 0, 10)
  )

plot_share <- plot_curves + plot_medians +
  plot_layout(widths = c(1.4, 1)) +
  plot_annotation(
    title = "Where the farmland lies: in small or in large fields?",
    subtitle = str_c(
      "Left: share of each state's agricultural area in fields up to a given ",
      "size. Where a curve crosses 50%, half of the area lies in larger\n",
      "fields: the size of the field the typical hectare lies in (right). ",
      "For BB and NI, the field blocks are shown as well (dotted)."
    ),
    caption = caption_fields,
    theme = theme_charts()
  )

ggsave(
  "plots/field_size_area_share.png",
  plot_share,
  width = 15, height = 8, dpi = 300, bg = "white"
)

# 4. zoom windows -------------------------------------------------------------

zoom_windows <- definitions$field_zoom_windows |>
  bind_rows() |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(definitions$crs_reference) |>
  mutate(geometry = st_buffer(geometry, zoom_km * 500, endCapStyle = "SQUARE"))

zoom_fields <- st_read("results/fields_ftw_zoom.gpkg", quiet = TRUE) |>
  filter(land_use != "other", area_ha >= min_ha) |>
  left_join(collections |> select(collection, short, unit), by = "collection")

plot_zoom_window <- function(window) {
  bbox <- st_bbox(window)
  in_window <- zoom_fields |>
    filter(lengths(st_intersects(geom, window)) > 0)
  # fields whose centroid lies in the window
  centred <- in_window |>
    filter(x >= bbox$xmin, x <= bbox$xmax, y >= bbox$ymin, y <= bbox$ymax)

  ggplot(in_window) +
    geom_sf(aes(fill = area_ha), colour = "white", linewidth = 0.1) +
    scale_fill_size() +
    coord_sf(
      xlim = bbox[c("xmin", "xmax")],
      ylim = bbox[c("ymin", "ymax")],
      expand = FALSE,
      datum = NA
    ) +
    labs(
      title = window$label,
      subtitle = str_c(
        first(centred$short), " ", unit_labels[as.character(first(centred$unit))],
        ", median ", label_ha(median(centred$area_ha))
      )
    ) +
    theme_maps() +
    theme(
      # like the facet strips of the crop maps
      plot.title = element_text(size = rel(1), margin = margin(b = 2)),
      plot.subtitle = element_text(size = rel(0.85), margin = margin(b = 6)),
      panel.background = element_rect(fill = "grey95", colour = NA),
      plot.margin = margin(6, 8, 6, 8)
    )
}

plot_zoom <- zoom_windows |>
  split(seq_len(nrow(zoom_windows))) |>
  map(plot_zoom_window) |>
  wrap_plots(ncol = 3) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "What field sizes look like",
    subtitle = str_glue(
      "Agricultural polygons in {zoom_km} \u00D7 {zoom_km} km windows, ",
      "coloured by size as in the map; grey = no agricultural polygon."
    ),
    caption = str_wrap(caption_fields, 140),
    theme = theme_maps()
  ) &
  theme_legend_bottom()

ggsave(
  "plots/field_size_zoom.png",
  plot_zoom,
  width = 12, height = 9.5, dpi = 300, bg = "white"
)

# 5. field size by crop -----------------------------------------------------

crop_top_n <- 14 # crops with the largest single-crop area
crop_min_n <- 50 # fewer polygons per crop and state: tile left blank

crops <- collections$collection |>
  map(\(c) {
    read_parquet(str_glue("results/fields_ftw_{c}_dlr{dlr_year}.parquet")) |>
      mutate(collection = c)
  }) |>
  list_rbind()

# polygons dominated by one crop; class 111 (other agricultural use) is no crop
crop_fields <- fields |>
  inner_join(crops, by = c("collection", "id")) |>
  filter(dlr_share >= min_share, dlr_class != 111L)

top_classes <- crop_fields |>
  group_by(dlr_class) |>
  summarise(area_ha = sum(area_ha), .groups = "drop") |>
  slice_max(area_ha, n = crop_top_n) |>
  pull(dlr_class)

# crop names with a square in the legend colour of the crop maps
crop_labels <- legend |>
  filter(value %in% top_classes) |>
  transmute(
    dlr_class = value,
    crop_label = str_c(
      "<span style='color:", colour, "'>\u25A0</span> ", class
    )
  )

crop_medians <- crop_fields |>
  filter(dlr_class %in% top_classes) |>
  group_by(dlr_class, short, unit) |>
  summarise(n = n(), median_ha = median(area_ha), .groups = "drop") |>
  filter(n >= crop_min_n) |>
  left_join(crop_labels, by = "dlr_class") |>
  mutate(
    crop_label = fct_reorder(crop_label, median_ha, .fun = median),
    short = factor(short, levels = collections$short),
    # two lines, the facets of reference parcels are narrow
    unit = fct_relabel(unit, \(u) str_replace(unit_labels[u], " ", "\n"))
  )

# agreement with the declared crops, for the subtitle
validation <- read_csv("results/fields_ftw_dlr_validation.csv", show_col_types = FALSE) |>
  group_by(collection, year) |>
  summarise(agreement = weighted.mean(agreement_single, area_ha, na.rm = TRUE), .groups = "drop") |>
  left_join(collections |> select(collection, short), by = "collection")

validation_text <- str_c(
  validation$short, " ", validation$year, ": ",
  scales::label_percent(accuracy = 1)(validation$agreement),
  collapse = ", "
)

plot_crop <- crop_medians |>
  ggplot(aes(short, crop_label, fill = median_ha)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(
    aes(label = scales::label_number(accuracy = 0.1)(median_ha), colour = text_on_size(median_ha)),
    size = 3.2
  ) +
  scale_colour_identity() +
  scale_fill_size(name = "Median field size") +
  scale_x_discrete(position = "top") +
  facet_grid(cols = vars(unit), scales = "free_x", space = "free_x") +
  labs(
    title = "Field size by crop",
    subtitle = str_wrap(str_glue(
      "Median size of the polygons where one crop of the DLR CropTypes map ",
      "{dlr_year} covers \u2265 {scales::label_percent()(min_share)} of the ",
      "area, in ha. Check against the crops the farmers declared (same ",
      "year, single-crop fields): {validation_text} of the area agree."
    ), 110),
    caption = str_c(
      str_wrap(caption_fields, 140), "\n",
      str_remove(caption, " Borders: Natural Earth\\.")
    ),
    x = NULL,
    y = NULL
  ) +
  theme_charts() +
  theme(
    panel.grid = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    axis.text.y.left = ggtext::element_markdown(size = rel(1)),
    axis.text.x.top = element_text(face = "bold"),
    strip.text = element_text(face = "bold", lineheight = 1.1, margin = margin(b = 6)),
    strip.placement = "outside",
    panel.spacing.x = unit(1.2, "lines")
  ) +
  theme_legend_bottom()

ggsave(
  "plots/field_size_by_crop.png",
  plot_crop,
  width = 12, height = 9, dpi = 300, bg = "white"
)
