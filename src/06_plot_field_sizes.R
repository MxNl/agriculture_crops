# Field size maps and charts from the field tables of
# src/05_field_sizes_stats.R:
#
# - plots/field_size_map.png: median field size per map cell
# - plots/field_size_vs_median_map.png: median field size per map cell
#   relative to the median of all fields, binned diverging scale
# - plots/field_size_area_share.png: share of farmland in fields up to a given
#   size per state; the size where the curve crosses 50% is the size of the
#   field the typical hectare lies in
# - plots/field_size_zoom.png: the real field polygons in a few landscapes,
#   coloured like the map
# - plots/field_size_by_crop.png: median field size per crop in the states
#   whose fields carry crops
#
# What one polygon is differs between states (fields, reference parcels or
# field blocks, see definitions.yml); field blocks hold several fields and are
# larger. Run from the project root.

library(arrow)
library(patchwork)

# borders, settings and theme
source("src/functions.R")

# settings ------------------------------------------------------------------

collections <- bind_rows(definitions$field_collections)
block_collections <- bind_rows(definitions$field_block_collections)
min_ha <- definitions$field_min_ha
cell_m <- definitions$field_cell_km * 1000
min_count <- definitions$field_min_count
zoom_km <- definitions$field_zoom_km

units <- c("field", "reference parcel", "field block")
unit_labels <- c(
  "field" = "fields",
  "reference parcel" = "reference parcels",
  "field block" = "field blocks"
)

caption <- str_c(
  "Data: Fields of the World, harmonised field boundaries of the state ",
  "agricultural administrations (source.coop/ftw/harmonized-field-data), ",
  "DL-DE-BY-2.0, DL-DE-ZERO-2.0 and CC BY 4.0. Borders: Natural Earth."
)

caption_chart <- str_remove(caption, " Borders: Natural Earth\\.")

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

# map cells ---------------------------------------------------------------------

cells <- fields |>
  mutate(
    x = (floor(x / cell_m) + 0.5) * cell_m,
    y = (floor(y / cell_m) + 0.5) * cell_m
  ) |>
  group_by(x, y) |>
  summarise(n = n(), median_ha = median(area_ha), .groups = "drop") |>
  filter(n >= min_count)

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

# field size on a log scale, binned; shared by the map and the zoom windows
size_breaks <- c(0.5, 1, 2, 3, 5, 10, 20, 50)
scale_fill_size <- function(...) {
  scale_fill_viridis_b(
    option = "viridis",
    direction = -1,
    transform = "log10",
    breaks = size_breaks,
    labels = label_ha_short,
    limits = c(0.1, 500),
    oob = scales::squish,
    name = "Field size",
    ...
  )
}

plot_cell_map <- function(fill, fill_scale, title, subtitle) {
  cells |>
    mutate(fill = fill) |>
    ggplot(aes(x, y, fill = fill)) +
    geom_raster() +
    fill_scale +
    geom_borders() +
    geom_state_labels() +
    coord_equal() +
    labs(title = title, subtitle = subtitle, caption = str_wrap(caption, 110)) +
    theme_maps() +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(2.5, "cm"),
      legend.key.height = unit(0.4, "cm"),
      legend.title = element_text(vjust = 0.9, margin = margin(r = 10))
    )
}

not_covered <- str_c(
  "No comparable open data\nfor Baden-W\u00FCrttemberg, Rhineland-Palatinate ",
  "and Mecklenburg-Western Pomerania."
)

# 1. median field size --------------------------------------------------------

plot_size <- plot_cell_map(
  fill = cells$median_ha,
  fill_scale = scale_fill_size(),
  title = "How large are Germany's agricultural fields?",
  subtitle = str_glue(
    "Median size of the fields (\u2265 {min_ha} ha) per ",
    "{definitions$field_cell_km} km cell; cells with < {min_count} fields ",
    "are blank.\nWhere states publish no fields, reference parcels or ",
    "field blocks are shown;\nfield blocks hold several fields and are ",
    "larger. {not_covered}"
  )
)

ggsave(
  "plots/field_size_map.png",
  plot_size,
  width = 9, height = 11.5, dpi = 300, bg = "white"
)

# 2. larger or smaller than the median -----------------------------------------

# ratio to the median of all fields, log2 bins; the middle bin (2/3 to 3/2
# of the median) is neutral
ratio_breaks <- c(1 / 4, 1 / 2, 2 / 3, 3 / 2, 2, 4)
ratio_labels <- c("\u00BC\u00D7", "\u00BD\u00D7", "\u2154\u00D7", "1\u00BD\u00D7", "2\u00D7", "4\u00D7")

plot_ratio <- plot_cell_map(
  fill = cells$median_ha / national_median,
  fill_scale = scale_fill_fermenter(
    palette = "RdBu",
    direction = 1,
    transform = "log2",
    breaks = ratio_breaks,
    labels = ratio_labels,
    limits = c(1 / 8, 8),
    oob = scales::squish,
    name = "Relative to the\nmedian of all fields"
  ),
  title = "Larger or smaller than the typical field?",
  subtitle = str_glue(
    "Median field size per {definitions$field_cell_km} km cell relative to ",
    "the median of all {scales::label_comma()(nrow(fields))}\npolygons ",
    "({label_ha(national_median)}). Blue = larger, red = smaller, light = ",
    "\u2154\u00D7 to 1\u00BD\u00D7 the median.\nField ",
    "blocks (SH, SN, ST, TH) hold several fields and are larger. {not_covered}"
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
theme_chart <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.4)),
      plot.title.position = "plot",
      plot.subtitle = element_text(colour = "grey35", margin = margin(t = 6, b = 14)),
      plot.caption = element_text(colour = "grey50", size = rel(0.7), margin = margin(t = 14)),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

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
  theme_chart() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.key.width = unit(1.4, "cm"),
    legend.background = element_rect(fill = "white", colour = NA)
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
  theme_chart() +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = rel(0.95)))

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
    caption = str_wrap(caption_chart, 140),
    theme = theme_chart()
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
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(colour = "grey35", hjust = 0.5, margin = margin(t = 3, b = 5)),
      panel.background = element_rect(fill = "grey95", colour = NA),
      plot.margin = margin(6, 6, 6, 6)
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
    caption = str_wrap(caption_chart, 140),
    theme = theme_maps()
  ) &
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2.5, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.title = element_text(vjust = 0.9, margin = margin(r = 10))
  )

ggsave(
  "plots/field_size_zoom.png",
  plot_zoom,
  width = 12, height = 9.5, dpi = 300, bg = "white"
)

# 5. field size by crop -----------------------------------------------------

crop_top_n <- 15

crop_fields <- fields |>
  filter(!is.na(crop))

top_crops <- crop_fields |>
  group_by(crop) |>
  summarise(area_ha = sum(area_ha), .groups = "drop") |>
  slice_max(area_ha, n = crop_top_n) |>
  pull(crop)

crop_medians <- crop_fields |>
  filter(crop %in% top_crops) |>
  group_by(crop, short) |>
  summarise(n = n(), median_ha = median(area_ha), .groups = "drop") |>
  filter(n >= 100) |>
  # HCAT names like winter_common_soft_wheat
  mutate(
    crop = str_to_sentence(str_replace_all(crop, "_", " ")),
    crop = fct_reorder(crop, median_ha, .fun = max)
  )

crop_states <- collections |> filter(short %in% crop_medians$short)

plot_crop <- crop_medians |>
  ggplot(aes(median_ha, crop)) +
  geom_line(aes(group = crop), colour = "grey80", linewidth = 1.2) +
  geom_point(aes(colour = short), size = 3.2) +
  scale_x_log10(breaks = c(0.5, 1, 2, 5, 10, 20, 50), labels = label_ha_short) +
  scale_colour_manual(
    values = state_colours,
    breaks = crop_states$short,
    labels = str_c(crop_states$short, " ", crop_states$state),
    name = NULL
  ) +
  labs(
    title = "Field size by crop",
    subtitle = str_glue(
      "Median field size of the {crop_top_n} crops with the largest area, in ",
      "the states that publish fields with crops (HCAT crop names)."
    ),
    caption = str_wrap(caption_chart, 120),
    x = "Median field size (log scale)",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.4)),
    plot.title.position = "plot",
    plot.subtitle = element_text(colour = "grey35", margin = margin(t = 6, b = 14)),
    plot.caption = element_text(colour = "grey50", size = rel(0.7), margin = margin(t = 14)),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(colour = "grey92"),
    legend.position = "top",
    legend.justification = "left",
    plot.margin = margin(15, 15, 10, 15),
    plot.background = element_rect(fill = "white", colour = NA)
  )

ggsave(
  "plots/field_size_by_crop.png",
  plot_crop,
  width = 11, height = 8, dpi = 300, bg = "white"
)
