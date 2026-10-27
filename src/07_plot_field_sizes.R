# Field size maps and charts from the field tables of
# src/05_field_sizes_stats.R and the DLR crops of src/06_field_crops_dlr.R:
#
# - plots/field_size_map.png: median field size per hexagon
# - plots/field_size_vs_median_map.png: median field size per hexagon in
#   percent of the median of all polygons, binned diverging scale, beside
#   ridgelines of the size distribution per state
# - plots/field_size_area_share.png: share of farmland in fields up to a given
#   size per state; the size where the curve crosses 50% is the size of the
#   field the typical hectare lies in
# - plots/field_size_zoom.png: the real field polygons in windows chosen per
#   unit at percentiles of the median field size, coloured like the map
# - plots/field_size_by_crop.png: median field size per crop (DLR) and state
# - plots/field_size_by_crop_ridges.png: field size distribution per crop
#   (DLR), all states pooled
# - plots/field_size_crop_mix.png: crop shares per field size class
#   (marimekko, column width = area of the class)
# - plots/field_size_edges_map.png: field edges per ha of farmland per
#   hexagon and state
# - plots/field_size_crop_diversity_map.png: bivariate map of field size and
#   crop diversity (needs the 500 m class counts of src/03_crop_change_stats.R)
# - plots/field_size_time_series.png: field sizes over the yearly editions of
#   the collections in field_time_series
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
  summarise(
    n = n(),
    median_ha = median(area_ha),
    # field edges: length of the field outlines per ha of farmland
    edge_m_ha = sum(perimeter_m) / sum(area_ha),
    .groups = "drop"
  ) |>
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

# text in a crop colour on white: the hue of the colour at one lightness for
# all crops, so light colours (yellow, pale green) stay readable and no crop
# label stands out
text_on_white <- function(colour, lightness = 42) {
  hcl <- farver::decode_colour(colour, to = "hcl")
  hcl[, "l"] <- lightness
  farver::encode_colour(hcl, from = "hcl")
}

# field size on a log scale, binned; shared by the map, the zoom windows and
# the crop chart. Colours are taken at the bin centres of the scico palette
size_breaks <- c(0.5, 1, 2, 3, 5, 10, 20, 50)
size_limits <- c(0.1, 500)
scale_fill_size <- function(name = "Field size", aesthetics = "fill", ...) {
  scale_fill_stepsn(
    colours = scico::scico(256, palette = definitions$field_palette_size, direction = -1),
    transform = "log10",
    breaks = size_breaks,
    labels = label_ha_short,
    limits = size_limits,
    oob = scales::squish,
    name = name,
    aesthetics = aesthetics,
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
ratio_palette <- \(n) {
  scico::scico(
    n,
    palette = definitions$field_palette_ratio,
    begin = definitions$field_palette_ratio_range[1],
    end = definitions$field_palette_ratio_range[2]
  )
}
# colours for the words smaller and larger: at 20% and 80% of the palette,
# brighter than its ends
ratio_colours <- ratio_palette(11)[c(3, 9)]

scale_fill_ratio <- function(alpha = 1, aesthetics = "fill", ...) {
  scale_fill_stepsn(
    colours = scales::alpha(ratio_palette(256), alpha),
    transform = "log2",
    breaks = ratio_breaks,
    labels = scales::label_percent(),
    limits = c(1 / 8, 8),
    oob = scales::squish,
    name = "Median field size in percent\nof the median of all polygons",
    aesthetics = aesthetics,
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
    # outline in the fill colour, without it thin white seams show between
    # the hexagons (anti-aliasing)
    geom_sf(aes(fill = fill, colour = fill), linewidth = 0.1) +
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
  fill_scale = scale_fill_size(aesthetics = c("fill", "colour")),
  title = "How large are Germany's agricultural fields?",
  subtitle = str_glue(
    "Median size of the fields (\u2265 {min_ha} ha) per hexagon of ",
    "{definitions$field_hex_km2} km\u00B2; hexagons with < {min_count} ",
    "fields are blank. Where states publish no fields, reference parcels ",
    "or field blocks are shown; field blocks hold several fields and are ",
    "larger. {not_covered}"
  )
)

save_plot(
  "plots/field_size_map.png",
  plot_size,
  width = 9, height = 11.5, dpi = 300, bg = "white"
)

# 2. larger or smaller than the median -----------------------------------------

# the words larger and smaller in the end colours of the palette (markdown);
# wrapped before the colour tags are inserted, so no tag is split
colour_word <- \(word, colour) {
  str_c("<span style='color:", colour, "'>**", word, "**</span>")
}
ratio_subtitle <- str_glue(
  "The colours show where the fields are LARGER or SMALLER than the median ",
  "of all {scales::label_comma()(nrow(fields))} polygons ",
  # non-breaking space, so "1.4 ha" stays on one line
  "({str_replace(label_ha(national_median), ' ', '\u00A0')}): on the left ",
  "as median per hexagon, on the right as size distribution per state (line ",
  "= median of the state). The bins are symmetric in ratios (80% and 125% are ",
  "both a factor of 1.25). Field blocks hold several fields and are larger. ",
  "{not_covered}"
) |>
  str_wrap(150) |>
  str_replace("LARGER", colour_word("larger", ratio_colours[2])) |>
  str_replace("SMALLER", colour_word("smaller", ratio_colours[1])) |>
  str_replace_all("\n", "<br>")

map_ratio <- plot_hex_map(
  fill = hex_stats$median_ha / national_median,
  fill_scale = scale_fill_ratio(aesthetics = c("fill", "colour")),
  title = NULL,
  subtitle = NULL
) +
  labs(caption = NULL)

# size distribution per state, grouped by what one polygon is. Densities on
# the log scale; the fill follows the ratio to the median like the map
ridges_ratio <- fields |>
  mutate(
    ratio = area_ha / national_median,
    state = fct_reorder(str_c(state, " (", short, ")"), area_ha, .fun = median),
    # two lines, the strip of the reference parcels is short
    unit = fct_relabel(unit, \(u) str_replace(unit_labels[u], " ", "\n"))
  ) |>
  ggplot(aes(ratio, state)) +
  geom_vline(xintercept = 1, colour = "grey60", linewidth = 0.3) +
  ggridges::geom_density_ridges_gradient(
    # ggridges returns x back-transformed, i.e. as ratio
    aes(fill = after_stat(x)),
    bandwidth = 0.25,
    scale = 1.15,
    rel_min_height = 0.005,
    colour = "white",
    linewidth = 0.5,
    quantile_lines = TRUE,
    quantiles = 2
  ) +
  # slightly transparent (geom_density_ridges_gradient has no alpha); the map
  # shows the legend
  scale_fill_ratio(alpha = 0.8, guide = "none") +
  scale_x_continuous(
    transform = "log2",
    breaks = 2^seq(-3, 3),
    labels = scales::label_percent(big.mark = ","),
    limits = c(1 / 16, 64),
    expand = expansion(0),
    sec.axis = sec_axis(
      \(x) x * national_median,
      breaks = c(0.1, 0.5, 1, 2, 5, 10, 20, 50),
      labels = label_ha_short
    )
  ) +
  scale_y_discrete(expand = expansion(add = c(0.2, 1))) +
  facet_grid(rows = vars(unit), scales = "free_y", space = "free_y", switch = "y") +
  labs(x = "Polygon size in percent of the median of all polygons", y = NULL) +
  theme_charts() +
  theme(
    panel.grid.major.y = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(face = "bold", angle = 90, lineheight = 1.1),
    axis.text.y = element_text(vjust = 0),
    axis.title.x.bottom = element_text(margin = margin(t = 12)),
    # space below the subtitle; the map is free and not moved by it
    plot.margin = margin(60, 0, 0, 20)
  )

# legend below the map only. The map is limited by its height (fixed
# aspect), so it spans the ridges and a spacer below them: the map gets
# larger, the ridges keep their size. wrap_elements(): the map is not aligned
# with the ridges (their axis and top margin), so it starts right below the
# subtitle
plot_ratio <- wrap_elements(full = map_ratio) + ridges_ratio + plot_spacer() +
  plot_layout(
    design = "AB\nAC",
    widths = c(1.5, 1),
    heights = c(1, 0.18)
  ) +
  plot_annotation(
    title = "Larger or smaller than the typical field?",
    subtitle = ratio_subtitle,
    caption = str_wrap(caption_maps, 150),
    theme = theme_maps() +
      theme(
        plot.subtitle = ggtext::element_markdown(
          colour = "grey35", hjust = 0.5, halign = 0.5, lineheight = 1.2,
          margin = margin(t = 12, b = 0)
        )
      )
  ) &
  theme_legend_bottom()

save_plot(
  "plots/field_size_vs_median_map.png",
  plot_ratio,
  width = 17, height = 12.3, dpi = 300, bg = "white"
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

save_plot(
  "plots/field_size_area_share.png",
  plot_share,
  width = 15, height = 8, dpi = 300, bg = "white"
)

# 4. zoom windows -------------------------------------------------------------

# windows chosen from the data, not by hand: per unit the hexagons at the
# field_zoom_percentiles of the median field size, among the hexagons that
# are mostly farmland and whose fields all come from one collection
zoom_percentiles <- definitions$field_zoom_percentiles
hex_area_ha <- definitions$field_hex_km2 * 100

hex_units <- fields |>
  count(hex, collection, unit, short, state, wt = area_ha, name = "farm_ha") |>
  group_by(hex) |>
  filter(n() == 1) |>
  ungroup() |>
  inner_join(hex_stats |> st_drop_geometry() |> select(hex, median_ha), by = "hex") |>
  filter(farm_ha / hex_area_ha >= definitions$field_zoom_min_farmland)

# per unit and percentile the hexagon whose median is closest (log scale)
zoom_windows <- hex_units |>
  group_by(unit) |>
  reframe(
    percentile = zoom_percentiles,
    hex = map_int(zoom_percentiles, \(q) {
      target <- quantile(median_ha, q)
      hex[which.min(abs(log(median_ha) - log(target)))]
    })
  ) |>
  left_join(hex_units |> select(hex, collection, short, state), by = "hex") |>
  mutate(
    # rows from small to large polygons, as the columns
    unit = fct_relevel(unit, "reference parcel", "field", "field block"),
    window = map(hex, \(h) {
      st_buffer(st_centroid(hexagons[h]), zoom_km * 500, endCapStyle = "SQUARE")
    })
  )

plot_zoom_window <- function(collection, state, window) {
  window <- st_sfc(window, crs = definitions$crs_reference)
  bbox <- st_bbox(window)
  in_window <- read_fields_window(collection, window) |>
    filter(land_use != "other", area_ha >= min_ha)
  # median of the fields whose centroid lies in the window
  centred <- in_window |>
    filter(lengths(st_within(st_centroid(geometry), window)) > 0)

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
      title = state,
      subtitle = str_c("median ", label_ha(median(centred$area_ha)))
    ) +
    theme_maps() +
    theme(
      plot.title = element_text(size = rel(0.9), margin = margin(b = 2)),
      plot.subtitle = element_text(size = rel(0.8), margin = margin(b = 6)),
      panel.background = element_rect(fill = "grey95", colour = NA),
      plot.margin = margin(4, 8, 4, 8)
    )
}

zoom_panels <- zoom_windows |>
  arrange(unit, percentile) |>
  select(collection, state, window) |>
  pmap(plot_zoom_window)

# row labels (units) and column headers (percentiles) as text cells
text_cell <- function(label, rot = 0, y = 0.5) {
  wrap_elements(full = grid::textGrob(
    label,
    rot = rot,
    y = y,
    gp = grid::gpar(fontsize = 13, fontface = "bold", lineheight = 1.1)
  ))
}
column_headers <- str_c(
  c("Small", "Typical", "Large")[seq_along(zoom_percentiles)],
  "\n(", scales::label_ordinal()(100 * zoom_percentiles), " percentile)"
)
if (length(zoom_percentiles) != 3) {
  column_headers <- str_c(scales::label_ordinal()(100 * zoom_percentiles), " percentile")
}
row_labels <- str_to_sentence(unit_labels[levels(zoom_windows$unit)])

n_col <- length(zoom_percentiles)
cells <- c(
  list(plot_spacer()),
  # headers in the upper part of their row, away from the panel titles
  map(column_headers, \(label) text_cell(label, y = 0.62)),
  zoom_panels |>
    split(rep(seq_along(row_labels), each = n_col)) |>
    map2(row_labels, \(row, label) c(list(text_cell(label, rot = 90)), row)) |>
    list_flatten()
)

plot_zoom <- wrap_plots(
  cells,
  ncol = n_col + 1,
  widths = c(0.12, rep(1, n_col)),
  heights = c(0.2, rep(1, length(row_labels)))
) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "What field sizes look like",
    subtitle = str_wrap(str_glue(
      "Agricultural polygons in {zoom_km} \u00D7 {zoom_km} km windows, ",
      "coloured by size as in the map; grey = no agricultural polygon. ",
      "Windows chosen from the data: per row the hexagons at the ",
      "{str_flatten_comma(scales::label_ordinal()(100 * zoom_percentiles), ' and ')} ",
      "percentile of the median polygon size, among the hexagons with ",
      "\u2265 {scales::label_percent()(definitions$field_zoom_min_farmland)} ",
      "farmland."
    ), 110),
    caption = str_wrap(caption_fields, 140),
    theme = theme_maps()
  ) &
  theme_legend_bottom()

save_plot(
  "plots/field_size_zoom.png",
  plot_zoom,
  width = 12, height = 13, dpi = 300, bg = "white"
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

save_plot(
  "plots/field_size_by_crop.png",
  plot_crop,
  width = 12, height = 9, dpi = 300, bg = "white"
)

# 6. field size distribution by crop -----------------------------------------

# all states pooled; crops grown mostly where field blocks are mapped (east)
# look larger partly because of the blocks
crop_ridges <- crop_fields |>
  filter(dlr_class %in% top_classes) |>
  left_join(crop_labels, by = "dlr_class") |>
  left_join(legend |> select(dlr_class = value, colour), by = "dlr_class") |>
  mutate(crop_label = fct_reorder(crop_label, area_ha, .fun = median))

crop_ridge_medians <- crop_ridges |>
  group_by(crop_label, colour) |>
  summarise(n = n(), median_ha = median(area_ha), .groups = "drop") |>
  mutate(label_colour = text_on_white(colour))

plot_crop_ridges <- crop_ridges |>
  ggplot(aes(area_ha, crop_label)) +
  ggridges::geom_density_ridges(
    aes(fill = colour),
    colour = "white",
    bandwidth = 0.08,
    scale = 1.2,
    rel_min_height = 0.005,
    alpha = 0.8,
    linewidth = 0.5,
    quantile_lines = TRUE,
    quantiles = 2
  ) +
  # median of each crop at the right edge
  geom_text(
    aes(
      x = size_limits[2], y = crop_label, colour = label_colour,
      label = str_c(label_ha(median_ha), "  (n = ", scales::label_comma()(n), ")")
    ),
    data = crop_ridge_medians,
    inherit.aes = FALSE,
    hjust = 1,
    vjust = -0.6,
    size = 3.3
  ) +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_x_log10(
    breaks = c(0.1, size_breaks, 100, 500),
    labels = label_ha_short,
    expand = expansion(0)
  ) +
  scale_y_discrete(expand = expansion(add = c(0.2, 1.4))) +
  # zoom instead of scale limits, so polygons beyond them still count
  coord_cartesian(xlim = size_limits) +
  labs(
    title = "Field size distribution by crop",
    subtitle = str_wrap(str_glue(
      "Size of the polygons where one crop of the DLR CropTypes map ",
      "{dlr_year} covers \u2265 {scales::label_percent()(min_share)} of the ",
      "area, all states pooled (fields, reference parcels and field blocks); ",
      "line and label: median. Crops grown mainly in the states with field ",
      "blocks (SH, SN, ST, TH) look larger partly because blocks hold ",
      "several fields."
    ), 112),
    caption = str_c(
      str_wrap(caption_fields, 140), "\n",
      str_remove(caption, " Borders: Natural Earth\\.")
    ),
    x = "Polygon size (log scale)",
    y = NULL
  ) +
  theme_charts() +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid.major.y = element_blank(),
    axis.text.y.left = ggtext::element_markdown(size = rel(1), vjust = 0),
    axis.title.x = element_text(margin = margin(t = 12)),
    plot.subtitle = element_text(margin = margin(t = 12, b = 4))
  )

save_plot(
  "plots/field_size_by_crop_ridges.png",
  plot_crop_ridges,
  width = 12, height = 10, dpi = 300, bg = "white"
)

# 7. crop mix by field size ----------------------------------------------------

# area of each crop per field size class, single-crop polygons of all states;
# column width = area of the size class (marimekko)
size_class_breaks <- c(min_ha, 1, 2, 5, 10, 20, 50, Inf)
size_class_labels <- c(
  "< 1 ha", "1\u20132 ha", "2\u20135 ha", "5\u201310 ha",
  "10\u201320 ha", "20\u201350 ha", "> 50 ha"
)
other_crops_label <- "Other crops"

crop_mix <- crop_fields |>
  mutate(
    size_class = cut(area_ha, size_class_breaks, size_class_labels, right = FALSE),
    dlr_class = if_else(dlr_class %in% top_classes, dlr_class, NA_integer_)
  ) |>
  group_by(size_class, dlr_class) |>
  summarise(area_ha = sum(area_ha), .groups = "drop") |>
  left_join(legend |> select(dlr_class = value, class, colour), by = "dlr_class") |>
  mutate(
    class = coalesce(class, other_crops_label),
    colour = coalesce(colour, "grey80")
  ) |>
  group_by(size_class) |>
  mutate(class_area = sum(area_ha), share = area_ha / class_area) |>
  ungroup()

# crops in the order of their median field size, as in the ridgelines (small
# at the bottom); other crops on top
crop_order <- c(
  levels(fct_reorder(
    crop_fields |> filter(dlr_class %in% top_classes) |>
      left_join(legend |> select(dlr_class = value, class), by = "dlr_class") |>
      pull(class),
    crop_fields |> filter(dlr_class %in% top_classes) |> pull(area_ha),
    .fun = median
  )),
  other_crops_label
)

# column positions: cumulative area of the size classes
crop_mix_columns <- crop_mix |>
  distinct(size_class, class_area) |>
  arrange(size_class) |>
  mutate(
    xmax = cumsum(class_area) / sum(class_area),
    xmin = lag(xmax, default = 0)
  )

crop_mix_rects <- crop_mix |>
  mutate(class = factor(class, levels = crop_order)) |>
  arrange(size_class, class) |>
  group_by(size_class) |>
  mutate(ymax = cumsum(share), ymin = lag(ymax, default = 0)) |>
  ungroup() |>
  left_join(crop_mix_columns |> select(size_class, xmin, xmax), by = "size_class")

crop_colours <- crop_mix |>
  distinct(class, colour) |>
  deframe()

plot_crop_mix <- crop_mix_rects |>
  ggplot() +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = class),
    colour = "white",
    linewidth = 0.4
  ) +
  # size class and its share of the area below each column
  geom_text(
    aes(
      x = (xmin + xmax) / 2, y = -0.02,
      label = str_c(size_class, "\n", scales::label_percent(accuracy = 1)(xmax - xmin))
    ),
    data = crop_mix_columns,
    vjust = 1,
    size = 3.3,
    lineheight = 0.95,
    colour = "grey25"
  ) +
  scale_fill_manual(values = crop_colours, breaks = rev(crop_order), name = NULL) +
  scale_x_continuous(expand = expansion(0)) +
  scale_y_continuous(
    labels = scales::label_percent(),
    expand = expansion(mult = c(0.09, 0.01))
  ) +
  labs(
    title = "Which crops grow on small and on large fields?",
    subtitle = str_wrap(str_glue(
      "Share of each crop in the area of a field size class; column width = ",
      "share of the class in the total area (below the size). Polygons where ",
      "one crop of the DLR CropTypes map {dlr_year} covers ",
      "\u2265 {scales::label_percent()(min_share)} of the area, all states ",
      "pooled; field blocks hold several fields and shift crops to the large ",
      "classes."
    ), 105),
    caption = str_c(
      str_wrap(caption_fields, 140), "\n",
      str_remove(caption, " Borders: Natural Earth\\.")
    ),
    x = NULL,
    y = "Share of the area of the size class"
  ) +
  theme_charts() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    plot.subtitle = element_text(margin = margin(t = 12, b = 12))
  )

save_plot(
  "plots/field_size_crop_mix.png",
  plot_crop_mix,
  width = 13, height = 9, dpi = 300, bg = "white"
)

# 8. field edges ---------------------------------------------------------------

# length of the field outlines per ha of farmland: the smaller and the more
# irregular the fields, the more edges (margins, hedges, ditches)
edge_breaks <- c(150, 200, 250, 300, 400, 500, 700)
scale_fill_edges <- function(aesthetics = "fill", ...) {
  scale_fill_stepsn(
    colours = scico::scico(256, palette = definitions$field_palette_edges, direction = -1),
    breaks = edge_breaks,
    limits = c(100, 1000),
    oob = scales::squish,
    labels = scales::label_number(big.mark = ","),
    name = "Field edges per ha\nof farmland (m)",
    aesthetics = aesthetics,
    ...
  )
}

map_edges <- plot_hex_map(
  fill = hex_stats$edge_m_ha,
  fill_scale = scale_fill_edges(aesthetics = c("fill", "colour")),
  title = NULL,
  subtitle = NULL
) +
  labs(caption = NULL)

# per state, grouped by what one polygon is
edges_states <- fields |>
  group_by(state, short, unit) |>
  summarise(edge_m_ha = sum(perimeter_m) / sum(area_ha), .groups = "drop") |>
  mutate(
    state = fct_reorder(str_c(state, " (", short, ")"), edge_m_ha),
    unit = fct_relabel(unit, \(u) str_replace(unit_labels[u], " ", "\n"))
  )

bars_edges <- edges_states |>
  ggplot(aes(edge_m_ha, state, fill = edge_m_ha)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = scales::label_number(accuracy = 1)(edge_m_ha)),
    hjust = 0, nudge_x = 8, size = 3.6, colour = "grey25"
  ) +
  scale_fill_edges(guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  facet_grid(rows = vars(unit), scales = "free_y", space = "free_y", switch = "y") +
  labs(x = "Field edges per ha of farmland (m)", y = NULL) +
  theme_charts() +
  theme(
    panel.grid.major.y = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(face = "bold", angle = 90, lineheight = 1.1),
    axis.title.x = element_text(margin = margin(t = 12)),
    plot.margin = margin(60, 0, 0, 20)
  )

plot_edges <- wrap_elements(full = map_edges) + bars_edges + plot_spacer() +
  plot_layout(design = "AB\nAC", widths = c(1.5, 1), heights = c(1, 0.6)) +
  plot_annotation(
    title = "Field edges: where farmland is fine-grained",
    subtitle = str_wrap(str_c(
      "Length of the field outlines per hectare of farmland, per hexagon ",
      "(left) and state (right). Small and irregular fields have more edges, ",
      "i.e. more field margins, hedges and ditches. The border between two ",
      "neighbouring fields counts for both. Field blocks lack the borders ",
      "between the fields inside them and underestimate the edges. ",
      not_covered
    ), 150),
    caption = str_wrap(caption_maps, 150),
    theme = theme_maps() +
      theme(plot.subtitle = element_text(margin = margin(t = 12, b = 0)))
  ) &
  theme_legend_bottom()

save_plot(
  "plots/field_size_edges_map.png",
  plot_edges,
  width = 17, height = 12.3, dpi = 300, bg = "white"
)

# 9. field size and crop diversity -------------------------------------------

# crop diversity per hexagon from the 500 m class counts of
# src/03_crop_change_stats.R: effective number of crops = exp(Shannon index)
# of the crop classes, i.e. the number of equally common crops with the same
# diversity
class_counts <- rast(str_glue("results/croptypes_dlr_{dlr_year}_500m_class_counts.tif"))
crop_layers <- intersect(str_c("class_", crop_values), names(class_counts))

hex_crops <- exactextractr::exact_extract(
  class_counts[[crop_layers]],
  st_sf(hex = hex_stats$hex, geometry = st_geometry(hex_stats)),
  fun = "sum",
  progress = FALSE
)

hex_stats$crops_effective <- hex_crops |>
  as.matrix() |>
  apply(1, \(counts) {
    p <- counts[counts > 0] / sum(counts)
    if (length(p) == 0) NA_real_ else exp(-sum(p * log(p)))
  })

# terciles of field size and crop diversity; 3 x 3 colours, field size from
# light to orange as in the size map, diversity from light to blue
bivariate_colours <- tribble(
  ~size_class, ~crop_class, ~colour,
  1, 1, "#f3f3f3", 2, 1, "#f3e6b3", 3, 1, "#f3b300",
  1, 2, "#b4d3e1", 2, 2, "#b3b3b3", 3, 2, "#b36600",
  1, 3, "#509dc2", 2, 3, "#376387", 3, 3, "#000000"
)

terciles <- \(x) quantile(x, c(1 / 3, 2 / 3), na.rm = TRUE)
size_terciles <- terciles(hex_stats$median_ha)
crop_terciles <- terciles(hex_stats$crops_effective)

hex_bivariate <- hex_stats |>
  filter(!is.na(crops_effective)) |>
  mutate(
    size_class = findInterval(median_ha, size_terciles) + 1,
    crop_class = findInterval(crops_effective, crop_terciles) + 1
  ) |>
  left_join(bivariate_colours, by = c("size_class", "crop_class"))

map_bivariate <- hex_bivariate |>
  ggplot() +
  geom_sf(aes(fill = colour, colour = colour), linewidth = 0.1) +
  scale_fill_identity() +
  scale_colour_identity() +
  geom_borders() +
  geom_state_labels() +
  coord_sf(crs = definitions$crs_reference, datum = NA) +
  theme_maps()

legend_bivariate <- bivariate_colours |>
  ggplot(aes(size_class, crop_class, fill = colour)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  scale_fill_identity() +
  scale_x_continuous(
    breaks = c(1.5, 2.5),
    labels = label_ha_short(signif(size_terciles, 2)),
    expand = expansion(0)
  ) +
  scale_y_continuous(
    breaks = c(1.5, 2.5),
    labels = scales::label_number(accuracy = 0.1)(crop_terciles),
    expand = expansion(0)
  ) +
  labs(x = "Larger fields \u2192", y = "More crops \u2192") +
  coord_equal() +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold", size = rel(0.95)),
    plot.background = element_rect(fill = "white", colour = NA)
  )

plot_bivariate <- map_bivariate +
  # legend in the empty south-west (no data for BW and RP)
  inset_element(legend_bivariate, left = 0, bottom = 0, right = 0.26, top = 0.26) +
  plot_annotation(
    title = "Large fields, few crops?",
    subtitle = str_wrap(str_glue(
      "Median field size per hexagon against the crop diversity of the DLR ",
      "CropTypes map {dlr_year}: effective number of crops = exp(Shannon ",
      "index), the number of equally common crops with the same diversity. ",
      "Classes are terciles of the hexagons. Field blocks (SH, SN, ST, TH) ",
      "hold several fields and are larger. {not_covered}"
    ), 95),
    caption = str_c(
      str_wrap(caption_fields, 110), "\n",
      str_wrap(caption, 110)
    ),
    theme = theme_maps()
  )

save_plot(
  "plots/field_size_crop_diversity_map.png",
  plot_bivariate,
  width = 10, height = 12.5, dpi = 300, bg = "white"
)

# 10. field sizes over time ---------------------------------------------------

time_series <- definitions$field_time_series |>
  bind_rows() |>
  unnest(years) |>
  rename(year = years) |>
  mutate(
    fields = map2(collection, year, \(c, y) {
      read_parquet(str_glue("results/fields_ftw_{c}-{y}.parquet"), col_select = c(land_use, area_ha))
    })
  ) |>
  unnest(fields) |>
  filter(land_use %in% c("arable", "grassland"), area_ha >= min_ha) |>
  left_join(collections |> select(collection, state, short), by = "collection")

# median field size and the size of the field the typical hectare lies in:
# the area-weighted median, half of the farmland lies in fields at least this
# large. Per year and land use
weighted_median <- function(x, w) {
  o <- order(x)
  x[o][which(cumsum(w[o]) >= sum(w) / 2)[1]]
}

time_stats <- time_series |>
  group_by(short, state, year, land_use) |>
  summarise(
    `Median field size` = median(area_ha),
    `Field size of the typical hectare` = weighted_median(area_ha, area_ha),
    .groups = "drop"
  ) |>
  pivot_longer(c(`Median field size`, `Field size of the typical hectare`), names_to = "measure") |>
  mutate(
    measure = fct_inorder(measure),
    land_use = str_to_sentence(land_use),
    # direct labels for the land use, once: on the lines of the state with the
    # longer series in the upper panel (the states are named in the subtitle)
    line_label = str_to_lower(land_use)
  )

# two colours without the good/bad reading of red and blue; explained in the
# subtitle instead of a legend
time_colours <- c(BB = "#7B4E9E", NW = "#D08A1E")

plot_time_lines <- time_stats |>
  ggplot(aes(year, value, colour = short, linetype = land_use)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  # labels only, along the lines of the state with the longer series in the
  # upper panel
  geomtextpath::geom_textline(
    aes(label = line_label),
    data = \(d) filter(d, measure == first(measure), short == "BB"),
    text_only = TRUE,
    # no second line under the label layer (text_only alone still drew one)
    linecolour = NA,
    hjust = 0.1,
    vjust = -0.4,
    size = 3.6,
    fontface = "bold",
    # horizontal text, also where the line bends
    straight = TRUE
  ) +
  scale_colour_manual(values = time_colours, guide = "none") +
  scale_linetype_manual(values = c(Arable = "solid", Grassland = "22"), guide = "none") +
  # room above the top line for its label
  scale_y_continuous(
    labels = label_ha_short,
    limits = c(0, NA),
    expand = expansion(mult = c(0.05, 0.12))
  ) +
  scale_x_continuous(breaks = seq(2010, 2026, 2)) +
  facet_wrap(~measure, ncol = 1, scales = "free_y") +
  labs(x = NULL, y = NULL) +
  theme_charts() +
  theme(
    strip.text = element_text(face = "bold", hjust = 0, size = rel(1)),
    panel.spacing.y = unit(1.5, "lines"),
    plot.margin = margin(0, 20, 0, 0)
  )

# size distribution of the arable fields per year, both states in one column
plot_time_ridges <- time_series |>
  filter(land_use == "arable") |>
  ggplot(aes(area_ha, factor(year), fill = short, vline_colour = short)) +
  ggridges::geom_density_ridges(
    colour = "white",
    vline_width = 0.8,
    bandwidth = 0.08,
    scale = 1.6,
    rel_min_height = 0.005,
    alpha = 0.6,
    linewidth = 0.5,
    quantile_lines = TRUE,
    quantiles = 2
  ) +
  scale_fill_manual(values = time_colours, guide = "none") +
  scale_discrete_manual("vline_colour", values = time_colours, guide = "none") +
  scale_x_log10(breaks = c(0.1, 1, 10, 100), labels = label_ha_short, expand = expansion(0)) +
  scale_y_discrete(expand = expansion(add = c(0.2, 2))) +
  coord_cartesian(xlim = c(0.1, 300)) +
  labs(x = "Size of the arable fields (log scale)", y = NULL) +
  theme_charts() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 12))
  )

state_name <- \(short) collections$state[collections$short == short]
state_word <- \(short) {
  str_c(
    "<span style='color:", time_colours[short], "'>**", state_name(short),
    "**</span>"
  )
}
year_range <- \(short) {
  y <- range(time_series$year[time_series$short == short])
  str_c(y[1], "\u2013", y[2])
}

# wrapped with the plain state names, which are coloured afterwards, so the
# line lengths are right and no tag is split
time_subtitle <- str_c(
  "Fields (declared parcels) of all yearly editions published so far: ",
  state_name("BB"), " ", year_range("BB"), " and ", state_name("NW"), " ",
  year_range("NW"),
  " (no editions for 2024 and 2025). Left: median field size and the size ",
  "of the field the typical hectare lies in (area-weighted median: half of ",
  "the farmland lies in larger fields). Right: size distribution of the ",
  "arable fields per year, line = median."
) |>
  str_wrap(140) |>
  str_replace(fixed(state_name("BB")), state_word("BB")) |>
  str_replace(fixed(state_name("NW")), state_word("NW")) |>
  str_replace_all("\n", "<br>")

plot_time <- plot_time_lines + plot_time_ridges +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(
    title = "Are the fields getting larger?",
    subtitle = time_subtitle,
    caption = caption_fields,
    theme = theme_charts() +
      theme(
        plot.subtitle = ggtext::element_markdown(
          colour = "grey35", hjust = 0.5, halign = 0.5, lineheight = 1.2,
          margin = margin(t = 12, b = 16)
        )
      )
  )

save_plot(
  "plots/field_size_time_series.png",
  plot_time,
  width = 15, height = 10, dpi = 300, bg = "white"
)
