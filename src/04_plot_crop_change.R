# Crop change charts from the statistics of src/03_crop_change_stats.R:
#
# - plots/crop_change_<first period>_vs_<second period>.png: change of each
#   crop's share of farmland between two periods (500 m cells). Comparing
#   multi-year periods averages out crop rotation
# - plots/crop_repeat_<first>-<last>.png: on pixels that grew a crop at least
#   once, the mean number of years they grew it; separates monoculture from
#   rotation
# - plots/crop_rotation_<first>-<last>.png: which crop follows which in the
#   next year, national mean annual area of all consecutive year pairs
#
# Shows the change_top_n crops with the largest mean area over all years.
# Run from the project root.

library(ggalluvial)

# legend, colours, borders, counting and theme
source("src/functions.R")

# settings ------------------------------------------------------------------

change_top_n <- definitions$change_top_n
period_first <- seq(definitions$change_periods$first[1],
                    definitions$change_periods$first[2])
period_second <- seq(definitions$change_periods$second[1],
                     definitions$change_periods$second[2])

if (!all(c(period_first, period_second) %in% years)) {
  stop("change_periods must lie within years in definitions.yml")
}

min_farmland_share <- 0.10 # mask 500 m cells with less farmland
min_ever_share <- 0.05 # repeat map: mask cells where the crop is rare
delta_limit <- 20 # colour scale limit of the period change (percentage points)

span <- str_glue("{min(years)}-{max(years)}")
path_class_counts <- \(year) {
  str_glue("results/croptypes_dlr_{year}_500m_class_counts.tif")
}
path_repeat <- str_glue("results/croptypes_dlr_{span}_500m_repeat.tif")
path_transitions <- str_glue("results/croptypes_dlr_transitions_{span}.csv")

counts <- map(years, count_classes) |> list_rbind()

change_crops <- rank_crops_overall(counts) |>
  slice_head(n = change_top_n)

# all class counts per 500 m cell for a set of years, summed per layer
sum_layers <- function(years_sum, layers) {
  years_sum |>
    map(\(year) rast(path_class_counts(year))[[layers]]) |>
    reduce(`+`)
}

# raster stack (one layer per crop, named crop_<class value>) to long tibble,
# cells without data dropped
raster_to_long <- function(r, values_to) {
  as.data.frame(r, xy = TRUE, na.rm = FALSE) |>
    as_tibble() |>
    pivot_longer(
      -c(x, y),
      names_to = "value",
      names_prefix = "crop_",
      values_to = values_to
    ) |>
    filter(!is.na(.data[[values_to]])) |>
    mutate(value = as.integer(value))
}

# facet labels in rank order, strips in the crop colours
crop_facets <- function(labels) {
  change_crops |>
    left_join(labels, by = "value") |>
    mutate(label = fct_inorder(str_c(class, "\n", label_extra)))
}

plot_crop_maps <- function(cells, facets, fill_scale, title, subtitle) {
  cells |>
    left_join(facets |> select(value, label), by = "value") |>
    ggplot(aes(x, y, fill = fill)) +
    geom_raster() +
    fill_scale +
    geom_borders() +
    facet_wrap2(~label, nrow = 1, strip = strip_crops(facets)) +
    coord_equal() +
    labs(title = title, subtitle = subtitle, caption = caption) +
    theme_maps() +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(2.5, "cm"),
      legend.key.height = unit(0.4, "cm"),
      legend.title = element_text(vjust = 0.9, margin = margin(r = 10))
    )
}

period_label <- \(p) str_glue("{min(p)}\u2013{max(p)}")

# 1. period change of the share of farmland ---------------------------------

crop_layers <- str_c("class_", change_crops$value)
farmland_first <- sum_layers(period_first, "farmland")
farmland_second <- sum_layers(period_second, "farmland")

# percentage points; cells with little farmland in either period are masked
share_delta <- 100 * (
  sum_layers(period_second, crop_layers) / farmland_second -
    sum_layers(period_first, crop_layers) / farmland_first
)
enough_farmland <- (farmland_first / length(period_first) >=
  min_farmland_share * 50^2) &
  (farmland_second / length(period_second) >= min_farmland_share * 50^2)
share_delta <- mask(share_delta, enough_farmland, maskvalues = FALSE)
names(share_delta) <- str_c("crop_", change_crops$value)

# national change of each crop's share of farmland
national_delta <- counts |>
  filter(year %in% c(period_first, period_second)) |>
  mutate(period = if_else(year %in% period_first, "first", "second")) |>
  group_by(period) |>
  mutate(farmland = sum(count[value != 0L])) |>
  group_by(period, value) |>
  summarise(share = sum(count) / first(farmland), .groups = "drop") |>
  pivot_wider(names_from = period, values_from = share) |>
  transmute(
    value,
    label_extra = str_c(
      scales::label_number(accuracy = 0.1, style_positive = "plus")(
        100 * (second - first)
      ),
      " pp nationally"
    )
  )

plot_change <- plot_crop_maps(
  cells = raster_to_long(share_delta, "fill"),
  facets = crop_facets(national_delta),
  # binned diverging scale; the middle bin (±2 pp) is neutral
  fill_scale = scale_fill_fermenter(
    palette = "RdBu",
    direction = 1,
    breaks = c(-15, -10, -5, -2, 2, 5, 10, 15),
    limits = c(-delta_limit, delta_limit),
    oob = scales::squish,
    labels = scales::label_number(style_positive = "plus", suffix = " pp"),
    name = "Change in share of farmland\n(percentage points, pp)"
  ),
  title = str_glue(
    "Where crops gained and lost ground: {period_label(period_first)} ",
    "vs {period_label(period_second)}"
  ),
  subtitle = str_glue(
    "Change of each crop's share of farmland per 500 m cell in percentage ",
    "points (pp, e.g. 20% \u2192 25% = +5 pp); blue = more, red = less. ",
    "Cells with < {100 * min_farmland_share}% farmland are blank."
  )
)

save_plot(
  str_glue(
    "plots/crop_change_{min(period_first)}-{max(period_first)}_vs_",
    "{min(period_second)}-{max(period_second)}.png"
  ),
  plot_change,
  width = 16, height = 7, dpi = 300, bg = "white"
)

# 2. repeat cultivation ------------------------------------------------------

repeat_stats <- rast(path_repeat)
n_ever <- repeat_stats[[str_c(change_crops$value, "_n_ever")]]
sum_years <- repeat_stats[[str_c(change_crops$value, "_sum_years")]]

# mean years with the crop on pixels that grew it at least once; masked
# where those pixels are less than min_ever_share of the mean farmland
mean_farmland <- sum_layers(years, "farmland") / length(years)
mean_years <- mask(
  sum_years / n_ever,
  n_ever >= min_ever_share * mean_farmland,
  maskvalues = FALSE
)
names(mean_years) <- str_c("crop_", change_crops$value)

national_repeat <- tibble(
  value = change_crops$value,
  mean = global(sum_years, "sum", na.rm = TRUE)$sum /
    global(n_ever, "sum", na.rm = TRUE)$sum
) |>
  transmute(
    value,
    label_extra = str_c(
      "\u00D8 ", scales::label_number(accuracy = 0.1)(mean), " of ",
      length(years), " years"
    )
  )

plot_repeat <- plot_crop_maps(
  cells = raster_to_long(mean_years, "fill"),
  facets = crop_facets(national_repeat),
  # one bin per year step: 1–2, 2–3, ... years
  fill_scale = scale_fill_viridis_b(
    option = "rocket",
    direction = -1,
    limits = c(1, length(years)),
    breaks = seq(2, length(years) - 1),
    show.limits = TRUE,
    name = str_glue("Years grown (of {length(years)})")
  ),
  title = str_glue(
    "How often crops return to the same field, {min(years)}\u2013{max(years)}"
  ),
  subtitle = str_glue(
    "Mean number of years a crop was grown on the land that grew it at ",
    "least once, per 500 m cell. Dark = repeated cultivation, ",
    "light = rotation."
  )
)

save_plot(
  str_glue("plots/crop_repeat_{span}.png"),
  plot_repeat,
  width = 16, height = 7, dpi = 300, bg = "white"
)

# 3. rotation flows ----------------------------------------------------------

other_crops <- "Other crops"
other_land <- "Grassland & other"

group_class <- function(value) {
  case_when(
    value %in% change_crops$value ~
      change_crops$class[match(value, change_crops$value)],
    value %in% crop_values ~ other_crops,
    .default = other_land
  )
}

rotation <- read_csv(path_transitions, show_col_types = FALSE) |>
  filter(from %in% change_crops$value) |>
  mutate(
    from = group_class(from),
    to = group_class(to)
  ) |>
  group_by(from, to) |>
  # mean annual area over all consecutive year pairs
  summarise(
    area_km2 = sum(count) * 100 / 1e6 / (length(years) - 1),
    .groups = "drop"
  ) |>
  group_by(from) |>
  mutate(share = area_km2 / sum(area_km2)) |>
  ungroup() |>
  mutate(
    from = factor(from, levels = change_crops$class),
    to = factor(to, levels = c(change_crops$class, other_crops, other_land)),
    # "" instead of NA: rows with NA would be dropped before the flows are
    # stacked, which shifts all labels
    flow_label = if_else(
      share >= 0.1,
      str_c("\u2192 ", to, " ", scales::label_percent(accuracy = 1)(share)),
      ""
    )
  )

rotation_colours <- c(
  set_names(change_crops$colour, change_crops$class),
  set_names(c("grey75", "#E8FFBF"), c(other_crops, other_land))
)

# tone-on-tone block labels, as in the facet strips
rotation_tones <- c(
  set_names(change_crops$tone_colour, change_crops$class),
  # very light base colours need a stronger shade to stay readable
  set_names(
    scales::col_darker(c("grey75", "#E8FFBF"), c(35, 60)),
    c(other_crops, other_land)
  )
)

plot_rotation <- rotation |>
  ggplot(aes(axis1 = from, axis2 = to, y = area_km2)) +
  geom_flow(aes(fill = from), width = 1 / 4, alpha = 0.6) +
  geom_stratum(
    aes(fill = after_stat(stratum)),
    width = 1 / 4, colour = "white", linewidth = 0.8
  ) +
  geom_text(
    aes(label = after_stat(stratum), colour = after_stat(stratum)),
    stat = "stratum",
    size = 3.6,
    fontface = "bold"
  ) +
  scale_colour_manual(values = rotation_tones, guide = "none") +
  # share of each crop's area per follow-up crop, at the start of the flow
  geom_text(
    aes(label = stage(flow_label, after_stat = if_else(flow == "from", label, ""))),
    stat = "flow",
    hjust = 0,
    nudge_x = 1 / 8 + 0.02,
    size = 3.1
  ) +
  scale_fill_manual(values = rotation_colours, guide = "none") +
  scale_x_discrete(
    limits = c("from", "to"),
    labels = c("Crop in year t", "Crop in year t + 1"),
    expand = expansion(add = 0.2)
  ) +
  # little space below the blocks, so the axis labels sit close to them
  scale_y_continuous(labels = NULL, expand = expansion(mult = c(0.01, 0.02))) +
  labs(
    title = str_glue("What follows what: crop rotation, {min(years)}\u2013{max(years)}"),
    subtitle = str_glue(
      "Area of each crop by the crop grown on the same land the next year, ",
      "mean of all {length(years) - 1} year pairs.\nLabels: share of the ",
      "crop's area (\u2265 10%)."
    ),
    caption = str_remove(caption, " Borders: Natural Earth\\."),
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.4), hjust = 0.5),
    plot.subtitle = element_text(
      colour = "grey35", hjust = 0.5, margin = margin(t = 6, b = 16)
    ),
    plot.caption = element_text(
      colour = "grey50", size = rel(0.7), margin = margin(t = 18)
    ),
    panel.grid = element_blank(),
    axis.text.x = element_text(
      face = "bold", size = rel(1.1), margin = margin(t = 2)
    ),
    plot.margin = margin(20, 20, 15, 20),
    plot.background = element_rect(fill = "white", colour = NA)
  )

save_plot(
  str_glue("plots/crop_rotation_{span}.png"),
  plot_rotation,
  width = 12, height = 9, dpi = 300, bg = "white"
)
