# Map the top_n crop types with the largest area in Germany for the years in
# definitions.yml. Each map highlights one crop with gghighlight; all other
# agricultural classes are shown in their own colours but faded.
#
# Outputs:
# - plots/top<n>_crops_germany_<year>.png: one row of maps per year
# - plots/top<n>_crops_germany_<grid_years>_grid.png: grid_years as rows,
#   columns = rank
# - plots/top<ranking_top_n>_crops_germany_<first>-<last>_ranking.png: ranked
#   alluvial chart of crop rank and area over all years
#
# Facet labels are filled with the crop's legend colour so a crop can be
# followed across years and ranks.
#
# Data: DLR CropTypes V02 (10 m), reprojected by src/01_reproject_landcover.R.
# Run from the project root.

library(gghighlight)
library(ggalluvial)
library(patchwork)
library(stars)

# legend, colours, borders, counting and theme
source("src/functions.R")

# settings ------------------------------------------------------------------

top_n <- definitions$top_n
ranking_top_n <- definitions$ranking_top_n %||% top_n
ranking_label_top_n <- definitions$ranking_label_top_n %||% 5
germany_area_km2 <- definitions$germany_area_km2

labels_explained <- str_c(
  "Labels: area \u00B7 share of all agricultural land \u00B7 ",
  "share of Germany's total area."
)
grid_years <- sort(definitions$grid_years)

if (!all(grid_years %in% years)) {
  stop(
    "grid_years must be a subset of years in definitions.yml; not in years: ",
    str_flatten_comma(setdiff(grid_years, years))
  )
}

plot_res_factor <- 50 # 10 m * 50 = 500 m cells for plotting
fade_alpha <- 0.15 # opacity of the non-highlighted classes
# near-black: the faded magenta/red classes (barley, rye, vegetables) would be
# confused with a red highlight
highlight_colour <- "#1A1A1A"

# data per year --------------------------------------------------------------

# 500 m cells with the modal agricultural class (cached in cache/)
read_plot_pixels <- function(year) {
  path_plot_raster <- str_glue(
    "cache/croptypes_dlr_{year}_{10 * plot_res_factor}m_modal.tif"
  )

  # non-farmland is 0 and would win most cells, so it is ignored (-srcnodata 0)
  if (!file.exists(path_plot_raster)) {
    res_plot <- as.character(10 * plot_res_factor)
    sf::gdal_utils(
      "warp",
      source = path_croptypes(year),
      destination = path_plot_raster,
      options = c(
        "-tr", res_plot, res_plot,
        "-tap",
        "-r", "mode",
        "-srcnodata", "0",
        "-dstnodata", "0",
        "-multi",
        "-wo", "NUM_THREADS=ALL_CPUS",
        "-co", "COMPRESS=DEFLATE"
      )
    )
  }

  read_stars(path_plot_raster) |>
    as_tibble() |>
    rename(value = 3) |>
    # stars may read class rasters as factors with levels "0", "11", ...
    mutate(value = as.integer(as.character(value))) |>
    filter(!is.na(value), value != 0L) |>
    # snap cell centres to the grid so geom_raster sees even intervals
    mutate(across(c(x, y), round)) |>
    left_join(legend |> select(value, colour), by = "value")
}

counts <- map(years, count_classes) |> list_rbind()

# rank of every crop per year (rank 1 = largest area)
ranks <- counts |>
  filter(!value %in% non_crop_values) |>
  group_by(year) |>
  mutate(rank = min_rank(desc(area_km2))) |>
  ungroup()

# plot helpers ---------------------------------------------------------------

# one row of maps for one year: facet per top-n crop, the crop highlighted
plot_map_row <- function(year_plot) {
  # all agricultural land of the year: every DLR class except 0 (no data)
  agri_area_km2 <- counts |>
    filter(year == year_plot, value != 0L) |>
    pull(area_km2) |>
    sum()
  percent <- scales::label_percent(accuracy = 0.1)

  top <- ranks |>
    filter(year == year_plot, rank <= top_n) |>
    arrange(rank) |>
    mutate(
      # title: rank and crop, long names ("Other winter cereals") wrapped;
      # detail: area · share of agricultural land · share of Germany
      class_label = strip_label_markdown(
        title = str_wrap(str_c("#", rank, " ", class), width = 16),
        detail = str_c(
          scales::label_number(accuracy = 100, big.mark = ",")(area_km2),
          " km\u00B2 \u00B7 ", percent(area_km2 / agri_area_km2),
          " \u00B7 ", percent(area_km2 / germany_area_km2)
        ),
        tone_colour = tone_colour
      ),
      class_label = fct_inorder(class_label)
    )

  pixels <- read_plot_pixels(year_plot) |>
    left_join(top |> select(value, class_label), by = "value")

  plot <- pixels |>
    ggplot(aes(x, y, fill = colour)) +
    geom_raster() +
    scale_fill_identity() +
    gghighlight(
      value %in% top$value,
      use_group_by = FALSE,
      unhighlighted_params = list(fill = NULL, alpha = fade_alpha)
    )

  # gghighlight has no parameter for the highlighted layer's look, so fix its
  # fill directly: the faded background keeps the class colours, the
  # highlighted crop is drawn in one strong colour
  plot$layers[[length(plot$layers)]]$aes_params$fill <- highlight_colour

  plot +
    geom_borders() +
    # strips filled with the crop's legend colour (in facet order = rank)
    facet_wrap2(
      ~class_label,
      nrow = 1,
      strip = strip_crops(top, markdown = TRUE)
    ) +
    coord_equal() +
    theme_maps() +
    theme_strip_markdown()
}

# ranked alluvial chart: per year the crops are stacked by area (largest on
# top), block height is proportional to area and flows show rank changes.
# Every crop that was in the top ranking_top_n in at least one year is shown
plot_ranking <- function() {
  ranking <- ranks |>
    group_by(value) |>
    filter(any(rank <= ranking_top_n)) |>
    ungroup() |>
    group_by(year) |>
    mutate(share = area_km2 / sum(area_km2)) |>
    ungroup() |>
    # rank and area are also readable from position and block height, so the
    # labels stay subtle: only the top ranks and blocks large enough for the
    # text, in a darker (light blocks) or lighter (dark blocks) crop colour.
    # Unlabelled blocks get "" (NA would drop the rows and shift the stacking)
    mutate(
      label = if_else(
        rank <= ranking_label_top_n & share >= 0.08,
        str_c(
          "#", rank, "\n",
          scales::label_number(scale = 1e-3, accuracy = 0.1, suffix = "k km\u00B2")(
            area_km2
          )
        ),
        ""
      ),
      year = factor(year)
    )

  block_width <- 0.6

  # crop names beside the first and last year; ggrepel spreads the names of
  # thin blocks vertically. Labels are anchored at the outer block edge, so
  # leader lines end there instead of running into the block
  label_side <- function(year_side, side) {
    ggrepel::geom_text_repel(
      aes(
        x = stage(year, after_stat = x + side * block_width / 2),
        label = class
      ),
      data = \(d) filter(d, year == year_side),
      stat = "stratum",
      decreasing = FALSE,
      hjust = if (side < 0) 1 else 0,
      nudge_x = side * 0.12,
      direction = "y",
      size = 3.6,
      box.padding = 0.15,
      segment.colour = "grey60",
      segment.size = 0.3
    )
  }

  ranking |>
    ggplot(aes(x = year, y = area_km2, stratum = class, alluvium = class)) +
    geom_flow(
      aes(fill = colour),
      decreasing = FALSE, width = block_width, alpha = 0.55,
      curve_type = "sigmoid"
    ) +
    geom_stratum(
      aes(fill = colour),
      decreasing = FALSE, width = block_width, colour = "white",
      linewidth = 0.8
    ) +
    geom_text(
      aes(label = label, colour = tone_colour),
      stat = "stratum",
      decreasing = FALSE,
      size = 2.8,
      lineheight = 0.95
    ) +
    label_side(min(years), side = -1) +
    label_side(max(years), side = 1) +
    scale_fill_identity() +
    scale_colour_identity() +
    # room for the crop names beside the first and last year
    scale_x_discrete(expand = expansion(add = 1.9)) +
    # little space below the blocks, so the years sit close to them
    scale_y_continuous(labels = NULL, expand = expansion(mult = c(0.01, 0.02))) +
    coord_cartesian(clip = "off") +
    labs(
      title = str_glue(
        "Germany's largest crops by area, {min(years)}\u2013{max(years)}"
      ),
      subtitle = str_glue(
        "Crops are stacked by area per year, largest on top; block height is ",
        "proportional to area. All crops in the top {ranking_top_n} of at ",
        "least one year."
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
      axis.text.x = element_text(face = "bold", margin = margin(t = 2)),
      plot.margin = margin(20, 20, 15, 20),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

# single-year plots ----------------------------------------------------------

walk(years, \(year) {
  plot <- plot_map_row(year) +
    labs(
      title = str_glue("Where Germany's {top_n} largest crops grew in {year}"),
      subtitle = str_glue(
        "Each map highlights one crop in black, ranked by total area; ",
        "other agricultural land is shown faded. ",
        "{10 * plot_res_factor} m resolution.\n{labels_explained}"
      ),
      caption = caption
    )

  save_plot(
    str_glue("plots/top{top_n}_crops_germany_{year}.png"),
    plot,
    width = 16, height = 6.5, dpi = 300, bg = "white"
  )
})

# grid: grid_years as rows, ranks as columns ---------------------------------

# each row is turned into a finished gtable right away, so its pixel data and
# gghighlight's per-facet background copies are freed before the next year
# is built; building all rows at once in patchwork ran out of memory
# building and measuring grobs needs a graphics device. A temporary ragg
# device (as used by ggsave) avoids a stray Rplots.pdf and gives the markdown
# strip labels the same font metrics as the saved PNG (with pdf(NULL) the
# spaces between words got lost). Closed after the strip heights are measured
path_measure <- tempfile(fileext = ".png")
ragg::agg_png(path_measure, width = 16, height = 6.5, units = "in", res = 300)

row_grobs <- map(grid_years, \(year) {
  row <- plot_map_row(year) +
    labs(y = year) +
    theme(
      axis.title.y = element_text(
        face = "bold", size = rel(1.4), angle = 90, margin = margin(r = 10)
      )
    )
  row_grob <- ggplotGrob(row)
  rm(row)
  gc()
  row_grob
})

# pre-rendered rows are not aligned by patchwork: give all rows the height of
# the tallest strip (e.g. a wrapped "Other winter cereals"), so maps have the
# same size and position in every row
strip_row <- \(g) unique(g$layout$t[str_detect(g$layout$name, "^strip-t")])
strip_height_pt <- row_grobs |>
  map_dbl(\(g) max(grid::convertHeight(g$heights[strip_row(g)], "pt", TRUE))) |>
  max()

invisible(grDevices::dev.off())
unlink(path_measure)

map_rows <- map(row_grobs, \(g) {
  g$heights[strip_row(g)] <- unit(strip_height_pt, "pt")
  wrap_elements(full = g)
})

plot_grid <- wrap_plots(map_rows, ncol = 1) +
  plot_annotation(
    title = str_glue(
      "Germany's {top_n} largest crops in ",
      "{str_flatten_comma(grid_years, ' and ')}"
    ),
    subtitle = str_glue(
      "Rows are years, columns are ranks by total area; each map highlights ",
      "one crop in black, other agricultural land is faded. ",
      "Label colours mark the crop.\n{labels_explained}"
    ),
    caption = caption,
    theme = theme_maps()
  )

save_plot(
  str_glue(
    "plots/top{top_n}_crops_germany_{str_flatten(grid_years, '-')}_grid.png"
  ),
  plot_grid,
  width = 16, height = 1.6 + 3.4 * length(grid_years), dpi = 300, bg = "white"
)

# ranking chart: rank and area of the largest crops over all years ----------

save_plot(
  str_glue(
    "plots/top{ranking_top_n}_crops_germany_{min(years)}-{max(years)}_ranking.png"
  ),
  plot_ranking(),
  width = 4 + 1.1 * length(years), height = 8, dpi = 300, bg = "white"
)
