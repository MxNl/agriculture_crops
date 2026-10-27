# Shared settings, legend, borders and helpers for the DLR CropTypes scripts.
# Sourced by src/02_plot_top_crops.R, src/03_crop_change_stats.R and
# src/04_plot_crop_change.R. Run from the project root.

library(tidyverse)
library(terra)
library(sf)
library(ggh4x)

# settings ------------------------------------------------------------------

definitions <- yaml::read_yaml("definitions.yml")
years <- sort(definitions$years)

path_legend <- "cache/croptypes_dlr/LEGEND_CropTypes_V02.txt"

# no data, permanent grassland, fruit trees/woody vegetation, other use
non_crop_values <- c(0L, 83L, 100L, 111L)

caption <- str_c(
  "Data: DLR CropTypes V02 (Asam et al. 2022; Gessner et al. 2025), ",
  "CC BY 4.0. Borders: Natural Earth."
)

path_croptypes <- function(year) {
  file.path(
    "data",
    str_glue("CROPTYPES_DE_P1Y_{year}_V02_epsg{definitions$crs_reference}.tif")
  )
}

# legend: class names from the DLR legend file -------------------------------

# DLR provides no colours. These follow the JRC EUCROPMAP palette, which uses
# the USDA NASS Cropland Data Layer (CDL) colours; classes without an
# EUCROPMAP counterpart take the CDL colour or a tint of a related class
colours <- tribble(
  ~value, ~colour,
  11L,  "#A57000", # winter wheat (EUCROPMAP common wheat)
  12L,  "#E2007C", # winter barley (EUCROPMAP barley)
  13L,  "#AA007C", # winter rye (EUCROPMAP rye)
  14L,  "#D69EBC", # other winter cereals (EUCROPMAP triticale/other cereals)
  21L,  "#D9B56C", # spring wheat (CDL spring wheat)
  22L,  "#F08FC1", # spring barley (lighter barley, CDL has no split)
  23L,  "#A05989", # spring oat (EUCROPMAP oats)
  30L,  "#FFD300", # maize (EUCROPMAP maize)
  40L,  "#F2A377", # legumes (EUCROPMAP dry pulses)
  41L,  "#267000", # soybeans (EUCROPMAP soya)
  42L,  "#F9CFB4", # lupins (lighter dry pulses)
  50L,  "#DDA50A", # potatoes (EUCROPMAP potatoes)
  60L,  "#A800E2", # sugar beet (EUCROPMAP sugar beet)
  71L,  "#D1FF00", # rapeseed (EUCROPMAP rapeseed)
  72L,  "#FFFF00", # sunflowers (EUCROPMAP sunflower)
  83L,  "#E8FFBF", # permanent grassland (EUCROPMAP grasslands)
  84L,  "#E8BFFF", # forage (EUCROPMAP fodder crops)
  90L,  "#704489", # vineyard (CDL grapes)
  100L, "#93CC93", # fruit trees/woody vegetation (EUCROPMAP woodland incl. permanent crops)
  110L, "#00AF49", # hops (CDL hops)
  111L, "#BFBF7A", # other agricultural use (CDL fallow/idle cropland)
  120L, "#FF6666"  # vegetables/strawberries (CDL misc. vegetables & fruits)
)

# lines look like "11<TAB>Winterweizen<TAB>winter wheat" (one uses a space)
legend <- read_lines(path_legend, skip = 1) |>
  str_trim() |>
  str_match("^(\\d+)\\s+(.+)\\t(.+)$") |>
  as_tibble(.name_repair = ~ c("line", "value", "class_de", "class")) |>
  transmute(value = as.integer(value), class = str_to_sentence(class)) |>
  left_join(colours, by = "value") |>
  # labels on the crop colour: tone-on-tone, a darker shade on light colours
  # and a lighter shade on dark ones
  mutate(
    is_light = farver::decode_colour(
      coalesce(colour, "#FFFFFF"),
      to = "lab"
    )[, "l"] > 60,
    text_colour = if_else(is_light, "black", "white"),
    tone_colour = if_else(
      is_light,
      scales::col_darker(colour, 35),
      scales::col_lighter(colour, 35)
    )
  ) |>
  select(-is_light)

crop_values <- legend |>
  filter(!value %in% non_crop_values) |>
  pull(value)

# state borders for orientation ----------------------------------------------

states <- rnaturalearth::ne_download(
  scale = 10,
  type = "admin_1_states_provinces",
  category = "cultural",
  destdir = "cache/naturalearth",
  load = file.exists("cache/naturalearth/ne_10m_admin_1_states_provinces.gpkg"),
  returnclass = "sf"
) |>
  filter(adm0_a3 == "DEU") |>
  st_transform(definitions$crs_reference)

# geom_raster needs a linear coordinate system, so borders are drawn as paths
as_path_data <- function(x) {
  x |>
    st_cast("MULTILINESTRING") |>
    st_coordinates() |>
    as_tibble() |>
    mutate(group = str_c(L1, L2, sep = "_"))
}

borders_states <- as_path_data(st_geometry(states))
borders_country <- as_path_data(st_union(states))

# state and country borders as two thin, light geom_path layers (dark lines
# would be confused with the near-black highlighted crop pixels)
geom_borders <- function() {
  list(
    geom_path(
      aes(X, Y, group = group),
      data = borders_states,
      inherit.aes = FALSE,
      colour = "grey85",
      linewidth = 0.15
    ),
    geom_path(
      aes(X, Y, group = group),
      data = borders_country,
      inherit.aes = FALSE,
      colour = "grey70",
      linewidth = 0.25
    )
  )
}

# pixel counts per class ----------------------------------------------------

# pixel counts per class at full resolution (cached in results/)
count_classes <- function(year) {
  path_counts <- str_glue("results/croptypes_dlr_{year}_pixel_counts.csv")

  if (!file.exists(path_counts)) {
    croptypes <- rast(path_croptypes(year))

    freq(croptypes) |>
      as_tibble() |>
      transmute(value = as.integer(value), count) |>
      left_join(legend |> select(value, class), by = "value") |>
      mutate(area_km2 = count * prod(res(croptypes)) / 1e6) |>
      arrange(desc(count)) |>
      write_csv(path_counts)
  }

  read_csv(path_counts, show_col_types = FALSE) |>
    select(value, count, area_km2) |>
    left_join(legend, by = "value") |>
    mutate(year = year)
}

# crops ranked by mean area over all years (rank 1 = largest)
rank_crops_overall <- function(counts) {
  counts |>
    filter(value %in% crop_values) |>
    group_by(value, class, colour, text_colour, tone_colour) |>
    summarise(area_km2 = mean(area_km2), .groups = "drop") |>
    mutate(rank = min_rank(desc(area_km2))) |>
    arrange(rank)
}

# plot helpers ---------------------------------------------------------------

theme_maps <- function() {
  theme_void(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.4), hjust = 0.5),
      plot.subtitle = element_text(
        colour = "grey35", hjust = 0.5, margin = margin(t = 6, b = 16)
      ),
      plot.caption = element_text(
        colour = "grey50", size = rel(0.7), margin = margin(t = 14)
      ),
      strip.text = element_text(
        face = "bold", lineheight = 1.1, margin = margin(4, 4, 4, 4)
      ),
      panel.spacing.x = unit(1.5, "lines"),
      plot.margin = margin(10, 15, 10, 15),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

# facet strips filled with each crop's legend colour and tone-on-tone text;
# crops must be in facet order and have columns colour and tone_colour.
# With markdown = TRUE the text colour comes from the label's HTML (ggh4x only
# accepts plain text elements per strip), see strip_label_markdown()
strip_crops <- function(crops, markdown = FALSE) {
  # outline in the fill colour, i.e. no visible border
  background <- elem_list_rect(fill = crops$colour, colour = crops$colour)

  if (markdown) {
    strip_themed(background_x = background)
  } else {
    strip_themed(
      background_x = background,
      text_x = elem_list_text(colour = crops$tone_colour)
    )
  }
}

# two-line markdown strip label: bold title, smaller detail line, both in
# the tone colour; needs theme(strip.text = ggtext::element_markdown())
strip_label_markdown <- function(title, detail, tone_colour) {
  str_c(
    "<span style='color:", tone_colour, "'>",
    "**", str_replace_all(title, "\n", "<br>"), "**<br>",
    "<span style='font-size:9pt'>", detail, "</span></span>"
  )
}

theme_strip_markdown <- function() {
  theme(
    strip.text = ggtext::element_markdown(
      size = 11, lineheight = 1.15, halign = 0.5, margin = margin(4, 4, 4, 4)
    )
  )
}

# theme of the charts, as in the ranking and rotation charts: centred title
# and subtitle
theme_charts <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.4), hjust = 0.5),
      plot.subtitle = element_text(
        colour = "grey35", hjust = 0.5, margin = margin(t = 6, b = 16)
      ),
      plot.caption = element_text(
        colour = "grey50", size = rel(0.7), margin = margin(t = 18)
      ),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 15, 20),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

# field boundaries (Fields of the World) --------------------------------------

# harmonised field boundaries, one GeoParquet per state and edition:
# https://source.coop/ftw/harmonized-field-data
url_fields <- "https://data.source.coop/ftw/harmonized-field-data"
dir_fields_raw <- "cache/fields_ftw"

# download a collection, year = NULL for the latest edition. An existing file
# is kept, so delete it to update to a newer edition
download_fields <- function(collection, year = NULL) {
  dir.create(dir_fields_raw, showWarnings = FALSE, recursive = TRUE)
  if (is.null(year)) {
    file <- str_glue("{collection}.parquet")
    url <- str_glue("{url_fields}/{collection}/latest/{file}")
  } else {
    file <- str_glue("{collection}-{year}.parquet")
    url <- str_glue("{url_fields}/{collection}/year={year}/{file}")
  }
  path_raw <- file.path(dir_fields_raw, file)

  if (!file.exists(path_raw)) {
    status <- curl::multi_download(url, path_raw, resume = TRUE, progress = FALSE)
    if (!status$success) {
      stop("Download failed for ", url, ": ", status$error)
    }
  }

  path_raw
}

# all attributes and the geometry of a downloaded collection, in the CRS of
# the source (taken from the GeoParquet metadata)
read_fields_ftw <- function(path_raw) {
  geo <- jsonlite::fromJSON(arrow::open_dataset(path_raw)$metadata$geo)
  fields <- arrow::read_parquet(path_raw, col_select = -bbox)

  st_sf(
    # ids are not unique where a field has sub-fields (Lower Saxony), so the
    # repeats get a suffix: _1, _2, ...
    select(fields, -geometry) |>
      mutate(id = make.unique(as.character(id), sep = "_")),
    geometry = st_as_sfc(
      structure(as.list(fields$geometry), class = "WKB"),
      crs = st_crs(str_c("EPSG:", geo$columns$geometry$crs$id$code))
    )
  )
}

# land use --------------------------------------------------------------------

# each collection codes land use differently; mapped to arable, grassland,
# permanent crops or other (landscape elements, forest, non-agricultural)
land_use_groups <- c("arable", "grassland", "permanent crops", "other")

classify_land_use <- function(fields, collection) {
  land_use <- switch(
    collection,
    # HCAT codes: 3301 arable crops, 3302 grassland, 3303 permanent crops
    de_bb = , de_nds = , de_nrw = case_match(
      fields$`hcat:code` %/% 1e6,
      3301 ~ "arable",
      3302 ~ "grassland",
      3303 ~ "permanent crops"
    ),
    de_by_block = , de_he = , de_st = case_match(
      fields$`crop:name`,
      "Ackerland" ~ "arable",
      "Dauergr\u00FCnland" ~ "grassland",
      "Dauerkultur" ~ "permanent crops"
    ),
    de_sh = case_match(
      fields$hbn,
      "Ackerland" ~ "arable",
      "Dauergr\u00FCnland" ~ "grassland",
      "Dauerkulturen" ~ "permanent crops"
    ),
    # crop groups of the application; landscape elements and other areas
    # are not fields
    de_sl = case_match(
      fields$name,
      "Dauergr\u00FCnland" ~ "grassland",
      "Dauerkulturen" ~ "permanent crops",
      c(
        "Greening/Greening-Landschaftselemente", "Sonstige Fl\u00E4chen",
        "Stilllegung/ Aufforstung"
      ) ~ "other",
      .default = "arable"
    ),
    # AL arable, GL grassland, OD fruit, RF vineyards, HO hops
    de_sax = case_match(
      fields$FB_BN_KAT,
      "AL" ~ "arable",
      "GL" ~ "grassland",
      c("OD", "RF", "HO") ~ "permanent crops"
    ),
    de_th = case_match(
      fields$bnk,
      "AL" ~ "arable",
      "GL" ~ "grassland",
      "DK" ~ "permanent crops"
    ),
    # AL arable (AF: agroforestry), GL grassland (ELP: local practice, MO: moor),
    # DK permanent crops
    de_bb_block = case_when(
      str_starts(fields$hbn, "AL") ~ "arable",
      str_starts(fields$hbn, "GL") ~ "grassland",
      fields$hbn == "DK" ~ "permanent crops"
    ),
    de_nds_block = case_when(
      fields$bnk_txt == "Ackerland" ~ "arable",
      str_starts(fields$bnk_txt, "Gr\u00FCnland") ~ "grassland",
      str_starts(fields$bnk_txt, "Dauerkulturen") ~ "permanent crops"
    ),
    stop("No land use mapping for collection ", collection)
  )

  coalesce(land_use, "other")
}

# area (ha) and perimeter (m) in the source CRS if it is projected (UTM zone
# of the state), otherwise in the reference CRS; geometry in the reference CRS
measure_fields <- function(fields) {
  geometry <- st_geometry(fields)
  if (st_is_longlat(geometry)) {
    geometry <- st_transform(geometry, definitions$crs_reference)
  }
  measured <- list(
    area_ha = as.numeric(st_area(geometry)) / 1e4,
    # length of the outline (st_perimeter() would need lwgeom)
    perimeter_m = as.numeric(st_length(st_boundary(geometry))),
    geometry = st_transform(geometry, definitions$crs_reference)
  )
  measured
}

# polygons of a collection that intersect a window (bbox in the reference
# CRS), read via the bbox column without loading the whole file; with land
# use, area (ha) and geometry in the reference CRS
read_fields_window <- function(collection, window) {
  path_raw <- download_fields(collection)
  geo <- jsonlite::fromJSON(arrow::open_dataset(path_raw)$metadata$geo)
  crs_source <- st_crs(str_c("EPSG:", geo$columns$geometry$crs$id$code))
  box <- st_bbox(st_transform(st_geometry(window), crs_source))

  fields <- arrow::open_dataset(path_raw) |>
    filter(
      bbox$xmax >= box[["xmin"]], bbox$xmin <= box[["xmax"]],
      bbox$ymax >= box[["ymin"]], bbox$ymin <= box[["ymax"]]
    ) |>
    select(-bbox) |>
    collect()

  fields <- st_sf(
    select(fields, -geometry),
    geometry = st_as_sfc(
      structure(as.list(fields$geometry), class = "WKB"),
      crs = crs_source
    )
  )
  measured <- measure_fields(fields)

  st_sf(
    land_use = classify_land_use(fields, collection),
    area_ha = measured$area_ha,
    geometry = measured$geometry
  )
}

# text around the plots ------------------------------------------------------

# plot_text in definitions.yml: show title, subtitle and caption, and align
# title and subtitle centre or left. Works on ggplots and on patchworks
# (whose text is in plot_annotation())
apply_plot_text <- function(plot) {
  settings <- definitions$plot_text %||% list()
  show <- \(x) isTRUE(settings[[x]] %||% TRUE)
  hjust <- if (identical(settings$align, "left")) 0 else 0.5

  # set hjust (and halign of markdown elements) of title and subtitle,
  # keeping the class and all other properties of the elements
  align <- function(theme) {
    elements <- c("plot.title", "plot.subtitle") |>
      set_names() |>
      map(\(name) {
        element <- theme[[name]]
        if (!inherits(element, "element_text")) element <- element_text()
        element$hjust <- hjust
        if (!is.null(element$halign)) element$halign <- hjust
        element
      })
    position <- if (hjust == 0) "plot" else "panel"
    theme + do.call(theme_fn, c(elements, list(plot.title.position = position)))
  }
  theme_fn <- ggplot2::theme

  hidden <- c("title", "subtitle", "caption") |> discard(show)

  if (inherits(plot, "patchwork")) {
    annotation <- plot$patches$annotation
    for (x in hidden) annotation[x] <- list(NULL)
    annotation$theme <- align(annotation$theme %||% theme_fn())
    plot$patches$annotation <- annotation
    plot
  } else {
    if (length(hidden) > 0) {
      plot <- plot + do.call(labs, set_names(rep(list(NULL), length(hidden)), hidden))
    }
    plot$theme <- align(plot$theme %||% theme_fn())
    plot
  }
}

# ggsave() with the plot_text settings applied
save_plot <- function(filename, plot, ...) {
  ggsave(filename, apply_plot_text(plot), ...)
}
