# Field sizes from the harmonised field boundaries of Fields of the World
# (FTW, https://source.coop/ftw/harmonized-field-data), one GeoParquet file
# per state (field_collections and field_block_collections in
# definitions.yml). Per collection the latest edition is downloaded into
# cache/fields_ftw/ and reduced to one row per polygon, cached in results/:
#
# - results/fields_ftw_<collection>.parquet: id, land use (arable, grassland,
#   permanent crops, other), crop (HCAT name, only where fields carry crops),
#   area in ha, perimeter in m and centroid in the reference CRS
# - results/fields_ftw_zoom.gpkg: the polygons of the zoom windows
# - results/fields_ftw_<collection>-<year>.parquet: id, land use, area and
#   perimeter of the yearly editions in field_time_series
#
# Areas are computed from the geometry (the published areas differ between
# states, some are eligible areas). Collections whose outputs exist are
# skipped; delete them, and the download in cache/, to update to a newer
# edition. Run from the project root.

library(arrow)

# borders, settings, theme and field boundary download
source("src/functions.R")

# settings ------------------------------------------------------------------

collections <- c(
  definitions$field_collections,
  definitions$field_block_collections
) |>
  bind_rows()

zoom_collections <- map_chr(definitions$field_collections, "collection")
path_fields <- \(collection) str_glue("results/fields_ftw_{collection}.parquet")
path_zoom <- \(collection) file.path(dir_fields_raw, str_glue("zoom_{collection}.gpkg"))
path_zoom_all <- "results/fields_ftw_zoom.gpkg"

# zoom windows as squares in the reference CRS
zoom_km <- definitions$field_zoom_km
zoom_windows <- definitions$field_zoom_windows |>
  bind_rows() |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(definitions$crs_reference) |>
  mutate(geometry = st_buffer(geometry, zoom_km * 500, endCapStyle = "SQUARE"))

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

# reduce per collection --------------------------------------------------------

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

# outputs written by an older version of this script lack columns
up_to_date <- function(path, columns) {
  file.exists(path) && all(columns %in% names(open_dataset(path)))
}

reduce_collection <- function(collection) {
  if (up_to_date(path_fields(collection), "perimeter_m") &&
        file.exists(path_zoom(collection))) {
    return(invisible())
  }
  message(str_glue("{format(Sys.time(), '%H:%M')} {collection}"))

  fields <- read_fields_ftw(download_fields(collection))
  measured <- measure_fields(fields)
  geometry <- measured$geometry
  centroids <- st_coordinates(st_centroid(geometry))
  # before tibble(), where `collection` would be the new column
  land_use <- classify_land_use(fields, collection)

  reduced <- tibble(
    collection = collection,
    id = fields$id,
    land_use = land_use,
    crop = if ("hcat:name" %in% names(fields)) fields$`hcat:name` else NA_character_,
    area_ha = measured$area_ha,
    perimeter_m = measured$perimeter_m,
    x = centroids[, "X"],
    y = centroids[, "Y"]
  )
  write_parquet(reduced, path_fields(collection))

  # polygons of the zoom windows (whole polygons, clipped when plotting)
  zoom <- st_sf(reduced, geometry = geometry) |>
    filter(lengths(st_intersects(geometry, zoom_windows)) > 0)
  st_write(zoom, path_zoom(collection), delete_dsn = TRUE, quiet = TRUE)

  invisible()
}

walk(collections$collection, reduce_collection)

# yearly editions for the time series --------------------------------------

path_edition <- \(collection, year) {
  str_glue("results/fields_ftw_{collection}-{year}.parquet")
}

reduce_edition <- function(collection, year) {
  if (up_to_date(path_edition(collection, year), "perimeter_m")) {
    return(invisible())
  }
  message(str_glue("{format(Sys.time(), '%H:%M')} {collection} {year}"))

  fields <- read_fields_ftw(download_fields(collection, year))
  measured <- measure_fields(fields)
  land_use <- classify_land_use(fields, collection)

  tibble(
    collection = collection,
    year = year,
    id = fields$id,
    land_use = land_use,
    area_ha = measured$area_ha,
    perimeter_m = measured$perimeter_m
  ) |>
    write_parquet(path_edition(collection, year))
}

definitions$field_time_series |>
  bind_rows() |>
  unnest(years) |>
  pwalk(\(collection, years) reduce_edition(collection, years))

# zoom polygons of all mapped collections in one file
zoom_collections |>
  map(\(collection) st_read(path_zoom(collection), quiet = TRUE)) |>
  bind_rows() |>
  st_write(path_zoom_all, delete_dsn = TRUE, quiet = TRUE)

# checks ---------------------------------------------------------------------

# agricultural area per collection, to compare with the farmland statistics
# of the states (Destatis)
summary_fields <- collections |>
  mutate(fields = map(collection, \(c) read_parquet(path_fields(c)))) |>
  unnest(fields, names_sep = "_") |>
  filter(fields_land_use != "other") |>
  group_by(collection, state, unit) |>
  summarise(
    n = n(),
    area_kha = sum(fields_area_ha) / 1e3,
    median_ha = median(fields_area_ha),
    .groups = "drop"
  )

print(summary_fields)

if (any(summary_fields$n == 0)) {
  stop("Collections without agricultural polygons, check classify_land_use().")
}

message("Done: field tables in results/fields_ftw_*.parquet.")
