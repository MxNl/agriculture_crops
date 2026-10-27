# Field sizes from the harmonised field boundaries of Fields of the World
# (FTW, https://source.coop/ftw/harmonized-field-data), one GeoParquet file
# per state (field_collections and field_block_collections in
# definitions.yml). Per collection the latest edition is downloaded into
# cache/fields_ftw/ and reduced to one row per polygon, cached in results/:
#
# - results/fields_ftw_<collection>.parquet: id, land use (arable, grassland,
#   permanent crops, other), crop (HCAT name, only where fields carry crops),
#   area in ha, perimeter in m and centroid in the reference CRS
# - results/fields_ftw_<collection>-<year>.parquet: id, land use, area and
#   perimeter of the yearly editions in field_time_series
#
# Areas are computed from the geometry (the published areas differ between
# states, some are eligible areas). Collections whose outputs exist are
# skipped; delete them, and the download in cache/, to update to a newer
# edition. Run from the project root.

library(arrow)

# borders, settings, theme, field boundary download and land use
source("src/functions.R")

# settings ------------------------------------------------------------------

collections <- c(
  definitions$field_collections,
  definitions$field_block_collections
) |>
  bind_rows()

path_fields <- \(collection) str_glue("results/fields_ftw_{collection}.parquet")

# reduce per collection --------------------------------------------------------

# outputs written by an older version of this script lack columns
up_to_date <- function(path, columns) {
  file.exists(path) && all(columns %in% names(open_dataset(path)))
}

reduce_collection <- function(collection) {
  if (up_to_date(path_fields(collection), "perimeter_m")) {
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
