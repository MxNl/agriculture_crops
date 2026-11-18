# Field sizes from the harmonised field boundaries of Fields of the World
# (FTW, https://source.coop/ftw/harmonized-field-data), one GeoParquet file
# per state (field_collections and field_block_collections in
# definitions.yml). Per collection the latest edition is downloaded into
# cache/fields_ftw/ (target field_download) and reduced to one row per
# polygon in results/ (target field_table):
#
# - results/fields_ftw_<collection>.parquet: id, land use (arable, grassland,
#   permanent crops, other), crop (HCAT name, only where fields carry crops),
#   area in ha, perimeter in m and centroid in the reference CRS
# - results/fields_ftw_<collection>-<year>.parquet: id, land use, area and
#   perimeter of the yearly editions in field_time_series (targets
#   field_edition_download and field_edition_table)
#
# Areas are computed from the geometry (the published areas differ between
# states, some are eligible areas). A download is kept once it exists; delete
# it in cache/ to update to a newer edition.

path_fields <- \(collection) str_glue("results/fields_ftw_{collection}.parquet")
path_edition <- \(collection, year) {
  str_glue("results/fields_ftw_{collection}-{year}.parquet")
}

# collections of the field size maps and the field blocks for comparison
field_collection_names <- function(definitions) {
  c(definitions$field_collections, definitions$field_block_collections) |>
    bind_rows() |>
    pull(collection)
}

# yearly editions for the time series: one row per collection and year
field_editions <- function(definitions) {
  definitions$field_time_series |>
    bind_rows() |>
    unnest(years) |>
    rename(year = years)
}

# latest edition reduced to one row per polygon; returns the path
reduce_collection <- function(path_raw, collection, definitions) {
  use_context(definitions)
  message(str_glue("{format(Sys.time(), '%H:%M')} {collection}"))

  fields <- read_fields_ftw(path_raw)
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

  # check: classify_land_use() must know the collection's land use coding
  if (!any(reduced$land_use != "other")) {
    stop("No agricultural polygons in ", collection, ", check classify_land_use().")
  }

  write_parquet(reduced, path_fields(collection))
  path_fields(collection)
}

# a yearly edition reduced to one row per polygon; returns the path
reduce_edition <- function(path_raw, collection, year, definitions) {
  use_context(definitions)
  message(str_glue("{format(Sys.time(), '%H:%M')} {collection} {year}"))

  fields <- read_fields_ftw(path_raw)
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

  path_edition(collection, year)
}

# agricultural area per collection, to compare with the farmland statistics
# of the states (Destatis); tar_read(field_summary)
summarise_field_tables <- function(field_tables) {
  field_tables |>
    map(read_parquet) |>
    list_rbind() |>
    filter(land_use != "other") |>
    group_by(collection) |>
    summarise(
      n = n(),
      area_kha = sum(area_ha) / 1e3,
      median_ha = median(area_ha),
      .groups = "drop"
    )
}
