# Crop per field polygon from the DLR CropTypes maps, for all states,
# including those whose polygons carry no crop. Each polygon gets the DLR
# class covering most of it and the share of the polygon that class covers;
# pixels count with the fraction that lies inside the polygon (exactextractr).
# Field blocks and reference parcels can hold several crops, so the share
# shows whether a polygon is dominated by one crop.
#
# - results/fields_ftw_<collection>_dlr<year>.parquet: id, dlr_class,
#   dlr_share, for the latest edition of each collection in
#   field_collections and the latest DLR year in years
# - results/fields_ftw_dlr_validation.csv: for the editions in
#   field_crop_validation, the area of each declared crop and the share of it
#   where the DLR map has the same crop (same year)
#
# The field polygons of the latest editions are newer than the latest DLR
# map, so this assumes the polygons change little from one year to the next.
# Needs the reprojected DLR rasters of src/01_reproject_landcover.R and the
# downloads of src/05_field_sizes_stats.R. Outputs that exist are skipped.
# Run from the project root.

library(arrow)
library(exactextractr)

# legend, borders, settings and field boundary download
source("src/functions.R")

# settings ------------------------------------------------------------------

collections <- bind_rows(definitions$field_collections)
validation <- bind_rows(definitions$field_crop_validation)
dlr_year <- max(years)

path_crops <- \(collection, year) {
  str_glue("results/fields_ftw_{collection}_dlr{year}.parquet")
}
path_validation <- "results/fields_ftw_dlr_validation.csv"

if (!all(validation$year %in% years)) {
  stop("field_crop_validation years must lie within years in definitions.yml")
}

# declared crops (HCAT names) and the DLR class they correspond to; crops
# without a clear DLR class are not compared
hcat_to_dlr <- tribble(
  ~value, ~hcat,
  11L,  c("winter_common_soft_wheat", "winter_durum_hard_wheat"),
  12L,  "winter_barley",
  13L,  "winter_rye",
  14L,  c("winter_triticale", "winter_spelt", "winter_oats", "winter_meslin"),
  21L,  c("spring_common_soft_wheat", "spring_durum_hard_wheat"),
  22L,  "spring_barley",
  23L,  "spring_oats",
  30L,  c("green_silo_maize", "grain_maize_corn_popcorn"),
  40L,  c("peas", "beans", "legumes_dried_pulses_protein_crops"),
  41L,  "soy_soybeans",
  42L,  "sweet_lupins",
  50L,  "potatoes",
  60L,  "sugar_beet",
  71L,  c("winter_rapeseed_rape", "summer_rapeseed_rape"),
  72L,  "sunflower",
  83L,  "pasture_meadow_grassland_grass",
  84L,  c("alfalfa_lucerne", "clover", "temporary_grass"),
  90L,  "vineyards_wine_vine_rebland_grapes",
  100L, "orchards_fruits",
  110L, "hops",
  120L, c(
    "alliums", "carrots_daucus", "asparagus", "aspargus",
    "brassica_oleracea_cabbage", "strawberries", "pumpkin_squash_gourd",
    "salads_lettuce_leaf_vegetables", "spinach", "fresh_vegetables"
  )
) |>
  unnest(hcat)

# majority class per polygon ------------------------------------------------

# fields: sf in any CRS. Returns id, the class covering most of the polygon
# (class 0, no farmland, never wins) and its share of the polygon
majority_crop <- function(fields, year) {
  croptypes <- rast(path_croptypes(year))
  fields <- st_transform(fields, crs(croptypes))

  # share of each class per polygon, columns frac_<class>
  frac <- exact_extract(croptypes, fields, fun = "frac", progress = FALSE)

  frac_classes <- frac |> select(-any_of("frac_0")) |> as.matrix()
  classes <- as.integer(str_remove(colnames(frac_classes), "frac_"))
  best <- max.col(frac_classes, ties.method = "first")

  tibble(
    id = fields$id,
    dlr_class = classes[best],
    dlr_share = frac_classes[cbind(seq_along(best), best)]
  ) |>
    # polygons without any farmland pixel
    mutate(dlr_class = if_else(dlr_share > 0, dlr_class, NA_integer_))
}

crops_collection <- function(collection) {
  path_out <- path_crops(collection, dlr_year)
  if (file.exists(path_out)) {
    return(invisible())
  }
  message(str_glue("{format(Sys.time(), '%H:%M')} {collection}"))

  download_fields(collection) |>
    read_fields_ftw() |>
    select(id) |>
    majority_crop(dlr_year) |>
    write_parquet(path_out)
}

walk(collections$collection, crops_collection)

# validation against the declared crops -------------------------------------

validate_edition <- function(collection, year) {
  fields <- download_fields(collection, year) |>
    read_fields_ftw() |>
    select(id, hcat = `hcat:name`) |>
    mutate(area_ha = as.numeric(st_area(geometry)) / 1e4) |>
    filter(area_ha >= definitions$field_min_ha)

  fields |>
    st_drop_geometry() |>
    inner_join(hcat_to_dlr, by = "hcat") |>
    left_join(majority_crop(fields, year), by = "id") |>
    mutate(
      collection = .env$collection,
      year = .env$year,
      same = coalesce(dlr_class == value, FALSE),
      single = coalesce(dlr_share >= definitions$field_crop_min_share, FALSE)
    ) |>
    group_by(collection, year, value) |>
    summarise(
      n = n(),
      # declared area where the DLR majority class is the declared crop, of
      # all fields and of the single-crop fields (as used in the plots)
      agreement = sum(area_ha[same]) / sum(area_ha),
      agreement_single = sum(area_ha[same & single]) / sum(area_ha[single]),
      area_ha = sum(area_ha),
      .groups = "drop"
    ) |>
    left_join(legend |> select(value, class), by = "value")
}

if (!file.exists(path_validation)) {
  message(str_glue("{format(Sys.time(), '%H:%M')} validation"))
  validation |>
    pmap(\(collection, year) validate_edition(collection, year)) |>
    list_rbind() |>
    write_csv(path_validation)
}

# checks ---------------------------------------------------------------------

agreement <- read_csv(path_validation, show_col_types = FALSE) |>
  group_by(collection, year) |>
  summarise(
    agreement = weighted.mean(agreement, area_ha),
    agreement_single = weighted.mean(agreement_single, area_ha, na.rm = TRUE),
    .groups = "drop"
  )

print(agreement)
message("Done: DLR crops in results/fields_ftw_*_dlr*.parquet.")
