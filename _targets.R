# Pipeline of the project (https://docs.ropensci.org/targets/): downloads,
# statistics and plots, rebuilt only where settings, code or inputs changed.
# The functions of the steps are in R/, the settings in definitions.yml.
#
# Run from the project root:
#   targets::tar_make()                          # build everything outdated
#   targets::tar_make(names = plots_field_sizes) # one step and what it needs
#   targets::tar_outdated()                      # what would be rebuilt
#   targets::tar_visnetwork()                    # dependency graph
#   targets::tar_read(field_summary)             # a result, e.g. farmland per state
#
# Steps:
# - DLR CropTypes maps per year (download, reprojection, pixel counts), the
#   500 m change statistics (about an hour) and their plots
# - Fields of the World field boundaries per state (download, field tables,
#   crops per polygon, validation) and the field size plots
# - specialty crop regions from the Thuenen crop type map
#
# Files are written to cache/ (downloads), data/ (reprojected maps), results/
# and plots/ as before; targets keeps its records in _targets/. Downloads,
# the reprojected maps and resampled Thuenen map are kept once they exist
# (delete them to download again); everything else is rebuilt by targets
# when it is outdated.

library(targets)

# loaded in this order for every step (later packages mask earlier ones,
# e.g. terra's area() and mask()), and here, as the files in R/ use some of
# them when they are sourced
packages <- c(
  "arrow", "exactextractr", "gghighlight", "ggalluvial", "patchwork", "stars",
  "tidyverse", "terra", "sf", "ggh4x"
)
for (package in packages) {
  suppressPackageStartupMessages(library(package, character.only = TRUE))
}
# large files (maps, downloads) are only hashed again when their time stamp
# changes (needs targets >= 1.8.0)
tar_option_set(packages = packages, trust_timestamps = TRUE)

tar_source("R")

list(
  # settings and shared inputs ------------------------------------------------
  tar_target(definitions_file, "definitions.yml", format = "file"),
  tar_target(definitions, read_definitions(definitions_file)),
  tar_target(legend_file, download_croptypes_legend(), format = "file"),
  tar_target(states, read_states(definitions$crs_reference)),

  # settings of the long-running steps, so they rerun only when these change
  tar_target(settings_change, pick_settings(definitions, c("years", "crs_reference", "change_top_n"))),
  tar_target(settings_fields, pick_settings(definitions, "crs_reference")),
  tar_target(settings_crops, pick_settings(definitions, c("years", "crs_reference"))),
  tar_target(
    settings_validation,
    pick_settings(
      definitions,
      c("years", "crs_reference", "field_crop_validation", "field_min_ha", "field_crop_min_share")
    )
  ),

  # DLR CropTypes maps (R/croptypes.R, R/crop_change_stats.R) -----------------
  tar_target(year, sort(definitions$years)),
  tar_target(
    croptypes,
    prepare_croptypes(year, definitions$crs_reference),
    pattern = map(year),
    format = "file"
  ),
  tar_target(
    pixel_counts,
    write_pixel_counts(croptypes, year, legend_file),
    pattern = map(croptypes, year),
    format = "file"
  ),
  tar_target(
    change_stats,
    compute_crop_change_stats(croptypes, pixel_counts, settings_change, legend_file),
    format = "file"
  ),

  # Fields of the World (R/field_tables.R, R/field_crops.R) -------------------
  tar_target(field_collection, field_collection_names(definitions)),
  tar_target(
    field_download,
    download_fields(field_collection),
    pattern = map(field_collection),
    format = "file"
  ),
  tar_target(
    field_table,
    reduce_collection(field_download, field_collection, settings_fields),
    pattern = map(field_download, field_collection),
    format = "file"
  ),
  tar_target(field_summary, summarise_field_tables(field_table)),
  tar_target(field_edition, field_editions(definitions)),
  tar_target(
    field_edition_download,
    download_fields(field_edition$collection, field_edition$year),
    pattern = map(field_edition),
    format = "file"
  ),
  tar_target(
    field_edition_table,
    reduce_edition(
      field_edition_download, field_edition$collection, field_edition$year, settings_fields
    ),
    pattern = map(field_edition_download, field_edition),
    format = "file"
  ),
  tar_target(field_crop_collection, field_crop_collection_names(definitions)),
  tar_target(
    field_crops,
    crops_collection(field_crop_collection, field_download, croptypes, settings_crops),
    pattern = map(field_crop_collection),
    format = "file"
  ),
  tar_target(
    field_crop_validation,
    validate_field_crops(field_edition_download, croptypes, settings_validation, legend_file),
    format = "file"
  ),

  # Thuenen crop type map --------------------------------------------------------
  tar_target(thuenen_legend_file, definitions$thuenen_legend, format = "file"),

  # plots (R/plot_*.R) -------------------------------------------------------------
  tar_target(
    plots_top_crops,
    plot_top_crops(croptypes, pixel_counts, definitions, legend_file, states),
    format = "file"
  ),
  tar_target(
    plots_crop_change,
    plot_crop_change(change_stats, pixel_counts, definitions, legend_file, states),
    format = "file"
  ),
  tar_target(
    plots_field_sizes,
    plot_field_sizes(
      field_table, field_edition_table, field_crops, field_crop_validation, change_stats,
      definitions, legend_file, states
    ),
    format = "file"
  ),
  tar_target(
    plots_specialty_regions,
    plot_specialty_regions(thuenen_legend_file, definitions, legend_file, states),
    format = "file"
  )
)
