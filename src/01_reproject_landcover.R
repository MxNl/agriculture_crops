# Download the DLR CropTypes maps of Germany (V02, 10 m) for the years in
# definitions.yml into cache/ and reproject them to the reference CRS in data/.
#
# Source: https://download.geoservice.dlr.de/CROPTYPES/files/
# Run from the project root.

library(tidyverse)
library(terra)

# settings ------------------------------------------------------------------

definitions <- yaml::read_yaml("definitions.yml")
crs_target <- paste0("EPSG:", definitions$crs_reference)

url_base <- "https://download.geoservice.dlr.de/CROPTYPES/files"
dir_raw <- "cache/croptypes_dlr"
dir.create(dir_raw, showWarnings = FALSE, recursive = TRUE)

# legend --------------------------------------------------------------------

path_legend <- file.path(dir_raw, "LEGEND_CropTypes_V02.txt")
if (!file.exists(path_legend)) {
  download.file(
    file.path(url_base, "LEGEND_CropTypes_V02.txt"),
    path_legend,
    mode = "wb"
  )
}

# download and reproject per year -------------------------------------------

download_croptypes <- function(year) {
  name <- str_glue("CROPTYPES_DE_P1Y_{year}_V02")
  path_raw <- file.path(dir_raw, str_glue("{name}.tif"))

  # resume = TRUE continues interrupted downloads and is a no-op once complete
  status <- curl::multi_download(
    str_glue("{url_base}/{name}/{name}.tif"),
    path_raw,
    resume = TRUE,
    progress = FALSE
  )
  if (!status$success) {
    stop("Download failed for ", year, ": ", status$error)
  }

  path_raw
}

reproject_croptypes <- function(path_raw) {
  path_out <- file.path(
    "data",
    str_glue(
      "{tools::file_path_sans_ext(basename(path_raw))}",
      "_epsg{definitions$crs_reference}.tif"
    )
  )

  if (!file.exists(path_out)) {
    # gdalwarp streams block by block; terra::project() would need ~56 GB RAM
    # for the 7 billion cells. Categorical classes -> nearest neighbour,
    # snapped to a clean 10 m grid
    sf::gdal_utils(
      "warp",
      source = path_raw,
      destination = path_out,
      options = c(
        "-t_srs", crs_target,
        "-r", "near",
        "-tr", "10", "10",
        "-tap",
        "-ot", "Byte",
        "-multi",
        "-wo", "NUM_THREADS=ALL_CPUS",
        "-wm", "1024",
        "-co", "COMPRESS=DEFLATE",
        "-co", "TILED=YES",
        "-co", "BIGTIFF=YES"
      )
    )
  }

  path_out
}

definitions$years |>
  map_chr(download_croptypes) |>
  map_chr(reproject_croptypes) |>
  walk(\(path) print(rast(path)))
