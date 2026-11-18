# DLR CropTypes maps of Germany (V02, 10 m): the legend and the yearly maps,
# downloaded into cache/croptypes_dlr/ and reprojected to the reference CRS
# in data/ (targets legend_file and croptypes, one branch per year).
#
# Source: https://download.geoservice.dlr.de/CROPTYPES/files/

url_croptypes <- "https://download.geoservice.dlr.de/CROPTYPES/files"
dir_croptypes_raw <- "cache/croptypes_dlr"

# the legend file (path_legend); kept once downloaded
download_croptypes_legend <- function() {
  dir.create(dir_croptypes_raw, showWarnings = FALSE, recursive = TRUE)
  if (!file.exists(path_legend)) {
    download.file(
      file.path(url_croptypes, "LEGEND_CropTypes_V02.txt"),
      path_legend,
      mode = "wb"
    )
  }
  path_legend
}

download_croptypes <- function(year) {
  dir.create(dir_croptypes_raw, showWarnings = FALSE, recursive = TRUE)
  name <- str_glue("CROPTYPES_DE_P1Y_{year}_V02")
  path_raw <- file.path(dir_croptypes_raw, str_glue("{name}.tif"))

  # resume = TRUE continues interrupted downloads and is a no-op once complete
  status <- curl::multi_download(
    str_glue("{url_croptypes}/{name}/{name}.tif"),
    path_raw,
    resume = TRUE,
    progress = FALSE
  )
  if (!status$success) {
    stop("Download failed for ", year, ": ", status$error)
  }

  path_raw
}

# the reprojected map of a year in data/ (as path_croptypes()). The file
# name holds the year and the CRS, so an existing file is kept (delete it to
# download and reproject again)
prepare_croptypes <- function(year, crs_reference) {
  path_out <- file.path(
    "data",
    str_glue("CROPTYPES_DE_P1Y_{year}_V02_epsg{crs_reference}.tif")
  )

  # the download only when the reprojected map is missing
  if (!file.exists(path_out)) {
    path_raw <- download_croptypes(year)
    dir.create("data", showWarnings = FALSE)
    # gdalwarp streams block by block; terra::project() would need ~56 GB RAM
    # for the 7 billion cells. Categorical classes -> nearest neighbour,
    # snapped to a clean 10 m grid
    sf::gdal_utils(
      "warp",
      source = path_raw,
      destination = path_out,
      options = c(
        "-t_srs", str_c("EPSG:", crs_reference),
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
