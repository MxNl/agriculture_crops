# Crop change statistics from the 10 m DLR CropTypes rasters of all years in
# definitions.yml, computed in one block-wise pass (one 500 m row of cells at a
# time, all years read in parallel) and cached in results/:
#
# - results/croptypes_dlr_<year>_500m_class_counts.tif: per 500 m cell the
#   number of 10 m pixels of each class (layers class_<code>) and of farmland
#   (layer farmland = pixels != 0)
# - results/croptypes_dlr_<first>-<last>_500m_repeat.tif: per 500 m cell and
#   change crop the number of pixels that grew the crop at least once
#   (<code>_n_ever) and the summed number of years with the crop
#   (<code>_sum_years)
# - results/croptypes_dlr_transitions_<first>-<last>.csv: national pixel
#   counts of crop transitions between consecutive years (year_from, from, to)
#
# The pass reads ~7 billion cells per year and takes about an hour; it is
# skipped when all outputs exist. Run from the project root, preferably with
# Rscript in a terminal.

source("src/functions.R")

# settings ------------------------------------------------------------------

change_top_n <- definitions$change_top_n
block <- 50 # 10 m * 50 = 500 m cells, same grid as the plotting rasters

span <- str_glue("{min(years)}-{max(years)}")
path_class_counts <- \(year) {
  str_glue("results/croptypes_dlr_{year}_500m_class_counts.tif")
}
path_repeat <- str_glue("results/croptypes_dlr_{span}_500m_repeat.tif")
path_transitions <- str_glue("results/croptypes_dlr_transitions_{span}.csv")

counts <- map(years, count_classes) |> list_rbind()

change_crops <- rank_crops_overall(counts) |>
  slice_head(n = change_top_n) |>
  pull(value)

class_codes <- setdiff(legend$value, 0L)
repeat_layers <- c(
  str_c(change_crops, "_n_ever"),
  str_c(change_crops, "_sum_years")
)

outputs <- c(map_chr(years, path_class_counts), path_repeat, path_transitions)
up_to_date <- all(file.exists(outputs)) &&
  all(repeat_layers %in% names(rast(path_repeat)))

# block-wise pass ------------------------------------------------------------

compute_change_stats <- function() {
  sources <- map(years, \(year) rast(path_croptypes(year)))
  src <- sources[[1]]
  # all years must share one 10 m grid (same warp settings in script 01)
  walk(sources[-1], \(r) compareGeom(r, src))

  # 500 m grid snapped to multiples of 500 m, like gdalwarp -tap
  res_out <- block * res(src)[1]
  e <- ext(src)
  template <- rast(
    ext(
      floor(xmin(e) / res_out) * res_out,
      ceiling(xmax(e) / res_out) * res_out,
      floor(ymin(e) / res_out) * res_out,
      ceiling(ymax(e) / res_out) * res_out
    ),
    resolution = res_out,
    crs = crs(src)
  )
  n_blocks <- ncol(template)

  # 500 m column of every 10 m column, and 10 m rows of every 500 m row
  col_offset <- round((xmin(e) - xmin(template)) / res(src)[1])
  col_block <- (seq_len(ncol(src)) - 1 + col_offset) %/% block + 1
  row_offset <- round((ymax(template) - ymax(e)) / res(src)[2])
  bands <- tibble(row_out = seq_len(nrow(template))) |>
    mutate(
      first = pmax((row_out - 1) * block - row_offset + 1, 1),
      last = pmin(row_out * block - row_offset, nrow(src)),
      n = last - first + 1
    )

  # writers: rows are written as they are computed, memory stays at one band
  gdal_options <- c("COMPRESS=DEFLATE", "TILED=YES")
  writers_counts <- map(years, \(year) {
    out <- rast(
      template,
      nlyrs = length(class_codes) + 1,
      names = c("farmland", str_c("class_", class_codes))
    )
    writeStart(
      out, path_class_counts(year),
      datatype = "INT2U", overwrite = TRUE, gdal = gdal_options
    )
    out
  })
  writer_repeat <- rast(
    template,
    nlyrs = length(repeat_layers),
    names = repeat_layers
  )
  writeStart(
    writer_repeat, path_repeat,
    datatype = "INT2U", overwrite = TRUE, gdal = gdal_options
  )

  # lookup class value -> bit of the change crop (0 for all other classes)
  crop_bits <- integer(256)
  crop_bits[change_crops + 1] <- 2L^(seq_along(change_crops) - 1)

  walk(sources, readStart)

  n_pairs <- length(years) - 1
  transitions <- reduce(
    seq_len(nrow(template)),
    \(acc, i) {
      band <- bands[i, ]
      vals <- map(sources, \(r) {
        coalesce(readValues(r, row = band$first, nrows = band$n), 0)
      })
      cb <- rep(col_block, times = band$n)

      # class counts per 500 m cell: one tabulate per year for all classes,
      # column v + 1 holds class v
      tables <- map(vals, \(v) {
        matrix(tabulate(cb + n_blocks * v, n_blocks * 256), nrow = n_blocks)
      })
      walk2(tables, writers_counts, \(m, w) {
        writeValues(
          w,
          cbind(rowSums(m[, -1]), m[, class_codes + 1]),
          start = i,
          nrows = 1
        )
      })

      # repeat cultivation: pixels that grew the crop at least once, and the
      # summed years with the crop (= sum of the yearly class counts).
      # Each change crop gets one bit; the per-pixel bitmask over all years
      # is tabulated once per 500 m cell
      mask <- vals |>
        map(\(v) crop_bits[v + 1L]) |>
        reduce(bitwOr)
      mask_table <- matrix(
        tabulate(cb + n_blocks * mask, n_blocks * 2^length(change_crops)),
        nrow = n_blocks
      )
      n_ever <- map(seq_along(change_crops), \(j) {
        has_crop <- bitwAnd(seq_len(ncol(mask_table)) - 1L, 2L^(j - 1)) > 0
        rowSums(mask_table[, has_crop, drop = FALSE])
      })
      sum_years <- map(change_crops, \(k) {
        tables |> map(\(m) m[, k + 1]) |> reduce(`+`)
      })
      writeValues(
        writer_repeat,
        do.call(cbind, c(n_ever, sum_years)),
        start = i,
        nrows = 1
      )

      # transitions between consecutive years, coded as from * 256 + to
      pair_tables <- map(seq_len(n_pairs), \(j) {
        tabulate(vals[[j]] * 256 + vals[[j + 1]] + 1, 256^2)
      })

      if (i %% 100 == 0) {
        message(str_glue("{format(Sys.time(), '%H:%M')} row {i}/{nrow(template)}"))
      }

      acc + do.call(cbind, pair_tables)
    },
    .init = matrix(0, nrow = 256^2, ncol = n_pairs)
  )

  walk(sources, readStop)
  walk(writers_counts, writeStop)
  writeStop(writer_repeat)

  tibble(
    year_from = rep(head(years, -1), each = 256^2),
    code = rep(seq_len(256^2) - 1, times = n_pairs),
    count = as.vector(transitions)
  ) |>
    filter(count > 0) |>
    transmute(year_from, from = code %/% 256, to = code %% 256, count) |>
    write_csv(path_transitions)
}

if (up_to_date) {
  message("Change statistics are up to date, nothing to compute.")
} else {
  compute_change_stats()
}

# checks ---------------------------------------------------------------------

# the 500 m class counts must add up to the full-resolution pixel counts
check_counts <- years |>
  map(\(year) {
    totals <- global(rast(path_class_counts(year)), "sum")
    tibble(
      year = year,
      layer = rownames(totals),
      count_500m = totals$sum
    )
  }) |>
  list_rbind() |>
  filter(layer != "farmland") |>
  mutate(value = as.integer(str_remove(layer, "class_"))) |>
  left_join(counts |> select(year, value, count), by = c("year", "value")) |>
  mutate(count = coalesce(count, 0))

mismatch <- check_counts |> filter(count_500m != count)

if (nrow(mismatch) > 0) {
  print(mismatch)
  stop("500 m class counts differ from the full-resolution pixel counts.")
}

message("Checks passed: 500 m class counts match the pixel counts.")
