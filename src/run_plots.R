# Run all plot scripts, e.g. after changing plot_text or other settings in
# definitions.yml. Each script runs in its own R session (as with Rscript),
# so they do not affect each other; a failing script is reported and the
# others still run.
#
# The plot scripts use the cached results of the statistics scripts
# (01_reproject_landcover.R, 03_crop_change_stats.R, 05_field_sizes_stats.R,
# 06_field_crops_dlr.R), which are not run here; run those first when their
# settings or the data changed.
#
# Run from the project root:
#   Rscript src/run_plots.R            # all plot scripts
#   Rscript src/run_plots.R 07 08      # only scripts starting with 07 or 08
# or source("src/run_plots.R") in RStudio.

plot_scripts <- c(
  "src/02_plot_top_crops.R",
  "src/04_plot_crop_change.R",
  "src/07_plot_field_sizes.R",
  "src/08_plot_specialty_regions.R"
)

# optional selection by script number from the command line
selection <- commandArgs(trailingOnly = TRUE)
if (length(selection) > 0) {
  keep <- vapply(
    basename(plot_scripts),
    \(script) any(startsWith(script, selection)),
    logical(1)
  )
  plot_scripts <- plot_scripts[keep]
}

rscript <- file.path(R.home("bin"), "Rscript")

results <- lapply(plot_scripts, \(script) {
  message(format(Sys.time(), "%H:%M"), " ", script)
  start <- Sys.time()
  status <- system2(rscript, script)
  data.frame(
    script = script,
    status = if (status == 0) "ok" else "FAILED",
    minutes = round(as.numeric(difftime(Sys.time(), start, units = "mins")), 1)
  )
})
results <- do.call(rbind, results)

print(results, row.names = FALSE)
if (any(results$status != "ok")) {
  stop("Failed: ", paste(results$script[results$status != "ok"], collapse = ", "))
}
message("All plots updated in plots/.")
