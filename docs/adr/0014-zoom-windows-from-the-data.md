# 14. Example windows chosen from the data

- Status: accepted
- Date: 2026-10-26
- Code: `R/plot_field_sizes.R`; settings `field_zoom_km`,
  `field_zoom_percentiles`, `field_zoom_min_farmland`

## Context

Maps of medians are abstract; example windows with the real field polygons
make field sizes tangible. Hand-picked windows would be arbitrary.

## Decision

- Per unit (fields, reference parcels, field blocks), the hexagons whose
  median field size is closest (on a log scale) to the 10th, 50th and 90th
  percentile of the hexagon medians of that unit
  (`field_zoom_percentiles`).
- Only hexagons with at least 60 % arable land (`field_zoom_min_farmland`)
  and fields of one state.
- A window of 4 × 4 km (`field_zoom_km`) around the hexagon centre; the
  median of the fields whose centroid lies in the window is given.

## Consequences

- The windows are reproducible and represent small, typical and large
  fields of each unit, not the extremes.
