# 2. What counts as a crop, and ranking crops by area

- Status: accepted
- Date: 2026-10-17
- Code: `R/functions.R` (`non_crop_values`, `write_pixel_counts()`,
  `rank_crops_overall()`), `R/plot_top_crops.R`; settings `top_n`,
  `ranking_top_n`, `germany_area_km2`

## Context

The DLR legend contains, besides field crops, permanent grassland, fruit
trees / woody vegetation and "other agricultural use". Rankings of "the
largest crops" would be dominated by grassland if all classes counted.

## Decision

- Crops are all DLR classes except 0 (no data), 83 (permanent grassland),
  100 (fruit trees / woody vegetation) and 111 (other agricultural use)
  (`non_crop_values`).
- Crop areas come from the pixel counts of the full-resolution (10 m) maps
  per year (pixel count × 100 m²), not from aggregated cells.
- Crops are ranked per year by area; the top `top_n` (6) are mapped, the
  ranking chart follows every crop that was in the top `ranking_top_n` (10)
  in at least one year.
- Shares are given relative to all agricultural land (all classes except 0)
  and to the area of Germany (`germany_area_km2`, 357,683 km²).

## Consequences

- Rankings and areas are exact counts of the classified maps, independent of
  the 500 m aggregation of the maps (ADR 3).
- Grassland and permanent crops are excluded from all crop rankings, the
  crop change and the crop plots of the field size analyses (ADR 8, 11), so
  "crop" means the same everywhere in the project.

## Alternatives considered

- Including grassland as a "crop": rejected, it is land use rather than a
  rotating crop and would dominate the rankings.
