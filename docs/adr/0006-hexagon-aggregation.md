# 6. Hexagons of 25 km² as the spatial unit of the summary maps

- Status: accepted
- Date: 2026-10-21 (crop change maps: 2026-11-16)
- Code: `R/plot_field_sizes.R` (`hex_summary()`), `R/plot_crop_change.R`
  (`hex_sums()`); settings `field_hex_km2`, `field_min_count`

## Context

Field sizes are a property of polygons, not of pixels, and the 500 m crop
change maps are noisy at the national scale. Administrative units
(districts) differ strongly in size and shape.

## Decision

- Summarise on a regular grid of hexagons of 25 km² (`field_hex_km2`) in the
  reference CRS.
- Fields are assigned to the hexagon their centroid lies in; a hexagon is
  shown with at least `field_min_count` (20) polygons.
- For the crop change and repeat maps, the 500 m pixel counts are summed per
  hexagon (coverage-weighted) and the measures are computed from the sums
  (ADR 4, 5); the farmland thresholds are applied relative to the pixels the
  hexagon's cells hold. The 500 m versions are kept.

## Consequences

- Equal-area units make the maps comparable across Germany; hexagons have
  no preferred direction and neighbours at equal distance.
- Measures from summed counts weight each cell by its farmland, unlike
  averages of cell values.
- 25 km² smooths small structures (river valleys, edges of special-crop
  areas).

## Alternatives considered

- Districts / municipalities: unequal sizes distort the map impression.
- Square grid cells: possible, hexagons were chosen for the more even
  neighbourhood.
