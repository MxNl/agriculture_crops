# 8. Field size analyses on arable land only

- Status: accepted
- Date: 2026-10-28
- Code: `R/plot_field_sizes.R`

## Context

The first field size maps pooled arable land, grassland and permanent crops.
Grassland plots are half of the polygons and about half the size of arable
fields (median ~1.1 vs ~2.3 ha), so regions rich in grassland (uplands,
Alpine foothills, coastal marshes) looked "small-scale" because of the
grassland, not because of farm structure. The crop plots (ADR 2) already
excluded grassland.

## Decision

- All field size maps and charts use arable land only.
- Exceptions that compare the land uses on purpose: the map of arable land
  and grassland side by side, and the time series.
- The crop plots of the field size analysis also exclude the non-crop
  classes (`non_crop_values`, ADR 2).

## Consequences

- Field sizes describe the structure of arable farming and are consistent
  with the crop analyses.
- The national median rises (about 1.4 → 2.0 ha) and is not comparable to
  statistics that include grassland.

## Alternatives considered

- Pooled farmland with a separate grassland map: rejected as confusing,
  grassland would be in some analyses and not in others.
