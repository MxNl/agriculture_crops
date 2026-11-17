# 1. DLR CropTypes maps as the source of crop data

- Status: accepted
- Date: 2026-10-17
- Code: `R/croptypes.R`; settings `years`, `crs_reference`

## Context

The project maps where crops grow in Germany and how this changes over the
years. It needs a crop map that covers all of Germany, distinguishes the main
field crops and is available for several consecutive years with one
consistent method.

## Decision

- Use the DLR CropTypes maps of Germany (V02, 10 m, Asam et al. 2022;
  Gessner et al. 2025) for all years in `years` (2017–2024).
- Reproject each yearly map to the reference CRS ETRS89 / UTM 32N
  (EPSG:25832) with nearest-neighbour resampling onto a 10 m grid snapped to
  multiples of 10 m (`gdalwarp -tap`), so all years share one pixel grid.
- Treat the DLR classes as they are; class 0 (no agricultural use, outside
  Germany) is "no farmland".

## Consequences

- All crop statistics are pixel counts on the same grid, so years can be
  compared pixel by pixel (repeat cultivation, rotation; ADR 5).
- Nearest neighbour keeps the classes categorical; areas change slightly
  through the reprojection (sub-pixel shifts), which is negligible at the
  national and 500 m scale.
- The maps are classifications of satellite data: crops that are rare or hard
  to separate (e.g. spring vs winter cereals) carry classification errors,
  which the field validation (ADR 11) quantifies against declared crops.

## Alternatives considered

- InVeKoS / IACS declarations: field-exact and authoritative, but only open
  for some states and years (used for field sizes, ADR 7, and the validation).
- Thünen crop type map: used for the specialty crops (ADR 15), as it
  separates permanent crops (hops, orchards, grapevine) in more detail; the
  DLR series offers the longer consistent time series for the main crops.
