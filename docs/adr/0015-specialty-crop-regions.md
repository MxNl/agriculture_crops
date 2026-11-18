# 15. Specialty crop regions found with a location quotient

- Status: accepted
- Date: 2026-10-27
- Code: `R/plot_specialty_regions.R`; settings `thuenen_*`, `specialty_*`

## Context

Regions specialised in one crop (grapevine, hops, orchards, vegetables, …)
are small and do not show on national maps. They should be found
objectively, not picked by hand.

## Decision

- Use the Thünen crop type map (2026, v302), which separates permanent and
  specialty crops in detail; read it over HTTP (cloud-optimised GeoTIFF)
  and resample it to 80 m (`specialty_search_res_m`) with GDAL, which uses
  the overviews.
- Count the classes per 2 km cell (`specialty_cell_km`). Farmland excludes
  small woody features and other areas (`specialty_non_farmland`);
  permanent and cultivated grassland, fallow land and other agricultural
  areas count as farmland but are not candidates (`specialty_excluded`).
- A **core cell** of a crop has at least 30 % farmland
  (`specialty_min_farmland`), the crop covers at least 15 % of the farmland
  (`specialty_cell_share`), and its share is at least 10 times its share in
  Germany (location quotient, `specialty_min_lq`). So the crop is both
  dominant locally and rare nationally.
- Neighbouring core cells form a region; gaps of one cell are bridged.
  Regions need at least 500 ha of the crop (`specialty_min_region_ha`).
- Greedy selection by crop area × share: at most 2 regions per crop
  (`specialty_max_per_crop`), centres at least 60 km apart
  (`specialty_min_distance_km`), at most 9 panels (`specialty_panels`).
- Each window shows the region plus 15 % of its size on each side
  (`specialty_context`), between 8 and 60 km, read at the resolution of the
  plot.

## Consequences

- Reproducible, transparent criteria; the thresholds are settings.
- The location quotient favours crops that are rare nationally; widespread
  crops (wheat, maize) cannot form specialty regions by design.
- The 2 km cells and 80 m resampling find regional clusters, not single
  farms.

## Alternatives considered

- Image recognition / clustering: less transparent for the same purpose.
- Hand-picked well-known regions: not reproducible, would miss lesser-known
  ones.
