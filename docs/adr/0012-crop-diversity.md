# 12. Crop diversity as the effective number of crops

- Status: accepted
- Date: 2026-10-26 (pooled years: 2026-11-01)
- Code: `R/plot_field_sizes.R` (`effective_crops()`)

## Context

The question "do large fields go with few crops?" needs a diversity measure
per area that accounts for both the number of crops and their evenness.

## Decision

- Per hexagon, the effective number of crops = exp(Shannon index) of the
  crop classes (ADR 2), from the 500 m class counts (ADR 3). It is the
  number of equally common crops that would give the same diversity.
- Two variants: the latest DLR year, and all years pooled (counts summed
  over 2017–2024), so a hexagon whose crop mix changes from year to year
  counts as more diverse (rotation).
- Field size and diversity are each classed into terciles of the hexagons
  and shown as a 3 × 3 bivariate map.

## Consequences

- Terciles show relative positions, not absolute values; the class limits
  differ between the two variants, so the maps compare ranks, not whether
  diversity went up or down.
- Diversity is measured per hexagon, not per field: an area where each field
  always grows the same crop but neighbours differ still looks diverse.
  Rotation diversity per field would need a new per-pixel pass over all
  years.

## Alternatives considered

- Number of crop classes (richness): ignores evenness, a single pixel of a
  crop counts fully.
- Simpson index: similar; the effective number of crops is easier to
  explain.
