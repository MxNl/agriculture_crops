# 10. Field size measures

- Status: accepted
- Date: 2026-10-21 (edges, typical hectare: 2026-10-26)
- Code: `R/plot_field_sizes.R`; settings `field_hex_km2`, `field_min_count`

## Context

Field size distributions are strongly right-skewed: many small fields, few
very large ones. A single mean is dominated by the large fields and says
little about what a typical field looks like.

## Decision

- **Median field size** per hexagon (ADR 6) and per state.
- **Relative size**: a hexagon's median in percent of the median of all
  arable polygons in Germany, binned symmetrically on a log scale (80 % and
  125 % are both a factor of 1.25 from the median), so "smaller" and
  "larger" are balanced.
- **Size of the field the typical hectare lies in**: the area-weighted
  median, i.e. half of the arable land lies in fields at least this large.
  It describes the land rather than the count of fields, and is the 50 %
  crossing of the cumulative share of land by field size.
- **Field edges**: total perimeter / total area (m per ha) per hexagon and
  state, as an indicator of field margins, hedges and ditches. A border
  between two fields counts for both; field blocks lack the borders inside
  them and show too few edges.
- Distributions are shown on a log scale (ridgelines).

## Consequences

- Median and area-weighted median together describe both views: the typical
  field and the field the typical hectare lies in; they differ strongly
  where a few large fields cover much land (e.g. the east).
- Edge density depends on size and shape; it is not a shape index.

## Alternatives considered

- Mean field size: dominated by large fields.
- A shape index (perimeter relative to a circle of the same area): noted as
  a possible addition, not implemented.
