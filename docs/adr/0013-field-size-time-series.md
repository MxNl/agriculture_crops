# 13. Field sizes over time from the yearly editions

- Status: accepted
- Date: 2026-10-26
- Code: `R/field_tables.R`, `R/plot_field_sizes.R`; setting
  `field_time_series`

## Context

Most states publish only their current polygons. Brandenburg and North
Rhine-Westphalia offer yearly editions in FTW.

## Decision

- Use all yearly editions of these two states (Brandenburg & Berlin
  2010–2026, North Rhine-Westphalia 2019–2026; none for 2024 and 2025),
  processed like the latest editions (ADR 7).
- Per year and land use (arable land, grassland): the median field size and
  the size of the field the typical hectare lies in (ADR 10), and the size
  distribution of the arable fields per year.

## Consequences

- A real time series of declared fields, but only for two states; it cannot
  be generalised to Germany.
- Changes in how a state digitises or declares fields appear as changes in
  field size.
