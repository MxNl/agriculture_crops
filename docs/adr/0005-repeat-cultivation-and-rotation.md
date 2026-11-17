# 5. Repeat cultivation and crop rotation from per-pixel sequences

- Status: accepted
- Date: 2026-10-17
- Code: `R/crop_change_stats.R`, `R/plot_crop_change.R`

## Context

Whether a crop returns to the same land year after year (monoculture,
narrow rotation) or alternates with others matters for soil and water. This
needs the crop sequence of each pixel, not only areas.

## Decision

- **Repeat cultivation:** per pixel, which of the analysed crops it grew at
  least once in 2017–2024 (a bit mask over the years). Per 500 m cell:
  - pixels that grew the crop at least once (`n_ever`),
  - the summed years with the crop (`sum_years`, the sum of the yearly
    class counts),
  - mean years grown = `sum_years / n_ever`, from 1 (once) to 8 (every
    year).
  Cells are blank where `n_ever` is below 5 % of the mean farmland.
- **Rotation:** national pixel counts of the transitions from each crop in
  year *t* to the crop in year *t* + 1, for all seven consecutive year pairs;
  shown as the mean annual area per transition. Crops outside the analysed
  ones are grouped as "other crops" and "grassland & other".

## Consequences

- The mean years grown separate repeated cultivation (high values) from
  rotation (values near 1–2) and are comparable between crops.
- Both measures rely on the pixel-exact co-registration of all years
  (ADR 1). Classification errors in single years look like rotation, so
  repeat cultivation is rather underestimated.
- Rotation is national only; regional transition matrices would need much
  more storage.

## Alternatives considered

- Share of years with the crop on all farmland: mixes how often a crop is
  grown with how widespread it is.
