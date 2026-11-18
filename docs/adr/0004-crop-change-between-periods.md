# 4. Crop change as the change of the share of farmland between two periods

- Status: accepted
- Date: 2026-10-17
- Code: `R/plot_crop_change.R`; settings `change_periods`, `change_top_n`

## Context

The area of a crop in a cell fluctuates strongly from year to year because of
crop rotation: a field grows wheat one year and rapeseed the next. Comparing
two single years mostly shows rotation, not a lasting shift.

## Decision

- Compare two multi-year periods, 2017–2020 and 2021–2024
  (`change_periods`), which averages out rotation.
- Measure per 500 m cell the crop's share of the farmland (crop pixels /
  farmland pixels, summed over the years of a period) and show the
  difference in percentage points (pp), e.g. 20 % → 25 % = +5 pp.
- Leave cells blank where the mean farmland per year is below 10 % of the
  cell in either period.
- Analyse the `change_top_n` (6) crops with the largest mean area over all
  years.
- The national change uses the same measure on the national counts.

## Consequences

- Shares are independent of how much farmland a cell has, so cells with
  little farmland are comparable to arable regions, and changes in the
  farmland itself (e.g. new settlements) do not appear as crop change.
- Percentage points, not relative change: a crop going from 1 % to 2 % is
  +1 pp, not +100 %, which avoids exaggerating rare crops.
- A binned diverging scale with a neutral middle bin (±2 pp) hides small,
  noisy changes.

## Alternatives considered

- Single years (e.g. 2017 vs 2024): dominated by rotation and weather.
- Trend over all years (regression per cell): more sensitive, but harder to
  explain and noisy with eight years per cell.
- Change of crop area instead of share: mixes crop change with changes of
  the farmland area.
