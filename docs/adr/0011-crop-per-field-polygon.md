# 11. Crop of a field polygon from the DLR map, and its validation

- Status: accepted
- Date: 2026-10-21
- Code: `R/field_crops.R`; settings `field_crop_min_share`,
  `field_crop_validation`

## Context

Only some states publish the declared crop per field, and blocks and
reference parcels carry no crop at all. To relate field size and crop in all
states, the crop has to come from the DLR map (ADR 1).

## Decision

- Per polygon, the DLR class covering most of it (class 0 never wins) and
  the share of the polygon it covers, with pixels weighted by the fraction
  inside the polygon (exactextractr).
- A polygon counts as a single-crop polygon if one crop covers at least
  `field_crop_min_share` (75 %); only these enter the crop analyses.
- The latest DLR year (2024) is used with the latest field editions, which
  are partly newer; this assumes the polygons change little between years.
- Validation: for editions with declared crops (Brandenburg and North
  Rhine-Westphalia 2023, same year as a DLR map), the declared HCAT crops are
  mapped to DLR classes and the share of the declared area where the DLR
  majority class agrees is computed, for all fields and for single-crop
  fields.

## Consequences

- One crop source for all states; field blocks and reference parcels with
  several crops drop out by the 75 % rule.
- The agreement figure (shown in the plot subtitle) quantifies how far the
  DLR classes can be trusted at field level.
- The year mismatch between polygons and crop map adds uncertainty, mainly
  for fields (not for the stable blocks).

## Alternatives considered

- Declared crops where available: field-exact, but only for some states, so
  the states would not be comparable.
