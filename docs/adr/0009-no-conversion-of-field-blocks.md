# 9. Field blocks are not converted to field sizes

- Status: accepted (the correction was implemented and then removed)
- Date: 2026-10-28
- Code: none (removed); the caveat is stated in the plots

## Context

Four states publish only field blocks, which are larger than fields. A
correction was tried: Brandenburg and Lower Saxony publish both fields and
blocks, so their size distributions give a conversion curve (quantile
mapping per land use: a block at the p-th percentile of the block sizes gets
the size of the field at the p-th percentile of the field sizes), applied to
the block-only states (SN, ST, TH ← BB; SH ← NI).

## Decision

Do not convert field blocks; show them as field blocks and state that they
hold several fields and are therefore larger.

The cross-check (curve of one reference state applied to the blocks of the
other, compared with its real fields) was acceptable for all land uses
pooled (median error −6 % and +2 %) but not for arable land, the basis of
the analyses (ADR 8): −34 % (Brandenburg blocks with the Lower Saxony
curve) and +38 % (the reverse). Brandenburg and Lower Saxony define field
blocks too differently for a transferable curve.

## Consequences

- Maps and charts are honest about the unit; comparisons between the
  block-only states and the others must be read with the caveat.
- No uncertain modelled values in the results.

## Alternatives considered

- Quantile mapping with the curve of the most similar state (implemented,
  removed, see above).
- One factor (median block / median field): simpler, but treats small and
  large blocks alike and has the same transfer problem.
- An average curve of both reference states: cannot be cross-checked.
