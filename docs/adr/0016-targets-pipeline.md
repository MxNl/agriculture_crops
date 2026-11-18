# 16. A targets pipeline instead of numbered scripts

- Status: accepted
- Date: 2026-11-16
- Code: `_targets.R`, `R/`

## Context

The analysis consisted of eight numbered scripts that had to be run in
order, each skipping outputs that already existed. Whether a result was up
to date after a change of settings or code was up to the user.

## Decision

- Use the targets package: the steps are functions in `R/`, `_targets.R`
  defines their dependencies, `targets::tar_make()` rebuilds what is
  outdated.
- Split the data steps per year (DLR maps, pixel counts) and per state
  (downloads, field tables, crops per polygon); one target per plot step.
- `definitions.yml` is a tracked input. The long-running steps receive only
  the settings they read (small settings targets), so changes of e.g. plot
  settings do not rerun the hour-long change statistics.
- Downloads, the reprojected DLR maps and the resampled Thünen map are kept
  once they exist (their file names fully describe them); all computed
  results are rebuilt by targets when outdated.

## Consequences

- Results are reproducible and provably up to date (`tar_outdated()`); the
  dependency graph documents the method (`tar_visnetwork()`).
- The first run after the switch recomputes everything once.
- A plot step still renders all plots of its former script; splitting them
  is a possible next step.
