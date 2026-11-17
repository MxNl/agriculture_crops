# 7. Field boundaries from Fields of the World, with the unit each state publishes

- Status: accepted
- Date: 2026-10-21
- Code: `R/field_tables.R`, `R/functions.R` (`download_fields()`,
  `read_fields_ftw()`, `classify_land_use()`, `measure_fields()`); settings
  `field_collections`, `field_block_collections`, `field_min_ha`

## Context

Field sizes need field polygons. The German states publish their IACS
(InVeKoS) geodata differently: some publish the declared fields, others only
the reference units of their land parcel identification system.

## Decision

- Use the harmonised field boundaries of Fields of the World (FTW,
  source.coop/ftw/harmonized-field-data), one GeoParquet per state, latest
  edition. Editions used (from the files' metadata, downloaded October
  2026): Brandenburg 2026, Lower Saxony 2025, North Rhine-Westphalia 2026,
  Saarland probably 2024 (only the copyright year is given), Bavaria current
  status 2026 (no edition year; parcel dates 2008–2026), Hesse 2025,
  Schleswig-Holstein 2026, Saxony 2026, Saxony-Anhalt 2023, Thuringia
  current status 2026 (block dates 2007–2026); the comparison blocks of
  Brandenburg and Lower Saxony 2026.
- Keep what each state publishes and label it:
  - **fields** (declared parcels): Brandenburg & Berlin, Lower Saxony
    (with Bremen & Hamburg), North Rhine-Westphalia, Saarland;
  - **reference parcels**: Bavaria, Hesse;
  - **field blocks** (permanent units that can hold several fields):
    Schleswig-Holstein, Saxony, Saxony-Anhalt, Thuringia.
  The field blocks of Brandenburg and Lower Saxony are read as well, only to
  compare blocks with fields.
- Baden-Württemberg, Mecklenburg-Western Pomerania and Rhineland-Palatinate
  are not included (no comparable open data).
- Compute areas and perimeters from the geometry in the reference CRS
  (EPSG:25832); the published areas differ between states (some are
  eligible areas).
- Drop polygons below `field_min_ha` (0.1 ha), the minimum parcel size for
  direct payments, which removes landscape elements and slivers.
- Map each state's land use coding (HCAT codes, "Ackerland" /
  "Dauergrünland" / "Dauerkultur(en)", AL / GL / DK codes, …) to arable
  land, grassland, permanent crops and other; "other" is not farmland.

## Consequences

- One consistent area definition for all states.
- The units are not equivalent: field blocks hold several fields and are
  larger; reference parcels lie in between. Comparisons across states must
  keep the unit in mind; the plots group states by unit.
- The polygons of the latest editions are newer than the latest DLR map
  (2024), see ADR 11.

## Alternatives considered

- Downloading the state portals directly: FTW already harmonises formats
  and attributes.
- Converting field blocks to field sizes: tested and rejected (ADR 9).
