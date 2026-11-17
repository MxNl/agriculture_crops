# Architecture decision records: methods

Decisions about the data and the analysis methods of this project, one file
per decision. Decisions about the look of the plots (colours, fonts,
layouts) are not recorded here.

The records were written after the fact, from the code, the commit history
and the discussions while the project was built; "Alternatives considered"
lists options that were discussed or are the obvious alternatives. Settings
named in the records are in `definitions.yml`, with the values in effect
when the record was written.

## Crop maps (DLR CropTypes)

- [1. DLR CropTypes maps as the source of crop data](0001-dlr-croptypes-as-crop-data.md)
- [2. What counts as a crop, and ranking crops by area](0002-crops-and-ranking-by-area.md)
- [3. A 500 m grid for crop maps and statistics, computed in one pass](0003-500-m-grid.md)
- [4. Crop change as the change of the share of farmland between two periods](0004-crop-change-between-periods.md)
- [5. Repeat cultivation and crop rotation from per-pixel sequences](0005-repeat-cultivation-and-rotation.md)
- [6. Hexagons of 25 km² as the spatial unit of the summary maps](0006-hexagon-aggregation.md)

## Field sizes (Fields of the World)

- [7. Field boundaries from Fields of the World, with the unit each state publishes](0007-fields-of-the-world-boundaries.md)
- [8. Field size analyses on arable land only](0008-arable-land-only.md)
- [9. Field blocks are not converted to field sizes](0009-no-conversion-of-field-blocks.md)
- [10. Field size measures](0010-field-size-measures.md)
- [11. Crop of a field polygon from the DLR map, and its validation](0011-crop-per-field-polygon.md)
- [12. Crop diversity as the effective number of crops](0012-crop-diversity.md)
- [13. Field sizes over time from the yearly editions](0013-field-size-time-series.md)
- [14. Example windows chosen from the data](0014-zoom-windows-from-the-data.md)

## Specialty crops (Thünen crop type map)

- [15. Specialty crop regions found with a location quotient](0015-specialty-crop-regions.md)

## Workflow

- [16. A targets pipeline instead of numbered scripts](0016-targets-pipeline.md)

## Method diagrams

Flow charts of the methods are in [`../diagrams/`](../diagrams/) as Mermaid
files (`.mmd`), rendered as SVG next to them. In Quarto they can be used
directly:

````markdown
```{mermaid}
%%| file: diagrams/crop_maps.mmd
```
````

## Format

Each record has a status (accepted, superseded, …), a date, the code and
settings it concerns, and the sections Context, Decision, Consequences and,
where useful, Alternatives considered. A new decision gets the next number;
a changed decision gets a new record that supersedes the old one, which
stays.
