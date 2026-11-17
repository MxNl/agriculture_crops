# Method diagrams

Flow charts of the methods, as Mermaid source (`.mmd`) with a rendered SVG
next to each. The numbers in brackets refer to the decision records in
[`../adr/`](../adr/).

| Chart | Diagram | Topic |
|---|---|---|
| 1 | [`crop_maps.mmd`](crop_maps.mmd) | crop ranking, crop change, repeat cultivation and rotation (DLR CropTypes) |
| 2 | [`field_sizes.mmd`](field_sizes.mmd) | field sizes from the Fields of the World field boundaries |
| 3 | [`field_crops.mmd`](field_crops.mmd) | crops of the field polygons, validation and crop diversity |
| 4 | [`specialty_regions.mmd`](specialty_regions.mmd) | regions specialised in one crop (Thünen crop type map) |
| 5 | [`pipeline.mmd`](pipeline.mmd) | the main steps of the targets pipeline and their inputs |

Node types: data (blue cylinders), method steps (grey boxes), criteria such
as filters and thresholds (yellow hexagons) and results, i.e. plots (green
rounded boxes).

## Datasets

Every dataset in the charts has an ID, so a dataset used in one chart can be
traced to where it comes from.

| ID | Dataset | Origin |
|---|---|---|
| D1 | DLR CropTypes maps, 10 m, 2017–2024, reprojected to one grid | source data (ADR 1) |
| D2 | Fields of the World, latest edition per state | source data (ADR 7) |
| D3 | Fields of the World, yearly editions (BB, NW), incl. declared crops | source data (ADR 11, 13) |
| D4 | Thünen crop type map 2026 | source data (ADR 15) |
| D5 | Class counts per 500 m cell and year, crop sequence of every pixel | created in chart 1 from D1 (ADR 3) |
| D6 | Field tables per state | created in chart 2 from D2 (ADR 7) |

The charts use the plot font Noto Sans Display (set in the `config` block
of each file). Where it is not installed, the browser falls back to its
sans-serif font; install it from `cache/fonts/` or Google Fonts for the
intended look. Box sizes are computed with the font available when the
chart is rendered, so render with the font installed.

## Use in Quarto

Quarto renders Mermaid natively; the file can be included as is (the colours
and layout come from the `config` block at the top of each file):

````markdown
```{mermaid}
%%| file: diagrams/crop_maps.mmd
%%| fig-cap: "Crop ranking, crop change, repeat cultivation and rotation"
```
````

The SVGs contain HTML labels, which browsers and Quarto show, but some
graphics programs do not; use the Mermaid source or a PNG export there.

## Rendering

With the Mermaid CLI (`npm install @mermaid-js/mermaid-cli`):

```sh
mmdc -i crop_maps.mmd -o crop_maps.svg -b white
```
