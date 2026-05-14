# riskGenerateR — Project TODO Log

## Package purpose

`riskGenerateR` generates synthetic spatial risk datasets for testing, examples, QA workflows, vignettes, dashboards, and package development across the GeoRisk Verse.

The package currently generates `sf` objects only. Future support may include H3, WKT/WKB, terra, raster, and tabular/non-spatial join tables.

---

## Current status

Working functions:

- `rg_bbox_example()`
- `rg_study_area()`
- `rg_points()`
- `rg_grid()`
- `rg_hex()`
- `rg_add_risk_attributes()`

Current example bbox:

- `wa_outback`
- EPSG:7851
- synthetic 5 km by 5 km inland Western Australia bbox

Current design decisions:

- Core functions should remain globally usable.
- Default study area is neutral synthetic coordinate space.
- WA bbox is an example preset, not hardwired into functions.
- `rg_grid()` and `rg_hex()` default to `clip = FALSE`.
- `clip = TRUE` remains available for clipped study-area outputs.
- `rg_hex()` is a generic sf hex tessellation, not H3.
- Future true H3 support should use a separate function such as `rg_h3()` or `rg_h3_grid()`.
- `rg_add_risk_attributes()` keeps one row per geometry.
- Polygon/count mode uses `dominant_*` fields.
- Detailed grouped hazard summaries should later be handled by non-spatial join tables.

---

## Next development steps

### 1. Commit current working state

Run:

```r
devtools::document()
devtools::test()