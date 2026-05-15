# riskGenerateR — Project TODO Log

## Package purpose

`riskGenerateR` generates synthetic spatial risk datasets for testing, examples, QA workflows, vignettes, dashboards, Shiny prototypes, and package development across the GeoRisk Verse.

The package currently generates `sf` objects. Future support may include H3, WKT/WKB, terra/raster objects, and non-spatial join tables.

---

## Current working functions

- `rg_bbox_example()`
- `rg_study_area()`
- `rg_points()`
- `rg_grid()`
- `rg_hex()`
- `rg_polygons()`
- `rg_add_risk_attributes()`
- `rg_scenario()`
- `rg_summary()`
- `print.rg_summary()`

Current test status:

- Core tests passing
- Point, grid, hex, polygon, risk attribute, scenario, and summary workflows tested
- Real irregular polygon dev test using Cottesloe LGA now working

---

## Current design decisions

- Package is generation-focused only.
- Current outputs are `sf`.
- Analysis workflows belong in companion packages such as `riskworkflowr`.
- Saving/export belongs in other packages/workflows.
- Default synthetic study area remains neutral and not state-specific.
- `wa_outback` is an example preset, not a hardwired package default.
- `rg_grid()` and `rg_hex()` default to `clip = FALSE`.
- `rg_polygons()` defaults to `clip = TRUE`.
- `rg_hex()` is a generic sf hex tessellation, not true H3.
- True H3 support should be added separately as `rg_h3()` or `rg_h3_grid()`.
- `rg_add_risk_attributes()` keeps one row per geometry.
- Polygon/count mode uses `dominant_*` fields.
- Detailed grouped hazard summaries should be handled later by non-spatial join tables.
- CRS handling must be robust:
  - projected CRS preferred for geometry generation
  - geographic CRS inputs should be transformed internally where needed
  - users should be able to supply `processing_crs`

---

## Immediate pre-publish tasks

### 1. Commit and push private repo

Run:

```r
devtools::document()
devtools::test()