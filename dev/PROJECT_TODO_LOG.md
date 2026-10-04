# riskGenerateR - Project TODO Log

## Package purpose

`riskGenerateR` generates synthetic spatial risk datasets for testing,
examples, QA workflows, vignettes, dashboards, Shiny prototypes,
stress-testing, and package development across the GeoRisk Verse.

The package currently generates `sf` objects only.

Future support may include:

- H3
- WKT/WKB
- terra/raster objects
- non-spatial join tables
- topology stress-testing workflows
- packaged example datasets

---

# Current package status

## Current exported functions

### Study area and example helpers

- `rgr_bbox_example()`
- `rgr_study_area()`
- `rgr_example_data()`

### Geometry generators

- `rgr_points()`
- `rgr_grid()`
- `rgr_hex()`
- `rgr_polygons()`

### Risk generators

- `rgr_add_risk_attributes()`

### Scenario helpers

- `rgr_scenario()`

### QA helpers

- `rgr_summary()`
- `print.rgr_summary()`

### Topology stress-testing helpers

- `rgr_topology_modify()`

---

## Current implemented scenarios

- `"basic"`
- `"grid_counts"`
- `"hex_counts"`
- `"irregular_polygons"`

---

## Current topology modification modes

- `"overlap"`

---

## Current test status

Current automated tests passing:

```r
devtools::test()
```

Current status:

- 179 passing tests
- no failing tests
- no skipped tests

Test coverage currently includes:

- study area generation
- bbox helpers
- point generation
- grid generation
- hex generation
- irregular polygon generation
- synthetic risk attributes
- CRS handling
- geographic CRS regression testing
- scenario generation
- example data helpers
- topology overlap generation
- summary outputs
- geometry validity checks

Real-world testing completed using:

- Cottesloe LGA (WA coastline polygon)
- irregular coastal geometry
- geographic CRS inputs
- topology stress-testing workflows

---

# Current design decisions

## Package scope

- Package is generation-focused only.
- Current outputs are `sf`.
- Analysis workflows belong in companion packages such as `riskworkflowr`.
- ETL workflows belong in `etlspatial`.
- General QA/helper utilities may later belong in a separate helper/core package.

---

## Spatial generation philosophy

- Default synthetic study area remains neutral and not state-specific.
- `wa_outback` is an example preset only.
- Functions should remain globally usable.
- Internal CRS handling should be robust.
- Geographic CRS inputs should be supported safely.

---

## Geometry generation decisions

### Grids and hexes

- `rgr_grid()` and `rgr_hex()` default to `clip = FALSE`.
- Clipping remains optional.
- `rgr_hex()` is generic sf hex tessellation only.
- True H3 support should remain separate.

### Irregular polygons

- `rgr_polygons()` defaults to `clip = TRUE`.
- Clean topology generation is the default behaviour.
- Stress-testing topology modifications occur separately using `rgr_topology_modify()`.

### Risk attributes

- `rgr_add_risk_attributes()` keeps one row per geometry.
- Polygon/count mode uses `dominant_*` fields.
- Detailed grouped hazard summaries should later use non-spatial join tables.

---

## CRS handling decisions

Current design:

- projected CRS preferred for geometry generation
- geographic CRS inputs transformed internally where required
- users may supply `processing_crs`
- outputs may optionally return to original CRS

Current functions supporting CRS-aware workflows:

- `rgr_points()`
- `rgr_polygons()`

---

# Current development strengths

The package now supports:

- realistic synthetic study areas
- random event generation
- inside/outside event generation
- square grid generation
- hex tessellation generation
- irregular polygon generation
- synthetic risk attributes
- exposure/event count generation
- bundled scenarios
- CRS-aware workflows
- real coastline polygon testing
- topology stress-testing
- reproducible synthetic datasets
- downstream workflow testing

The package is now beyond prototype stage and functioning as a usable synthetic spatial testing framework.

---

# Essential next development tasks

## 1. Improve documentation

High priority:

- improve roxygen examples
- add parameter explanations
- add conceptual documentation
- add generation philosophy notes
- add CRS guidance
- explain topology stress-testing concepts

---

## 2. Add vignettes

Priority vignettes:

- Getting started
- Generating study areas
- Generating event points
- Working with grids and hexes
- Generating irregular polygons
- Using synthetic risk attributes
- CRS-aware workflows
- Topology stress-testing
- Using outputs with `riskworkflowr`

---

## 3. Add pkgdown site

Tasks:

- configure `_pkgdown.yml`
- create reference groups
- add articles
- create package homepage
- integrate NEWS.md
- add lifecycle badge and package philosophy sections

---

## 4. Expand topology stress-testing

Current mode:

- `"overlap"`

Potential future modes:

- `"gap"`
- `"sliver"`
- `"invalid"`
- `"duplicate"`
- `"fragmented"`
- `"mixed"`

Important:

- keep clean generation separate from topology corruption
- topology modification should remain deliberate and controlled

---

## 5. Add true H3 support

Potential future functions:

- `rgr_h3()`
- `rgr_h3_grid()`

Needs:

- `h3jsr`
- true H3 resolution support
- `h3_id`
- optional clipping
- optional assignment workflows

Important:

- do not confuse H3 with generic sf hex tessellation

---

## 6. Expand scenario generation

Potential future scenarios:

- `"visitor_safety"`
- `"smr_demo"`
- `"hotspot_points"`
- `"edge_cases"`
- `"topology_failure"`
- `"coastal_polygons"`
- `"corridor_network"`
- `"multi_region"`
- `"temporal_events"`

---

## 7. Improve point generation

Potential future options:

```r
distribution = c(
  "random",
  "clustered",
  "edge",
  "linear",
  "mixed"
)
```

Potential outside strategies:

```r
outside_strategy = c(
  "ring",
  "bbox",
  "buffer"
)
```

---

## 8. Improve polygon generation

Current method:

- `"seeded_grid"`

Potential future methods:

- `"voronoi"`
- `"patchwork"`
- `"corridor"`
- `"coastal"`
- `"fragmented"`

---

## 9. Add non-spatial grouped risk tables

Potential future function:

```r
rgr_risk_table()
```

Purpose:

- grouped hazard/event summaries
- non-spatial joins
- multiple hazard rows per feature
- cleaner downstream analytics

---

# Possible future helper/core package

Potential future package responsibilities:

- topology checking
- CRS helpers
- geometry diagnostics
- sf QA summaries
- benchmarking
- workflow assertions
- timing/logging
- geometry validity helpers

Possible package concepts:

- `georiskcore`
- `geoRiskUtils`
- `spatialQAr`

Current thinking:

- keep `riskGenerateR` focused on generation only

---

# Nice-to-have future features

## Synthetic temporal support

Potential support for:

- timestamps
- seasonal trends
- hourly patterns
- temporal clustering

---

## Synthetic network/corridor support

Potential support for:

- road-like geometries
- trail networks
- coastline corridors
- river systems

---

## Synthetic raster support

Potential future support for:

- heat surfaces
- exposure rasters
- terrain-like surfaces
- synthetic hazard rasters

---

## Shiny/demo support

Potential future:

- interactive scenario builders
- topology stress-testing apps
- QA dashboards
- teaching/demo applications

---

# Pre-public release checklist

Before public release:

- [ ] final file cleanup
- [ ] remove local/private paths
- [ ] ensure ABS-derived local data excluded
- [ ] clean README examples
- [ ] finalise DESCRIPTION
- [ ] improve examples
- [ ] add vignettes
- [ ] create pkgdown site
- [ ] review dependency footprint
- [ ] check package naming consistency
- [ ] add license checks
- [ ] review exported functions
- [ ] review object naming consistency
- [ ] perform fresh clone/install test
- [ ] perform Windows clean-session test

---

# Current milestone

`riskGenerateR` has moved beyond skeleton/prototype stage.

The package now functions as a reusable synthetic spatial risk data generation framework suitable for:

- package development
- workflow QA
- topology stress-testing
- spatial demonstrations
- reproducible examples
- downstream GeoRisk Verse testing
- synthetic operational spatial environments
