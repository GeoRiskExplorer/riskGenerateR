\# riskGenerateR

<!-- badges: start -->

![Lifecycle](https://img.shields.io/badge/lifecycle-early_development-orange.svg)

<!-- badges: end -->

`riskGenerateR` generates synthetic spatial risk datasets for testing,
development, demonstrations, QA workflows, reproducible examples,
dashboards, and spatial risk package development.

The package is designed as part of the GeoRisk Verse ecosystem and
currently focuses on generating synthetic `sf` spatial objects.

---

## GeoRisk Verse

`riskGenerateR` is part of the broader GeoRisk Verse ecosystem of
spatial risk analysis and workflow packages.

The package focuses specifically on synthetic spatial data generation
for:

- testing
- demonstrations
- QA workflows
- reproducible examples
- dashboards
- package development
- Shiny prototypes
- spatial workflow validation

Spatial analysis workflows themselves are intended to be handled by
companion packages within the GeoRisk Verse ecosystem.

---

## Current features

- Study area generation
- Example bounding boxes
- Random point generation
- Inside/outside event generation
- Square grids
- Hex tessellations
- Irregular polygon generation
- Synthetic risk attributes
- Synthetic exposure and event counts
- Scenario generators
- Spatial QA summaries
- CRS-aware geometry generation workflows

---

## Installation

```r
# development version

# remotes::install_github(
#   "GeoRiskExplorer/riskGenerateR",
#   build_vignettes = FALSE
# )
```

---

## Quick example

```r
library(riskGenerateR)
library(mapview)

x <- rg_example_data("basic_scenario")

mapview(
  x$hex_risk,
  zcol = "risk_class"
) +
  mapview(
    x$points_risk,
    zcol = "risk_class"
  )
```

---

## Package philosophy

`riskGenerateR` focuses on generating synthetic spatial datasets only.

Spatial analysis workflows such as:

- spatial joins
- rates
- SMR
- choropleths
- aggregation
- DuckDB integration
- spatial risk metrics

are intentionally handled by companion packages within the
GeoRisk Verse ecosystem.

This separation keeps:

- generation workflows lightweight
- dependencies smaller
- package responsibilities clearer
- downstream workflows more modular

---

## Current object support

Current outputs are primarily:

- `sf`

Future support may include:

- H3
- WKT/WKB
- terra
- raster
- non-spatial join tables

---

## Current exported functions

### Study area and example helpers

- `rg_bbox_example()`
- `rg_study_area()`
- `rg_example_data()`

### Geometry generators

- `rg_points()`
- `rg_grid()`
- `rg_hex()`
- `rg_polygons()`

### Risk generators

- `rg_add_risk_attributes()`

### Scenario helpers

- `rg_scenario()`

### QA helpers

- `rg_summary()`

---

## Design principles

- generation-focused
- lightweight dependencies
- reproducible outputs
- CRS-aware workflows
- topology-aware geometry generation
- compatible with downstream spatial analysis packages
- package interoperability across the GeoRisk Verse ecosystem

---

## Current development status

Early development version.

The package is currently under active development and the API may change.

Current development priorities include:

- stronger CRS handling
- additional geometry generation methods
- H3 support
- topology stress-testing workflows
- expanded synthetic risk scenarios
- improved documentation and vignettes
- pkgdown integration

---

## License

MIT License.