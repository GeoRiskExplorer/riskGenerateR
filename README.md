# riskGenerateR

<!-- badges: start -->
<!-- badges: end -->

`riskGenerateR` generates synthetic spatial risk datasets for testing,
development, demonstrations, QA workflows, reproducible examples,
dashboards, and spatial risk package development.

The package is designed as part of the GeoRisk Verse ecosystem and
currently focuses on generating synthetic `sf` spatial objects.

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

## Example

```r
library(riskGenerateR)
library(mapview)

x <- rg_scenario(
  scenario = "basic",
  seed = 123
)

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

are intentionally handled by companion packages within the
GeoRisk Verse ecosystem.

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

## Design principles

- generation-focused
- lightweight dependencies
- reproducible outputs
- robust CRS handling
- topology-aware workflows
- compatible with downstream spatial analysis packages

---

## Development status

Early development version.

The package is currently under active development and the API may change.