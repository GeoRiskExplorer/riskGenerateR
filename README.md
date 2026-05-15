# riskGenerateR

<!-- badges: start -->
<!-- badges: end -->

`riskGenerateR` generates synthetic spatial risk datasets for testing,
development, demonstrations, QA workflows, reproducible examples,
and spatial risk package development.

The package is designed as part of the GeoRisk Verse ecosystem and
currently focuses on generating synthetic `sf` spatial objects.

## Current features

- Study area generation
- Example bounding boxes
- Random point generation
- Square grids
- Hex tessellations
- Irregular polygon generation
- Synthetic risk attributes
- Synthetic exposure and event counts
- Scenario generators
- Spatial QA summaries

## Installation

``` r
# development version
# remotes::install_github("GeoRiskExplorer/riskGenerateR")
```

## Example

``` r
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

## Package philosophy

`riskGenerateR` focuses on generating synthetic spatial datasets only.

Spatial analysis workflows such as:

- spatial joins
- rates
- SMR
- choropleths
- aggregation
- DuckDB integration

are intended for companion packages within the GeoRisk Verse ecosystem.

## Current object support

Current outputs are primarily:

- `sf`

Future support may include:

- H3
- WKT/WKB
- terra
- raster
- non-spatial join tables

## Development status

Early development version.