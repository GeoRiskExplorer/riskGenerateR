# riskGenerateR

<!-- badges: start -->

![Lifecycle](https://img.shields.io/badge/lifecycle-early_development-orange.svg)

<!-- badges: end -->

`riskGenerateR` generates reproducible synthetic spatial and contextual data
for risk-related development, testing, demonstrations and quality assurance.

It provides spatial features together with plausible contextual attributes
representing hazards, incidents, risk-register records, observations and
generic analytical variables.

`riskGenerateR` generates the analytical fixture. It does not calculate or
infer risk.

---

## GeoRisk Verse

`riskGenerateR` is part of the broader GeoRisk Verse ecosystem.

Its responsibility is synthetic data generation for:

- package development and testing
- reproducible examples
- QA workflows
- demonstrations
- application prototypes
- spatial workflow validation
- risk-related analytical development

Spatial analysis, aggregation, modelling and decision-support workflows are
intentionally handled downstream.

---

## Current features

`riskGenerateR` currently supports:

- study area generation
- example bounding boxes
- random point generation
- controlled inside/outside point generation
- square and hexagonal tessellations
- irregular polygon generation
- contextual attribute generation
- complete synthetic scenarios
- spatial QA and descriptive summaries
- CRS-aware geometry workflows
- explicit topology stress testing

Contextual attribute generators currently include:

- hazard assessments
- incidents
- risk-register records
- observations
- generic analytical variables

---

## Installation

```r
# install.packages("remotes")

remotes::install_github(
  "GeoRiskExplorer/riskGenerateR",
  build_vignettes = TRUE
)
```

---

## Quick example

```r
library(riskGenerateR)

x <- rgr_scenario(
  scenario = "basic",
  seed = 123
)

names(x)

rgr_summary(
  x$incidents
)

rgr_summary(
  x$grid_attributes
)
```

Individual components can also be generated directly:

```r
ex <- rgr_bbox_example(
  "wa_outback"
)

study_area <- rgr_study_area(
  bbox = ex$bbox,
  crs = ex$crs
)

points <- rgr_points(
  study_area,
  n = 100,
  seed = 123
)

incidents <- rgr_add_attributes(
  points,
  type = "incident",
  seed = 456
)

rgr_summary(
  incidents
)
```

---

## Package boundary

`riskGenerateR` generates synthetic spatial and contextual data representing
plausible conditions for risk-related analysis.

The package does **not** calculate analytical risk outputs.

Operations such as:

- spatial aggregation
- rates
- expected counts
- standardised ratios
- risk scores or classes
- statistical modelling
- H3 processing
- database integration
- decision-support outputs

belong in downstream analytical workflows or companion packages.

This separation keeps synthetic data generation reproducible, lightweight and
reusable across different analytical approaches.

---

## Current exported functions

### Study areas and examples

- `rgr_bbox_example()`
- `rgr_study_area()`
- `rgr_example_data()`

### Geometry generation

- `rgr_points()`
- `rgr_tessellate()`
- `rgr_polygons()`

### Contextual attributes

- `rgr_add_attributes()`

### Scenario generation

- `rgr_scenario()`

### QA and inspection

- `rgr_summary()`

### Topology stress testing

- `rgr_topology_modify()`

---

## Attribute contexts

`rgr_add_attributes()` separates geometry generation from semantic context.

For example:

```r
incidents <- rgr_add_attributes(
  points,
  type = "incident",
  seed = 123
)

observations <- rgr_add_attributes(
  points,
  type = "observation",
  seed = 123
)

hazards <- rgr_add_attributes(
  points,
  type = "hazard_assessment",
  seed = 123
)
```

The same spatial features can therefore represent different synthetic
risk-related contexts without embedding analytical conclusions in the data
generator.

---

## Design principles

`riskGenerateR` is designed around:

- generation rather than analysis
- reproducible outputs
- coherent synthetic attributes
- CRS-aware spatial workflows
- geometry and semantic separation
- valid geometry by default
- explicit stress-test behaviour
- lightweight dependencies
- interoperability with downstream packages

Synthetic XY data without a defined CRS are also supported as a first-class
development and testing workflow.

---

## Development status

`riskGenerateR` is under active development.

The current development focus is stabilising the public API, documentation,
examples and vignettes for the first public release.

Functionality beyond synthetic data generation is intentionally kept outside
the package.

---

## License

MIT License.
