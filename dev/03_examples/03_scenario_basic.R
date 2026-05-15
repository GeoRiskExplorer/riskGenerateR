# 01 — Basic bundled scenario

library(mapview)
library(riskGenerateR)

x <- rg_scenario(
  scenario = "basic",
  bbox_name = "wa_outback",
  n_points = 100,
  inside_pct = 0.9,
  cell_size = 500,
  seed = 123
)

hex <- x$hex
points <- x$points_risk
study_area <- x$study_area

mapview(
  hex,
  color = "grey40",
  alpha.regions = 0,
  lwd = 1,
  legend = FALSE,
  layer.name = "Synthetic Hex Grid"
) +
  mapview(
    study_area,
    alpha.regions = 0.1,
    legend = FALSE,
    layer.name = "Study Area"
  ) +
  mapview(
    points,
    zcol = "risk_class",
    layer.name = "Synthetic Risk Events"
  )

cat("\n--- SCENARIO QA ---\n")
cat("Scenario:", x$scenario, "\n")
cat("Points:", nrow(x$points_risk), "\n")
cat("Point event count:", sum(x$points_risk$event_count), "\n")
cat("Grid cells:", nrow(x$grid_risk), "\n")
cat("Grid events:", sum(x$grid_risk$event_count), "\n")
cat("Hex cells:", nrow(x$hex_risk), "\n")
cat("Hex events:", sum(x$hex_risk$event_count), "\n")