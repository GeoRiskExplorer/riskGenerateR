# 01 — Map example bbox, points, grid, and risk attributes

library(sf)
library(mapview)
library(riskGenerateR)

ex <- rg_bbox_example("wa_outback")

study_area <- rg_study_area(
  bbox = ex$bbox,
  crs = ex$crs,
  area_id = "wa_outback_test_bbox",
  area_name = "WA Outback Test Bounding Box"
)

points <- rg_points(
  study_area,
  n = 100,
  inside_pct = 0.9,
  outside_distance = 1000,
  seed = 123
)

points_risk <- rg_add_risk_attributes(
  points,
  seed = 123
)

grid <- rg_grid(
  study_area,
  cell_size = 500,
  clip = TRUE
)

grid_risk <- rg_add_risk_attributes(
  grid,
  seed = 123
)

mapview(
  grid,
  color = "grey40",
  alpha.regions = 0,
  lwd = 1,
  legend = FALSE
) +
  mapview(
    study_area,
    alpha.regions = 0.1,
    legend = FALSE
  ) +
  mapview(
    points_risk,
    zcol = "risk_class"
  )

# points_risk
# grid_risk

points_risk
grid_risk

# =========================================================
# QA — Point Risk Summary
# =========================================================

cat("\n--- POINT RISK QA ---\n")

cat("Total points:", nrow(points_risk), "\n")
cat("Total event count:", sum(points_risk$event_count), "\n")

print(table(points_risk$risk_class))

# =========================================================
# QA — Grid Risk Summary
# =========================================================

cat("\n--- GRID RISK QA ---\n")

cat("Total grid cells:", nrow(grid_risk), "\n")
cat("Total aggregated events:", sum(grid_risk$event_count), "\n")
cat("Total exposure:", sum(grid_risk$exposure_count), "\n")

print(table(grid_risk$risk_class))