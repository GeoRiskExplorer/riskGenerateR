# 01 — Map example bbox, points, and hexes

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

hex <- rg_hex(
  study_area,
  cell_size = 500,
  clip = FALSE
)

hex_risk <- rg_add_risk_attributes(
  hex,
  seed = 456
)

mapview(
  hex,
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

cat("\n--- HEX RISK QA ---\n")
cat("Total hex cells:", nrow(hex_risk), "\n")
cat("Total aggregated events:", sum(hex_risk$event_count), "\n")
cat("Total exposure:", sum(hex_risk$exposure_count), "\n")
print(table(hex_risk$risk_class))

# hex_risk