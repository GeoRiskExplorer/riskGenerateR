# =========================================================
# 01 — Test rg_polygons() using real irregular Cottesloe LGA
# =========================================================

library(sf)
library(dplyr)
library(mapview)
library(qs2)

devtools::load_all(".")

sf::sf_use_s2(FALSE)

# =========================================================
# 02 — Paths and parameters
# =========================================================

cottesloe_path <- "dev/02_geometry_checks/data_lga_test_polygons/lga_2025_cottesloe_gda2020.qs2"

target_n <- 20
cell_size <- 100
seed <- 123

# =========================================================
# 03 — Load Cottesloe test polygon
# =========================================================

cottesloe <- qs2::qs_read(cottesloe_path)

cottesloe <- cottesloe |>
  sf::st_make_valid()

cottesloe <- suppressWarnings(
  sf::st_buffer(cottesloe, 0)
)

cottesloe <- suppressWarnings(
  sf::st_collection_extract(cottesloe, "POLYGON")
)

cottesloe <- cottesloe[!sf::st_is_empty(cottesloe), ]

cat("\n--- INPUT COTTESLOE QA ---\n")
cat("Rows:", nrow(cottesloe), "\n")
cat("CRS EPSG:", sf::st_crs(cottesloe)$epsg, "\n")
cat("Valid:", all(sf::st_is_valid(cottesloe)), "\n")
cat("Empty:", any(sf::st_is_empty(cottesloe)), "\n")
cat("Area km2:", round(sum(as.numeric(sf::st_area(cottesloe))) / 1e6, 4), "\n")

# =========================================================
# 04 — Generate irregular polygons
# =========================================================

polys <- rg_polygons(
  study_area = cottesloe,
  target_n = 20,
  cell_size = 100,
  method = "seeded_grid",
  clip = TRUE,
  seed = 123,
  processing_crs = 7850
)

polys_risk <- rg_add_risk_attributes(
  polys,
  seed = seed
)

# =========================================================
# 05 — Package summaries
# =========================================================

cat("\n--- RG SUMMARY: POLYS ---\n")
rg_summary(polys)

cat("\n--- RG SUMMARY: POLYS RISK ---\n")
rg_summary(polys_risk)

# =========================================================
# 06 — QA — Structure
# =========================================================

cat("\n--- QA: STRUCTURE ---\n")
cat("Target polygon count:", target_n, "\n")
cat("Output polygon count:", nrow(polys), "\n")
cat("Geometry types:\n")
print(table(as.character(sf::st_geometry_type(polys))))

# =========================================================
# 07 — QA — Validity and empties
# =========================================================

valid_flags <- sf::st_is_valid(polys)
empty_flags <- sf::st_is_empty(polys)

cat("\n--- QA: GEOMETRY VALIDITY ---\n")
cat("Valid polygons:", sum(valid_flags), "\n")
cat("Invalid polygons:", sum(!valid_flags), "\n")
cat("Empty geometries:", sum(empty_flags), "\n")

if (any(!valid_flags)) {
  print(polys$poly_id[!valid_flags])
}

if (any(empty_flags)) {
  print(polys$poly_id[empty_flags])
}

# =========================================================
# 08 — QA — Area reconciliation
# =========================================================

cottesloe_area_m2 <- sum(as.numeric(sf::st_area(cottesloe)))
poly_area_m2 <- sum(as.numeric(sf::st_area(polys)))

area_diff_m2 <- poly_area_m2 - cottesloe_area_m2
area_diff_pct <- (area_diff_m2 / cottesloe_area_m2) * 100

cat("\n--- QA: AREA RECONCILIATION ---\n")
cat("Cottesloe area m2:", round(cottesloe_area_m2, 2), "\n")
cat("Generated polygon total m2:", round(poly_area_m2, 2), "\n")
cat("Area difference m2:", round(area_diff_m2, 6), "\n")
cat("Area difference pct:", round(area_diff_pct, 6), "\n")

# =========================================================
# 09 — QA — Overlap check
# =========================================================

overlap_matrix <- sf::st_overlaps(polys, sparse = TRUE)
overlap_count <- sum(lengths(overlap_matrix))

cat("\n--- QA: OVERLAPS ---\n")
cat("Overlap pair references:", overlap_count, "\n")

if (overlap_count > 0) {
  print(polys$poly_id[lengths(overlap_matrix) > 0])
}

# =========================================================
# 10 — QA — Gap check against Cottesloe
# =========================================================

poly_union <- sf::st_union(polys)

gap_geom <- suppressWarnings(
  sf::st_difference(
    sf::st_geometry(sf::st_union(cottesloe)),
    poly_union
  )
)

gap_area_m2 <- sum(as.numeric(sf::st_area(gap_geom)), na.rm = TRUE)
gap_area_pct <- (gap_area_m2 / cottesloe_area_m2) * 100

cat("\n--- QA: GAPS AGAINST COTTESLOE ---\n")
cat("Gap area m2:", round(gap_area_m2, 6), "\n")
cat("Gap area pct:", round(gap_area_pct, 6), "\n")

# =========================================================
# 11 — QA — Outside coverage check
# =========================================================

outside_geom <- suppressWarnings(
  sf::st_difference(
    sf::st_geometry(poly_union),
    sf::st_geometry(sf::st_union(cottesloe))
  )
)

outside_area_m2 <- sum(as.numeric(sf::st_area(outside_geom)), na.rm = TRUE)
outside_area_pct <- (outside_area_m2 / cottesloe_area_m2) * 100

cat("\n--- QA: OUTSIDE COTTESLOE ---\n")
cat("Outside area m2:", round(outside_area_m2, 6), "\n")
cat("Outside area pct:", round(outside_area_pct, 6), "\n")

# =========================================================
# 12 — QA — Area and source cell distributions
# =========================================================

cat("\n--- QA: POLYGON AREA DISTRIBUTION ---\n")
print(summary(polys$area_km2))

cat("\n--- QA: SOURCE CELL COUNT DISTRIBUTION ---\n")
print(summary(polys$source_cell_count))

# =========================================================
# 13 — QA — Risk attributes
# =========================================================

cat("\n--- QA: RISK ATTRIBUTES ---\n")
cat("Total event count:", sum(polys_risk$event_count), "\n")
cat("Total exposure count:", sum(polys_risk$exposure_count), "\n")

cat("\nRisk class count:\n")
print(table(polys_risk$risk_class, useNA = "ifany"))

cat("\nDominant hazard type count:\n")
print(table(polys_risk$dominant_hazard_type, useNA = "ifany"))

# =========================================================
# 14 — Generate synthetic event points
# =========================================================

points <- rg_points(
  study_area = cottesloe,
  n = 150,
  inside_pct = 0.95,
  outside_distance = 200,
  seed = seed,
  processing_crs = 7850
)

points_risk <- rg_add_risk_attributes(
  points,
  seed = seed
)

# =========================================================
# 15 — Visual QA
# =========================================================

mapview(
  polys_risk,
  zcol = "risk_class",
  alpha.regions = 0.45,
  layer.name = "Generated Cottesloe Risk Polygons"
) +
  mapview(
    points_risk,
    zcol = "risk_class",
    cex = 4,
    alpha = 0.9,
    layer.name = "Synthetic Risk Events"
  ) +
  mapview(
    cottesloe,
    alpha.regions = 0,
    color = "black",
    lwd = 2,
    legend = FALSE,
    layer.name = "Cottesloe Boundary"
  )

polys
polys_risk
points_risk