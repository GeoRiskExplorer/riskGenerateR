# =========================================================
# 01 — Concept test for rg_polygons(): grid_merge method
# =========================================================

library(sf)
library(dplyr)
library(mapview)

devtools::load_all(".")

# =========================================================
# 02 — Parameters
# =========================================================

seed <- 123
target_n <- 20
base_cell_size <- 250

set.seed(seed)

# =========================================================
# 03 — Create study area
# =========================================================

ex <- rg_bbox_example("wa_outback")

study_area <- rg_study_area(
  bbox = ex$bbox,
  crs = ex$crs,
  area_id = "wa_outback_test_bbox",
  area_name = "WA Outback Test Bounding Box"
)

# =========================================================
# 04 — Create fine grid
# =========================================================

fine_grid <- rg_grid(
  study_area,
  cell_size = base_cell_size,
  clip = TRUE
)

# Create seed points inside the study area
seed_points <- sf::st_sample(
  study_area,
  size = target_n,
  type = "random"
)

seed_points_sf <- sf::st_sf(
  seed_group = seq_len(length(seed_points)),
  geometry = seed_points
)

sf::st_crs(seed_points_sf) <- sf::st_crs(study_area)

# Assign each fine-grid cell to nearest seed point
fine_grid_centroids <- sf::st_centroid(fine_grid)

nearest_seed <- sf::st_nearest_feature(
  fine_grid_centroids,
  seed_points_sf
)

fine_grid$seed_group <- seed_points_sf$seed_group[nearest_seed]

# =========================================================
# 05 — Dissolve grid cells into irregular-ish polygons
# =========================================================

poly_units <- fine_grid |>
  dplyr::group_by(seed_group) |>
  dplyr::summarise(
    source_cell_count = dplyr::n(),
    .groups = "drop"
  ) |>
  sf::st_make_valid()

poly_units$poly_id <- sprintf("poly_%06d", seq_len(nrow(poly_units)))
poly_units$generation_method <- "seeded_grid"
poly_units$topology_type <- "clean"

poly_units <- poly_units |>
  dplyr::select(
    poly_id,
    seed_group,
    source_cell_count,
    generation_method,
    topology_type,
    geometry
  )

poly_units$area_m2 <- as.numeric(sf::st_area(poly_units))
poly_units$area_km2 <- round(poly_units$area_m2 / 1e6, 4)

# =========================================================
# 06 — QA — Basic structure
# =========================================================

cat("\n--- QA: BASIC STRUCTURE ---\n")
cat("Study area rows:", nrow(study_area), "\n")
cat("Fine grid cells:", nrow(fine_grid), "\n")
cat("Target polygon count:", target_n, "\n")
cat("Output polygon count:", nrow(poly_units), "\n")
cat("CRS EPSG:", sf::st_crs(poly_units)$epsg, "\n")

cat("\nGeometry types:\n")
print(table(as.character(sf::st_geometry_type(poly_units))))

# =========================================================
# 07 — QA — Geometry validity
# =========================================================

valid_flags <- sf::st_is_valid(poly_units)

cat("\n--- QA: GEOMETRY VALIDITY ---\n")
cat("Valid polygons:", sum(valid_flags), "\n")
cat("Invalid polygons:", sum(!valid_flags), "\n")

if (any(!valid_flags)) {
  cat("\nInvalid polygon IDs:\n")
  print(poly_units$poly_id[!valid_flags])
}

# =========================================================
# 08 — QA — Empty geometries
# =========================================================

empty_flags <- sf::st_is_empty(poly_units)

cat("\n--- QA: EMPTY GEOMETRY ---\n")
cat("Empty geometries:", sum(empty_flags), "\n")

if (any(empty_flags)) {
  print(poly_units$poly_id[empty_flags])
}

# =========================================================
# 09 — QA — Area reconciliation
# =========================================================

study_area_area_m2 <- as.numeric(sf::st_area(study_area))
poly_total_area_m2 <- sum(poly_units$area_m2, na.rm = TRUE)

area_diff_m2 <- poly_total_area_m2 - study_area_area_m2
area_diff_pct <- (area_diff_m2 / study_area_area_m2) * 100

cat("\n--- QA: AREA RECONCILIATION ---\n")
cat("Study area m2:", round(study_area_area_m2, 2), "\n")
cat("Polygon total m2:", round(poly_total_area_m2, 2), "\n")
cat("Area difference m2:", round(area_diff_m2, 6), "\n")
cat("Area difference pct:", round(area_diff_pct, 6), "\n")

# =========================================================
# 10 — QA — Overlap check
# =========================================================

overlap_matrix <- sf::st_overlaps(poly_units, sparse = TRUE)
overlap_count <- sum(lengths(overlap_matrix))

cat("\n--- QA: OVERLAPS ---\n")
cat("Overlap pair references:", overlap_count, "\n")

if (overlap_count > 0) {
  cat("Potential overlapping polygon IDs:\n")
  print(poly_units$poly_id[lengths(overlap_matrix) > 0])
}

# =========================================================
# 11 — QA — Coverage gap check
# =========================================================

poly_union <- sf::st_union(poly_units)
gap_geom <- suppressWarnings(sf::st_difference(sf::st_geometry(study_area), poly_union))

gap_area_m2 <- sum(as.numeric(sf::st_area(gap_geom)), na.rm = TRUE)

cat("\n--- QA: GAPS AGAINST STUDY AREA ---\n")
cat("Gap area m2:", round(gap_area_m2, 6), "\n")
cat("Gap area pct:", round((gap_area_m2 / study_area_area_m2) * 100, 6), "\n")

# =========================================================
# 12 — QA — Polygon size distribution
# =========================================================

cat("\n--- QA: POLYGON AREA DISTRIBUTION ---\n")
print(summary(poly_units$area_km2))

cat("\nSource cell count distribution:\n")
print(summary(poly_units$source_cell_count))

# =========================================================
# 13 — QA — Attribute completeness
# =========================================================

cat("\n--- QA: ATTRIBUTE COMPLETENESS ---\n")
qa_cols <- c(
  "poly_id",
  "seed_group",
  "source_cell_count",
  "generation_method",
  "topology_type",
  "area_m2",
  "area_km2"
)

missing_by_col <- vapply(
  poly_units[qa_cols],
  function(x) sum(is.na(x)),
  integer(1)
)

print(missing_by_col)

# =========================================================
# 14 — Visual QA
# =========================================================

mapview(
  poly_units,
  zcol = "poly_id",
  alpha.regions = 0.35,
  layer.name = "Concept Irregular Polygons"
) +
  mapview(
    study_area,
    alpha.regions = 0,
    color = "black",
    lwd = 2,
    legend = FALSE,
    layer.name = "Study Area Boundary"
  )

poly_units