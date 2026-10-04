# =============================================================================
# riskGenerateR - rgr_points() live QA
# =============================================================================
#
# Purpose:
#   Human functional and visual inspection of synthetic point generation.
#
# This script complements testthat. It is not an automated regression test.
#
# =============================================================================


# 01 - Setup -----------------------------------------------------------------

devtools::load_all()

library(sf)


# 02 - Create synthetic XY study area ----------------------------------------

study_xy <- rgr_study_area()

plot(
  sf::st_geometry(study_xy),
  main = "Synthetic XY study area"
)


# 03 - Default point generation ----------------------------------------------

pts_default <- rgr_points(
  study_xy,
  n = 100,
  seed = 123
)

print(pts_default)
print(sf::st_crs(pts_default))
print(table(pts_default$inside_flag))

plot(
  sf::st_geometry(study_xy),
  main = "Default random points - synthetic XY"
)

plot(
  sf::st_geometry(pts_default),
  add = TRUE,
  pch = 16
)


# 04 - Mixed inside/outside generation ---------------------------------------

pts_mixed <- rgr_points(
  study_xy,
  n = 100,
  inside_pct = 0.8,
  outside_distance = 1000,
  seed = 123
)

print(table(pts_mixed$inside_flag))

display_extent <- sf::st_buffer(
  study_xy,
  dist = 1000
)

plot(
  sf::st_geometry(display_extent),
  border = "grey",
  main = "Random points - 80% inside / 20% outside"
)

plot(
  sf::st_geometry(study_xy),
  add = TRUE,
  lwd = 2
)

plot(
  sf::st_geometry(pts_mixed),
  add = TRUE,
  pch = 16
)


# 05 - Reproducibility --------------------------------------------------------

pts_a <- rgr_points(
  study_xy,
  n = 50,
  seed = 999
)

pts_b <- rgr_points(
  study_xy,
  n = 50,
  seed = 999
)

reproducible <- identical(
  sf::st_coordinates(pts_a),
  sf::st_coordinates(pts_b)
)

print(reproducible)


# 06 - Projected CRS ----------------------------------------------------------

study_projected <- rgr_study_area(
  bbox = c(
    xmin = 300000,
    ymin = 5800000,
    xmax = 305000,
    ymax = 5805000
  ),
  crs = 7855,
  area_name = "Projected Study Area"
)

pts_projected <- rgr_points(
  study_projected,
  n = 100,
  seed = 123
)

print(sf::st_crs(pts_projected))
print(table(pts_projected$inside_flag))

plot(
  sf::st_geometry(study_projected),
  main = "Random points - projected CRS"
)

plot(
  sf::st_geometry(pts_projected),
  add = TRUE,
  pch = 16
)


# 07 - Visual QA summary ------------------------------------------------------

cat(
  "\n",
  "============================================================\n",
  "rgr_POINTS LIVE QA\n",
  "============================================================\n",
  "Default points:           ",
  nrow(pts_default),
  "\n",
  "Default inside:           ",
  sum(pts_default$inside_flag),
  "\n",
  "Default outside:          ",
  sum(!pts_default$inside_flag),
  "\n",
  "Mixed inside:             ",
  sum(pts_mixed$inside_flag),
  "\n",
  "Mixed outside:            ",
  sum(!pts_mixed$inside_flag),
  "\n",
  "Seed reproducible:        ",
  reproducible,
  "\n",
  "Synthetic XY CRS missing: ",
  is.na(sf::st_crs(pts_default)),
  "\n",
  "Projected output EPSG:    ",
  sf::st_crs(pts_projected)$epsg,
  "\n",
  "============================================================\n",
  sep = ""
)
