# =============================================================================
# riskGenerateR — rg_study_area() live QA
# =============================================================================
#
# Purpose:
#   Human functional and visual inspection of study-area generation.
#
# This script complements testthat. It is not an automated regression test.
#
# =============================================================================


# 01 — Setup -----------------------------------------------------------------

devtools::load_all()

library(sf)


# 02 — Default synthetic XY study area ---------------------------------------

study_xy <- rg_study_area()

print(study_xy)
print(sf::st_crs(study_xy))
print(sf::st_bbox(study_xy))

plot(
  sf::st_geometry(study_xy),
  main = "Default synthetic XY study area"
)


# 03 — Custom synthetic XY extent --------------------------------------------

study_custom <- rg_study_area(
  bbox = c(
    xmin = -2000,
    ymin = -1000,
    xmax = 8000,
    ymax = 6000
  ),
  area_name = "Custom Synthetic Study Area"
)

print(study_custom)
print(sf::st_crs(study_custom))
print(sf::st_bbox(study_custom))

plot(
  sf::st_geometry(study_custom),
  main = "Custom synthetic XY extent"
)


# 04 — sf bbox with inherited CRS --------------------------------------------

existing_bbox <- sf::st_bbox(
  c(
    xmin = 300000,
    ymin = 5800000,
    xmax = 305000,
    ymax = 5805000
  ),
  crs = sf::st_crs(7855)
)

study_bbox <- rg_study_area(
  bbox = existing_bbox,
  area_name = "BBox Study Area"
)

print(study_bbox)
print(sf::st_crs(study_bbox))
print(sf::st_bbox(study_bbox))

plot(
  sf::st_geometry(study_bbox),
  main = "sf bbox input — inherited CRS"
)


# 05 — Numeric bbox with explicit CRS ----------------------------------------

study_crs <- rg_study_area(
  bbox = c(
    xmin = 300000,
    ymin = 5800000,
    xmax = 305000,
    ymax = 5805000
  ),
  crs = 7855,
  area_name = "Projected Study Area"
)

print(study_crs)
print(sf::st_crs(study_crs))
print(sf::st_bbox(study_crs))

plot(
  sf::st_geometry(study_crs),
  main = "Numeric bbox — explicit CRS"
)


# 06 — Visual QA summary ------------------------------------------------------

cat(
  "\n",
  "============================================================\n",
  "RG_STUDY_AREA LIVE QA\n",
  "============================================================\n",
  "Default XY CRS missing: ",
  is.na(sf::st_crs(study_xy)),
  "\n",
  "Custom XY CRS missing:  ",
  is.na(sf::st_crs(study_custom)),
  "\n",
  "BBox inherited EPSG:     ",
  sf::st_crs(study_bbox)$epsg,
  "\n",
  "Explicit EPSG:           ",
  sf::st_crs(study_crs)$epsg,
  "\n",
  "============================================================\n",
  sep = ""
)