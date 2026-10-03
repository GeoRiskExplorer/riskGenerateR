# =============================================================================
# riskGenerateR — rg_tessellate() live QA
# =============================================================================


# 01 — Setup -----------------------------------------------------------------

devtools::load_all()

library(sf)


# 02 — Synthetic study area --------------------------------------------------

study <- rg_study_area(
  bbox = c(
    xmin = 0,
    ymin = 0,
    xmax = 5000,
    ymax = 5000
  )
)


# 03 — Square tessellation ---------------------------------------------------

square <- rg_tessellate(
  study,
  cell_size = 500,
  shape = "square"
)

print(square)
print(table(square$shape))

plot(
  sf::st_geometry(square),
  main = "Square tessellation"
)

plot(
  sf::st_geometry(study),
  add = TRUE,
  lwd = 3
)


# 04 — Hexagonal tessellation ------------------------------------------------

hex <- rg_tessellate(
  study,
  cell_size = 500,
  shape = "hex"
)

print(hex)
print(table(hex$shape))

plot(
  sf::st_geometry(hex),
  main = "Hexagonal tessellation"
)

plot(
  sf::st_geometry(study),
  add = TRUE,
  lwd = 3
)


# 05 — Create irregular test boundary ----------------------------------------

study_irregular_geom <- sf::st_buffer(
  sf::st_sfc(
    sf::st_point(
      c(2500, 2500)
    )
  ),
  dist = 2200
)

study_irregular <- sf::st_sf(
  area_id = "irregular_001",
  geometry = study_irregular_geom
)


# 06 — Clipped square tessellation ------------------------------------------

square_clipped <- rg_tessellate(
  study_irregular,
  cell_size = 500,
  shape = "square",
  clip = TRUE
)

plot(
  sf::st_geometry(square_clipped),
  main = "Clipped square tessellation"
)

plot(
  sf::st_geometry(study_irregular),
  add = TRUE,
  lwd = 3
)


# 07 — Clipped hexagonal tessellation ---------------------------------------

hex_clipped <- rg_tessellate(
  study_irregular,
  cell_size = 500,
  shape = "hex",
  clip = TRUE
)

plot(
  sf::st_geometry(hex_clipped),
  main = "Clipped hexagonal tessellation"
)

plot(
  sf::st_geometry(study_irregular),
  add = TRUE,
  lwd = 3
)


# 08 — Projected CRS ---------------------------------------------------------

study_projected <- rg_study_area(
  bbox = c(
    xmin = 300000,
    ymin = 5800000,
    xmax = 305000,
    ymax = 5805000
  ),
  crs = 7855
)

projected_hex <- rg_tessellate(
  study_projected,
  cell_size = 500,
  shape = "hex"
)


# 09 — Live QA summary -------------------------------------------------------

cat(
  "\n",
  "============================================================\n",
  "RG_TESSELLATE LIVE QA\n",
  "============================================================\n",
  "Square cells:             ",
  nrow(square),
  "\n",
  "Hex cells:                ",
  nrow(hex),
  "\n",
  "Clipped square cells:     ",
  nrow(square_clipped),
  "\n",
  "Clipped hex cells:        ",
  nrow(hex_clipped),
  "\n",
  "Synthetic square CRS NA:  ",
  is.na(sf::st_crs(square)),
  "\n",
  "Synthetic hex CRS NA:     ",
  is.na(sf::st_crs(hex)),
  "\n",
  "Projected output EPSG:    ",
  sf::st_crs(projected_hex)$epsg,
  "\n",
  "Square geometry valid:    ",
  all(sf::st_is_valid(square)),
  "\n",
  "Hex geometry valid:       ",
  all(sf::st_is_valid(hex)),
  "\n",
  "Clipped square valid:     ",
  all(sf::st_is_valid(square_clipped)),
  "\n",
  "Clipped hex valid:        ",
  all(sf::st_is_valid(hex_clipped)),
  "\n",
  "============================================================\n",
  sep = ""
)