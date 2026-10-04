# ==========================================================================
# riskGenerateR - attribute generation live QA
# ==========================================================================

library(riskGenerateR)
library(sf)


# 01 - Create neutral synthetic environment --------------------------------

study <- rgr_study_area()

hazard_points <- rgr_points(
  study,
  n = 25,
  inside_pct = 1,
  seed = 123
)


# 02 - Generate hazard-assessment context ----------------------------------

hazards <- rgr_add_attributes(
  hazard_points,
  type = "hazard_assessment",
  seed = 456
)


# 03 - Console inspection ---------------------------------------------------

cat(
  "\nRG_ADD_ATTRIBUTES LIVE QA\n",
  "Rows:                    ", nrow(hazards), "\n",
  "Attribute type:          ", unique(hazards$rgr_attribute_type), "\n",
  "Hazard categories:       ", length(unique(hazards$hazard_category)), "\n",
  "Hazard types:            ", length(unique(hazards$hazard_type)), "\n",
  "Missing hazard types:    ", sum(is.na(hazards$hazard_type)), "\n",
  "Missing consequence:     ", sum(is.na(hazards$consequence)), "\n",
  "Missing likelihood:      ", sum(is.na(hazards$likelihood)), "\n",
  "Geometry valid:          ", all(sf::st_is_valid(hazards)), "\n",
  "Synthetic XY CRS missing:", is.na(sf::st_crs(hazards)), "\n",
  sep = ""
)


# 04 - Inspect generated records -------------------------------------------

print(
  sf::st_drop_geometry(hazards)[
    1:10,
    c(
      "assessment_id",
      "hazard_category",
      "hazard_type",
      "exposed_group",
      "existing_control",
      "control_effectiveness",
      "consequence",
      "likelihood",
      "assessment_status"
    )
  ]
)


# 05 - Inspect distributions ------------------------------------------------

print(
  table(hazards$hazard_category)
)

print(
  table(hazards$control_effectiveness)
)

print(
  table(hazards$consequence)
)

print(
  table(hazards$likelihood)
)


# 06 - Visual inspection ----------------------------------------------------

plot(
  sf::st_geometry(study),
  main = "Synthetic hazard locations"
)

plot(
  sf::st_geometry(hazards),
  add = TRUE,
  pch = 19
)
