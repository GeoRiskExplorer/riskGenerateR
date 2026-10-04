# 01 - Incident live QA

library(riskGenerateR)
library(sf)


# 02 - Create synthetic incident locations ---------------------------------

study <- rgr_study_area()

incident_points <- rgr_points(
  study,
  n = 25,
  inside_pct = 1,
  seed = 123
)


# 03 - Add incident attributes ---------------------------------------------

incidents <- rgr_add_attributes(
  incident_points,
  type = "incident",
  seed = 456
)


# 04 - Console QA -----------------------------------------------------------

cat("\n")
cat("RGR_ADD_ATTRIBUTES - INCIDENT LIVE QA\n")
cat("Rows:                    ", nrow(incidents), "\n", sep = "")
cat("Attribute type:          ", unique(incidents$rgr_attribute_type), "\n", sep = "")
cat("Event types:             ", length(unique(incidents$event_type)), "\n", sep = "")
cat("Hazard categories:       ", length(unique(incidents$hazard_category)), "\n", sep = "")
cat("Missing mechanisms:      ", sum(is.na(incidents$mechanism)), "\n", sep = "")
cat("Missing activities:      ", sum(is.na(incidents$activity)), "\n", sep = "")
cat("Missing consequence:     ", sum(is.na(incidents$consequence)), "\n", sep = "")
cat("Missing dates:           ", sum(is.na(incidents$event_date)), "\n", sep = "")
cat("Geometry valid:          ", all(sf::st_is_valid(incidents)), "\n", sep = "")
cat("Synthetic XY CRS missing:", is.na(sf::st_crs(incidents)), "\n", sep = "")


# 05 - Inspect records ------------------------------------------------------

print(
  sf::st_drop_geometry(incidents)[
    1:10,
    c(
      "event_id",
      "event_type",
      "hazard_category",
      "mechanism",
      "activity",
      "consequence",
      "event_date",
      "event_hour"
    )
  ]
)

cat("\nEVENT TYPE\n")
print(table(incidents$event_type))

cat("\nCONSEQUENCE\n")
print(table(incidents$consequence))

cat("\nACTIVITY\n")
print(table(incidents$activity))


# 06 - Visual inspection ----------------------------------------------------

plot(
  sf::st_geometry(study),
  main = "Synthetic incident locations"
)

plot(
  sf::st_geometry(incidents),
  add = TRUE,
  pch = 19
)
