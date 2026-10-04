# 01 - Map example: study area, points, and hex tessellation

library(sf)
library(mapview)
library(riskGenerateR)


# 01 - Build study area -----------------------------------------------------

ex <- rgr_bbox_example(
  "wa_outback"
)

study_area <- rgr_study_area(
  bbox = ex$bbox,
  crs = ex$crs,
  area_id = "wa_outback_hex_example",
  area_name = "WA Outback Hex Example"
)


# 02 - Generate spatial features --------------------------------------------

points <- rgr_points(
  study_area,
  n = 100,
  inside_pct = 0.9,
  outside_distance = 1000,
  seed = 123
)

hex <- rgr_tessellate(
  study_area,
  cell_size = 500,
  shape = "hex",
  clip = FALSE
)


# 03 - Add contextual attributes --------------------------------------------

incidents <- rgr_add_attributes(
  points,
  type = "incident",
  seed = 123
)

hex_attributes <- rgr_add_attributes(
  hex,
  type = "generic",
  seed = 456
)


# 04 - Visual QA ------------------------------------------------------------

mapview(
  hex,
  color = "grey40",
  alpha.regions = 0,
  lwd = 1,
  legend = FALSE,
  layer.name = "Hex Tessellation"
) +
  mapview(
    study_area,
    alpha.regions = 0.1,
    legend = FALSE,
    layer.name = "Study Area"
  ) +
  mapview(
    incidents,
    zcol = "event_type",
    layer.name = "Synthetic Incidents"
  )


# 05 - Console QA -----------------------------------------------------------

cat(
  "\n--- HEX EXAMPLE QA ---\n"
)

cat(
  "Total hex cells:",
  nrow(hex_attributes),
  "\n"
)

cat(
  "Total points:",
  nrow(incidents),
  "\n"
)

cat(
  "Total event count:",
  sum(incidents$event_count),
  "\n"
)

print(
  table(incidents$event_type)
)

rgr_summary(
  incidents
)

rgr_summary(
  hex_attributes
)
