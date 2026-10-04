# 01 - Basic bundled scenario

library(mapview)
library(riskGenerateR)


# 01 - Generate scenario ----------------------------------------------------

x <- rgr_scenario(
  scenario = "basic",
  bbox_name = "wa_outback",
  n_points = 100,
  inside_pct = 0.9,
  cell_size = 500,
  seed = 123
)


# 02 - Inspect scenario -----------------------------------------------------

names(
  x
)

rgr_summary(
  x$incidents
)

rgr_summary(
  x$grid_attributes
)

rgr_summary(
  x$hex_attributes
)


# 03 - Visual QA ------------------------------------------------------------

mapview(
  x$hex,
  color = "grey40",
  alpha.regions = 0,
  lwd = 1,
  legend = FALSE,
  layer.name = "Hex Tessellation"
) +
  mapview(
    x$study_area,
    alpha.regions = 0.1,
    legend = FALSE,
    layer.name = "Study Area"
  ) +
  mapview(
    x$incidents,
    zcol = "event_type",
    layer.name = "Synthetic Incidents"
  )


# 04 - Console QA -----------------------------------------------------------

cat(
  "\n--- BASIC SCENARIO QA ---\n"
)

cat(
  "Scenario:",
  x$scenario,
  "\n"
)

cat(
  "Points:",
  nrow(x$points),
  "\n"
)

cat(
  "Incidents:",
  nrow(x$incidents),
  "\n"
)

cat(
  "Event count:",
  sum(x$incidents$event_count),
  "\n"
)

cat(
  "Square cells:",
  nrow(x$grid),
  "\n"
)

cat(
  "Hex cells:",
  nrow(x$hex),
  "\n"
)
