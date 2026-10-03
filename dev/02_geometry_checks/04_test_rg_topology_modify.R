# =========================================================
# 01 — Visual QA for topology modification
# =========================================================

library(sf)
library(mapview)

devtools::load_all(".")

# =========================================================
# 02 — Generate clean irregular polygons
# =========================================================

x <- rg_scenario(
  scenario = "basic",
  seed = 123
)

clean_polys <- rg_polygons(
  study_area = x$study_area,
  target_n = 20,
  cell_size = 250,
  seed = 123
)

bad_polys <- rg_topology_modify(
  clean_polys,
  mode = "overlap",
  pct = 0.2,
  distance = 75,
  seed = 123
)

# =========================================================
# 03 — QA
# =========================================================

cat("\n--- TOPOLOGY MODIFY QA ---\n")
cat("Clean polygons:", nrow(clean_polys), "\n")
cat("Modified polygons:", sum(bad_polys$topology_modified), "\n")

clean_overlap <- sum(lengths(sf::st_overlaps(clean_polys, sparse = TRUE)))
bad_overlap <- sum(lengths(sf::st_overlaps(bad_polys, sparse = TRUE)))

cat("Clean overlap pair references:", clean_overlap, "\n")
cat("Bad overlap pair references:", bad_overlap, "\n")

cat("\nModified polygon IDs:\n")
print(bad_polys$poly_id[bad_polys$topology_modified])

# =========================================================
# 04 — Visual QA
# =========================================================

mapview(
  clean_polys,
  zcol = "poly_id",
  alpha.regions = 0.25,
  layer.name = "Clean Polygons"
) +
  mapview(
    bad_polys[bad_polys$topology_modified, ],
    color = "red",
    alpha.regions = 0.4,
    layer.name = "Modified Overlap Polygons"
  ) +
  mapview(
    x$study_area,
    alpha.regions = 0,
    color = "black",
    lwd = 2,
    legend = FALSE,
    layer.name = "Study Area"
  )

bad_polys