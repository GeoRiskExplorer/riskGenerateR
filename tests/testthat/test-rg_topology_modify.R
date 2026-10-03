test_that("rg_topology_modify creates overlapping polygons", {
  ex <- rg_bbox_example("wa_outback")

  study_area <- rg_study_area(
    bbox = ex$bbox,
    crs = ex$crs
  )

  polys <- rg_polygons(
    study_area,
    target_n = 20,
    cell_size = 250,
    seed = 123
  )

  bad <- rg_topology_modify(
    polys,
    mode = "overlap",
    pct = 0.2,
    distance = 50,
    seed = 123
  )

  overlap_matrix <- sf::st_overlaps(bad, sparse = TRUE)
  overlap_count <- sum(lengths(overlap_matrix))

  expect_s3_class(bad, "sf")
  expect_equal(nrow(bad), nrow(polys))
  expect_true("topology_modified" %in% names(bad))
  expect_true("topology_distance" %in% names(bad))
  expect_true(sum(bad$topology_modified) > 0)
  expect_true(overlap_count > 0)
})

test_that("rg_topology_modify rejects invalid inputs", {
  ex <- rg_bbox_example("wa_outback")

  study_area <- rg_study_area(
    bbox = ex$bbox,
    crs = ex$crs
  )

  pts <- rg_points(
    study_area,
    n = 10,
    inside_pct = 1,
    seed = 123
  )

  expect_error(
    rg_topology_modify(pts),
    "polygon geometries"
  )

  expect_error(
    rg_topology_modify(study_area, pct = 0),
    "pct must be greater than 0"
  )

  expect_error(
    rg_topology_modify(study_area, distance = 0),
    "distance must be greater than 0"
  )

  expect_error(
    rg_topology_modify(study_area, mode = "gap"),
    "Currently only mode"
  )
})