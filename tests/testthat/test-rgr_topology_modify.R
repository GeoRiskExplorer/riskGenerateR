test_that("rgr_topology_modify creates overlapping polygons", {
  ex <- rgr_bbox_example("wa_outback")

  study_area <- rgr_study_area(
    bbox = ex$bbox,
    crs = ex$crs
  )

  polys <- rgr_polygons(
    study_area,
    target_n = 20,
    cell_size = 250,
    seed = 123
  )

  bad <- rgr_topology_modify(
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

test_that("rgr_topology_modify rejects invalid inputs", {
  ex <- rgr_bbox_example("wa_outback")

  study_area <- rgr_study_area(
    bbox = ex$bbox,
    crs = ex$crs
  )

  pts <- rgr_points(
    study_area,
    n = 10,
    inside_pct = 1,
    seed = 123
  )

  expect_error(
    rgr_topology_modify(pts),
    "polygon geometries"
  )

  expect_error(
    rgr_topology_modify(study_area, pct = 0),
    "pct must be greater than 0"
  )

  expect_error(
    rgr_topology_modify(study_area, distance = 0),
    "distance must be greater than 0"
  )

  expect_error(
    rgr_topology_modify(study_area, mode = "gap"),
    "Currently only mode"
  )
})
