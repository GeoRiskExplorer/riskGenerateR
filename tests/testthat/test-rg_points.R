test_that("rg_points returns requested number of points", {
  ex <- rg_bbox_example("wa_outback")

  study_area <- rg_study_area(
    bbox = ex$bbox,
    crs = ex$crs
  )

  pts <- rg_points(
    study_area,
    n = 100,
    inside_pct = 0.9,
    seed = 123
  )

  expect_s3_class(pts, "sf")
  expect_equal(nrow(pts), 100)
  expect_true(all(sf::st_geometry_type(pts) == "POINT"))
  expect_equal(sum(pts$inside_flag), 90)
  expect_equal(sum(!pts$inside_flag), 10)
  expect_equal(sf::st_crs(pts)$epsg, 7851)
})

test_that("rg_points defaults to all outside points", {
  ex <- rg_bbox_example("wa_outback")

  study_area <- rg_study_area(
    bbox = ex$bbox,
    crs = ex$crs
  )

  pts <- rg_points(
    study_area,
    n = 25,
    seed = 123
  )

  expect_equal(nrow(pts), 25)
  expect_equal(sum(pts$inside_flag), 0)
  expect_equal(sum(!pts$inside_flag), 25)
})

test_that("rg_points rejects invalid inside_pct", {
  ex <- rg_bbox_example("wa_outback")

  study_area <- rg_study_area(
    bbox = ex$bbox,
    crs = ex$crs
  )

  expect_error(
    rg_points(study_area, inside_pct = 1.5),
    "inside_pct must be between 0 and 1"
  )
})