test_that("rg_grid returns square polygon cells with default overhang", {
  ex <- rg_bbox_example("wa_outback")

  study_area <- rg_study_area(
    bbox = ex$bbox,
    crs = ex$crs
  )

  grid <- rg_grid(
    study_area,
    cell_size = 500
  )

  expect_s3_class(grid, "sf")
  expect_true(nrow(grid) > 0)
  expect_true(all(sf::st_geometry_type(grid) %in% c("POLYGON", "MULTIPOLYGON")))
  expect_equal(sf::st_crs(grid)$epsg, 7851)
  expect_true("grid_id" %in% names(grid))
})

test_that("rg_grid supports clipped output", {
  ex <- rg_bbox_example("wa_outback")

  study_area <- rg_study_area(
    bbox = ex$bbox,
    crs = ex$crs
  )

  grid <- rg_grid(
    study_area,
    cell_size = 500,
    clip = TRUE
  )

  expect_s3_class(grid, "sf")
  expect_true(nrow(grid) > 0)
  expect_true(all(sf::st_geometry_type(grid) %in% c("POLYGON", "MULTIPOLYGON")))
  expect_true("grid_id" %in% names(grid))
})

test_that("rg_grid rejects invalid cell_size", {
  ex <- rg_bbox_example("wa_outback")

  study_area <- rg_study_area(
    bbox = ex$bbox,
    crs = ex$crs
  )

  expect_error(
    rg_grid(study_area, cell_size = 0),
    "cell_size must be greater than 0"
  )
})