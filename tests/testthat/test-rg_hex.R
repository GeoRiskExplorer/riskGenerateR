test_that("rg_hex returns polygon cells with default overhang", {
  ex <- rg_bbox_example("wa_outback")

  study_area <- rg_study_area(
    bbox = ex$bbox,
    crs = ex$crs
  )

  hex <- rg_hex(
    study_area,
    cell_size = 500
  )

  expect_s3_class(hex, "sf")
  expect_true(nrow(hex) > 0)
  expect_true(all(sf::st_geometry_type(hex) %in% c("POLYGON", "MULTIPOLYGON")))
  expect_equal(sf::st_crs(hex)$epsg, 7851)
  expect_true("hex_id" %in% names(hex))
})

test_that("rg_hex supports clipped output", {
  ex <- rg_bbox_example("wa_outback")

  study_area <- rg_study_area(
    bbox = ex$bbox,
    crs = ex$crs
  )

  hex <- rg_hex(
    study_area,
    cell_size = 500,
    clip = TRUE
  )

  expect_s3_class(hex, "sf")
  expect_true(nrow(hex) > 0)
  expect_true(all(sf::st_geometry_type(hex) %in% c("POLYGON", "MULTIPOLYGON")))
  expect_true("hex_id" %in% names(hex))
})

test_that("rg_hex rejects invalid cell_size", {
  ex <- rg_bbox_example("wa_outback")

  study_area <- rg_study_area(
    bbox = ex$bbox,
    crs = ex$crs
  )

  expect_error(
    rg_hex(study_area, cell_size = 0),
    "cell_size must be greater than 0"
  )
})