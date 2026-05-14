test_that("rg_study_area returns an sf polygon with neutral defaults", {
  x <- rg_study_area()

  expect_s3_class(x, "sf")
  expect_equal(nrow(x), 1)
  expect_true(all(sf::st_geometry_type(x) == "POLYGON"))
  expect_true(is.na(sf::st_crs(x)))
})

test_that("rg_study_area accepts an example bbox and CRS", {
  ex <- rg_bbox_example("vicgrid")

  x <- rg_study_area(
    bbox = ex$bbox,
    crs = ex$crs
  )

  expect_s3_class(x, "sf")
  expect_equal(sf::st_crs(x)$epsg, 7899)
  expect_equal(nrow(x), 1)
})

test_that("rg_bbox_example rejects unknown examples", {
  expect_error(
    rg_bbox_example("banana"),
    "Unknown bbox example"
  )
})