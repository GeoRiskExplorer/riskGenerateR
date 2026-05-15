test_that("rg_polygons returns clean irregular polygon units", {
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

  expect_s3_class(polys, "sf")
  expect_true(nrow(polys) > 0)
  expect_true(all(sf::st_geometry_type(polys) %in% c("POLYGON", "MULTIPOLYGON")))
  expect_equal(sf::st_crs(polys)$epsg, 7851)

  expect_true("poly_id" %in% names(polys))
  expect_true("source_cell_count" %in% names(polys))
  expect_true("area_km2" %in% names(polys))

  expect_true(all(sf::st_is_valid(polys)))
  expect_equal(sum(sf::st_is_empty(polys)), 0)
})

test_that("rg_polygons approximately reconciles area to study area", {
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

  study_area_m2 <- as.numeric(sf::st_area(study_area))
  poly_area_m2 <- sum(as.numeric(sf::st_area(polys)))

  expect_equal(
    poly_area_m2,
    study_area_m2,
    tolerance = 1
  )
})

test_that("rg_polygons rejects invalid parameters", {
  ex <- rg_bbox_example("wa_outback")

  study_area <- rg_study_area(
    bbox = ex$bbox,
    crs = ex$crs
  )

  expect_error(
    rg_polygons(study_area, target_n = 0),
    "target_n must be greater than 0"
  )

  expect_error(
    rg_polygons(study_area, cell_size = 0),
    "cell_size must be greater than 0"
  )

  expect_error(
    rg_polygons(study_area, method = "banana"),
    "Currently only method"
  )
})
