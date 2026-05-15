test_that("rg_points handles geographic CRS inputs", {

  ex <- rg_bbox_example("wa_outback")

  study_area <- rg_study_area(
    bbox = ex$bbox,
    crs = ex$crs
  )

  study_area_ll <- sf::st_transform(study_area, 7844)

  pts <- rg_points(
    study_area = study_area_ll,
    n = 100,
    inside_pct = 0.9,
    outside_distance = 200,
    seed = 123,
    processing_crs = 7851
  )

  expect_s3_class(pts, "sf")

  expect_equal(
    sf::st_crs(pts)$epsg,
    7844
  )

  expect_equal(
    nrow(pts),
    100
  )

  expect_true(
    all(sf::st_is_valid(pts))
  )
})

test_that("rg_polygons handles geographic CRS inputs", {

  ex <- rg_bbox_example("wa_outback")

  study_area <- rg_study_area(
    bbox = ex$bbox,
    crs = ex$crs
  )

  study_area_ll <- sf::st_transform(study_area, 7844)

  polys <- rg_polygons(
    study_area = study_area_ll,
    target_n = 20,
    cell_size = 250,
    seed = 123,
    processing_crs = 7851
  )

  expect_s3_class(polys, "sf")

  expect_equal(
    sf::st_crs(polys)$epsg,
    7844
  )

  expect_true(
    nrow(polys) > 1
  )

  expect_true(
    all(sf::st_is_valid(polys))
  )

  expect_equal(
    sum(sf::st_is_empty(polys)),
    0
  )
})

test_that("rg_polygons approximately reconciles area after CRS roundtrip", {

  ex <- rg_bbox_example("wa_outback")

  study_area <- rg_study_area(
    bbox = ex$bbox,
    crs = ex$crs
  )

  study_area_ll <- sf::st_transform(study_area, 7844)

  polys <- rg_polygons(
    study_area = study_area_ll,
    target_n = 20,
    cell_size = 250,
    seed = 123,
    processing_crs = 7851
  )

  study_area_m2 <- sum(
    as.numeric(
      sf::st_area(
        sf::st_transform(study_area_ll, 7851)
      )
    )
  )

  poly_area_m2 <- sum(
    as.numeric(
      sf::st_area(
        sf::st_transform(polys, 7851)
      )
    )
  )

  expect_equal(
    poly_area_m2,
    study_area_m2,
    tolerance = 1
  )
})