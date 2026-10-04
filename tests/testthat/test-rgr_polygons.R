# 01 - Neutral XY polygon generation ---------------------------------------

test_that("rgr_polygons generates clean irregular polygons in synthetic XY", {

  study <- rgr_study_area()

  polys <- rgr_polygons(
    study,
    target_n = 20,
    cell_size = 250,
    seed = 123
  )

  expect_s3_class(polys, "sf")
  expect_true(nrow(polys) > 0L)
  expect_true(nrow(polys) <= 20L)

  expect_true(
    all(
      sf::st_geometry_type(polys) %in%
        c("POLYGON", "MULTIPOLYGON")
    )
  )

  expect_true(is.na(sf::st_crs(polys)))
  expect_true(all(sf::st_is_valid(polys)))
  expect_false(any(sf::st_is_empty(polys)))
})


# 02 - Standard output schema ----------------------------------------------

test_that("rgr_polygons returns standard generation fields", {

  study <- rgr_study_area()

  polys <- rgr_polygons(
    study,
    target_n = 20,
    cell_size = 250,
    seed = 123
  )

  expect_equal(
    names(polys),
    c(
      "poly_id",
      "seed_group",
      "source_cell_count",
      "generation_method",
      "topology_type",
      "area",
      "geometry"
    )
  )

  expect_true(all(polys$area > 0))
  expect_true(all(polys$source_cell_count > 0))
  expect_true(all(polys$generation_method == "seeded_grid"))
  expect_true(all(polys$topology_type == "clean"))
})


# 03 - Polygon identifiers --------------------------------------------------

test_that("rgr_polygons creates unique sequential polygon identifiers", {

  study <- rgr_study_area()

  polys <- rgr_polygons(
    study,
    target_n = 20,
    cell_size = 250,
    seed = 123
  )

  expect_equal(
    polys$poly_id,
    sprintf(
      "poly_%06d",
      seq_len(nrow(polys))
    )
  )

  expect_equal(
    length(unique(polys$poly_id)),
    nrow(polys)
  )
})


# 04 - Study-area coverage --------------------------------------------------

test_that("rgr_polygons approximately reconciles to study area", {

  study <- rgr_study_area()

  polys <- rgr_polygons(
    study,
    target_n = 20,
    cell_size = 250,
    seed = 123
  )

  study_area <- as.numeric(
    sf::st_area(study)
  )

  polygon_area <- sum(
    as.numeric(
      sf::st_area(polys)
    )
  )

  expect_equal(
    polygon_area,
    study_area,
    tolerance = 1
  )
})


# 05 - Stored area agrees with geometry ------------------------------------

test_that("rgr_polygons area field agrees with polygon geometry", {

  study <- rgr_study_area()

  polys <- rgr_polygons(
    study,
    target_n = 20,
    cell_size = 250,
    seed = 123
  )

  geometry_area <- as.numeric(
    sf::st_area(polys)
  )

  expect_equal(
    polys$area,
    geometry_area,
    tolerance = 1e-6
  )
})


# 06 - Seed reproducibility -------------------------------------------------

test_that("rgr_polygons is reproducible with the same seed", {

  study <- rgr_study_area()

  polys_a <- rgr_polygons(
    study,
    target_n = 20,
    cell_size = 250,
    seed = 999
  )

  polys_b <- rgr_polygons(
    study,
    target_n = 20,
    cell_size = 250,
    seed = 999
  )

  expect_equal(
    sf::st_as_binary(
      sf::st_geometry(polys_a)
    ),
    sf::st_as_binary(
      sf::st_geometry(polys_b)
    )
  )

  expect_equal(
    polys_a$seed_group,
    polys_b$seed_group
  )

  expect_equal(
    polys_a$source_cell_count,
    polys_b$source_cell_count
  )
})


# 07 - Different seeds ------------------------------------------------------

test_that("rgr_polygons different seeds generate different units", {

  study <- rgr_study_area()

  polys_a <- rgr_polygons(
    study,
    target_n = 20,
    cell_size = 250,
    seed = 123
  )

  polys_b <- rgr_polygons(
    study,
    target_n = 20,
    cell_size = 250,
    seed = 456
  )

  expect_false(
    identical(
      sf::st_as_binary(
        sf::st_geometry(polys_a)
      ),
      sf::st_as_binary(
        sf::st_geometry(polys_b)
      )
    )
  )
})


# 08 - Projected CRS --------------------------------------------------------

test_that("rgr_polygons preserves projected input CRS", {

  study <- rgr_study_area(
    bbox = c(
      xmin = 300000,
      ymin = 5800000,
      xmax = 305000,
      ymax = 5805000
    ),
    crs = 7855
  )

  polys <- rgr_polygons(
    study,
    target_n = 20,
    cell_size = 250,
    seed = 123
  )

  expect_equal(
    sf::st_crs(polys)$epsg,
    7855
  )

  expect_true(
    all(sf::st_is_valid(polys))
  )
})


# 09 - Geographic CRS -------------------------------------------------------

test_that("rgr_polygons returns geographic input to original CRS", {

  study <- rgr_study_area(
    bbox = c(
      xmin = 144.90,
      ymin = -37.85,
      xmax = 145.00,
      ymax = -37.75
    ),
    crs = 4326
  )

  polys <- suppressMessages(
    rgr_polygons(
      study,
      target_n = 20,
      cell_size = 250,
      processing_crs = 7855,
      seed = 123
    )
  )

  expect_equal(
    sf::st_crs(polys)$epsg,
    4326
  )

  expect_true(
    all(sf::st_is_valid(polys))
  )
})


# 10 - Return processing CRS ------------------------------------------------

test_that("rgr_polygons can retain processing CRS", {

  study <- rgr_study_area(
    bbox = c(
      xmin = 144.90,
      ymin = -37.85,
      xmax = 145.00,
      ymax = -37.75
    ),
    crs = 4326
  )

  polys <- suppressMessages(
    rgr_polygons(
      study,
      target_n = 20,
      cell_size = 250,
      processing_crs = 7855,
      return_input_crs = FALSE,
      seed = 123
    )
  )

  expect_equal(
    sf::st_crs(polys)$epsg,
    7855
  )
})

# 11 - Clip behaviour -------------------------------------------------------

test_that("rgr_polygons clipped output remains within study area", {

  study_geom <- sf::st_buffer(
    sf::st_sfc(
      sf::st_point(
        c(2500, 2500)
      )
    ),
    dist = 2200
  )

  study <- sf::st_sf(
    area_id = "study_001",
    geometry = study_geom
  )

  polys <- rgr_polygons(
    study,
    target_n = 20,
    cell_size = 250,
    clip = TRUE,
    seed = 123
  )

  outside <- suppressWarnings(
    sf::st_difference(
      sf::st_union(polys),
      sf::st_union(study)
    )
  )

  outside_area <- sum(
    as.numeric(
      sf::st_area(outside)
    )
  )

  expect_equal(
    outside_area,
    0,
    tolerance = 1e-6
  )
})



# 12 - Reject invalid target_n ---------------------------------------------

test_that("rgr_polygons rejects invalid target_n", {

  study <- rgr_study_area()

  expect_error(
    rgr_polygons(
      study,
      target_n = 0
    ),
    "target_n must be a positive whole number"
  )

  expect_error(
    rgr_polygons(
      study,
      target_n = -1
    ),
    "target_n must be a positive whole number"
  )

  expect_error(
    rgr_polygons(
      study,
      target_n = 10.5
    ),
    "target_n must be a positive whole number"
  )

  expect_error(
    rgr_polygons(
      study,
      target_n = NA_real_
    ),
    "target_n must be a positive whole number"
  )
})


# 13 - Reject invalid cell_size --------------------------------------------

test_that("rgr_polygons rejects invalid cell_size", {

  study <- rgr_study_area()

  expect_error(
    rgr_polygons(
      study,
      cell_size = 0
    ),
    "cell_size must be greater than 0"
  )

  expect_error(
    rgr_polygons(
      study,
      cell_size = -250
    ),
    "cell_size must be greater than 0"
  )

  expect_error(
    rgr_polygons(
      study,
      cell_size = NA_real_
    ),
    "cell_size must be greater than 0"
  )
})


# 14 - Reject unsupported method -------------------------------------------

test_that("rgr_polygons rejects unsupported methods", {

  study <- rgr_study_area()

  expect_error(
    rgr_polygons(
      study,
      method = "banana"
    ),
    "Currently only method = 'seeded_grid' is supported"
  )
})


# 15 - Reject processing CRS for neutral XY --------------------------------

test_that("rgr_polygons rejects processing_crs for neutral synthetic XY", {

  study <- rgr_study_area()

  expect_error(
    rgr_polygons(
      study,
      processing_crs = 7855
    ),
    "processing_crs cannot be supplied when study_area has no CRS"
  )
})


# 16 - Reject invalid logical controls -------------------------------------

test_that("rgr_polygons rejects invalid logical controls", {

  study <- rgr_study_area()

  expect_error(
    rgr_polygons(
      study,
      clip = NA
    ),
    "clip must be TRUE or FALSE"
  )

  expect_error(
    rgr_polygons(
      study,
      return_input_crs = NA
    ),
    "return_input_crs must be TRUE or FALSE"
  )
})


# 17 - Reject non-sf study area --------------------------------------------

test_that("rgr_polygons rejects non-sf study areas", {

  expect_error(
    rgr_polygons(
      data.frame(
        x = 1,
        y = 1
      )
    ),
    "study_area must be an sf object"
  )
})
