# 01 — Default synthetic XY generation -------------------------------------

test_that("rg_points generates default points inside synthetic XY study area", {

  study <- rg_study_area()

  pts <- rg_points(
    study,
    n = 100,
    seed = 123
  )

  expect_s3_class(pts, "sf")
  expect_equal(nrow(pts), 100L)

  expect_true(
    all(
      as.character(sf::st_geometry_type(pts)) == "POINT"
    )
  )

  expect_true(
    all(pts$inside_flag)
  )

  expect_equal(
    sum(!pts$inside_flag),
    0L
  )

  expect_true(
    is.na(sf::st_crs(pts))
  )
})


# 02 — Requested number of points ------------------------------------------

test_that("rg_points returns requested number of points", {

  study <- rg_study_area()

  pts <- rg_points(
    study,
    n = 37,
    seed = 123
  )

  expect_equal(
    nrow(pts),
    37L
  )
})


# 03 — Mixed inside and outside points -------------------------------------

test_that("rg_points generates requested inside and outside proportions", {

  study <- rg_study_area()

  pts <- rg_points(
    study,
    n = 100,
    inside_pct = 0.8,
    outside_distance = 1000,
    seed = 123
  )

  expect_equal(
    sum(pts$inside_flag),
    80L
  )

  expect_equal(
    sum(!pts$inside_flag),
    20L
  )
})


# 04 — Spatial containment -------------------------------------------------

test_that("rg_points inside_flag agrees with spatial containment", {

  study <- rg_study_area()

  pts <- rg_points(
    study,
    n = 100,
    inside_pct = 0.8,
    outside_distance = 1000,
    seed = 123
  )

  inside_actual <- lengths(
    sf::st_within(
      pts,
      study
    )
  ) > 0L

  expect_equal(
    inside_actual,
    pts$inside_flag
  )
})


# 05 — Point identifiers ---------------------------------------------------

test_that("rg_points creates unique sequential point identifiers", {

  study <- rg_study_area()

  pts <- rg_points(
    study,
    n = 25,
    seed = 123
  )

  expect_equal(
    pts$point_id,
    sprintf(
      "pt_%06d",
      seq_len(25)
    )
  )

  expect_equal(
    length(unique(pts$point_id)),
    25L
  )
})


# 06 — Generation metadata ------------------------------------------------

test_that("rg_points records generation method", {

  study <- rg_study_area()

  pts <- rg_points(
    study,
    n = 25,
    seed = 123
  )

  expect_true(
    all(
      pts$generation_method == "random"
    )
  )
})


# 07 — Seed reproducibility ------------------------------------------------

test_that("rg_points is reproducible with the same seed", {

  study <- rg_study_area()

  pts_a <- rg_points(
    study,
    n = 50,
    seed = 999
  )

  pts_b <- rg_points(
    study,
    n = 50,
    seed = 999
  )

  expect_equal(
    sf::st_coordinates(pts_a),
    sf::st_coordinates(pts_b)
  )

  expect_equal(
    pts_a$inside_flag,
    pts_b$inside_flag
  )
})


# 08 — Different seeds -----------------------------------------------------

test_that("rg_points different seeds generate different locations", {

  study <- rg_study_area()

  pts_a <- rg_points(
    study,
    n = 50,
    seed = 123
  )

  pts_b <- rg_points(
    study,
    n = 50,
    seed = 456
  )

  expect_false(
    identical(
      sf::st_coordinates(pts_a),
      sf::st_coordinates(pts_b)
    )
  )
})


# 09 — Projected CRS -------------------------------------------------------

test_that("rg_points preserves projected input CRS", {

  study <- rg_study_area(
    bbox = c(
      xmin = 300000,
      ymin = 5800000,
      xmax = 305000,
      ymax = 5805000
    ),
    crs = 7855
  )

  pts <- rg_points(
    study,
    n = 50,
    seed = 123
  )

  expect_equal(
    sf::st_crs(pts)$epsg,
    7855
  )
})


# 10 — Geographic CRS round trip ------------------------------------------

test_that("rg_points returns geographic input to its original CRS", {

  study <- rg_study_area(
    bbox = c(
      xmin = 144.90,
      ymin = -37.85,
      xmax = 145.00,
      ymax = -37.75
    ),
    crs = 4326
  )

  pts <- suppressMessages(
    rg_points(
      study,
      n = 50,
      processing_crs = 7855,
      seed = 123
    )
  )

  expect_equal(
    sf::st_crs(pts)$epsg,
    4326
  )
})


# 11 — Retain processing CRS -----------------------------------------------

test_that("rg_points can return the processing CRS", {

  study <- rg_study_area(
    bbox = c(
      xmin = 144.90,
      ymin = -37.85,
      xmax = 145.00,
      ymax = -37.75
    ),
    crs = 4326
  )

  pts <- rg_points(
    study,
    n = 50,
    processing_crs = 7855,
    return_input_crs = FALSE,
    seed = 123
  )

  expect_equal(
    sf::st_crs(pts)$epsg,
    7855
  )
})


# 12 — Reject processing CRS for synthetic XY ------------------------------

test_that("rg_points rejects processing_crs for neutral synthetic XY", {

  study <- rg_study_area()

  expect_error(
    rg_points(
      study,
      processing_crs = 7855
    ),
    "processing_crs cannot be supplied when study_area has no CRS"
  )
})


# 13 — Reject invalid n -----------------------------------------------------

test_that("rg_points rejects invalid n", {

  study <- rg_study_area()

  expect_error(
    rg_points(
      study,
      n = 0
    ),
    "n must be a positive whole number"
  )

  expect_error(
    rg_points(
      study,
      n = -10
    ),
    "n must be a positive whole number"
  )

  expect_error(
    rg_points(
      study,
      n = 10.5
    ),
    "n must be a positive whole number"
  )

  expect_error(
    rg_points(
      study,
      n = NA_real_
    ),
    "n must be a positive whole number"
  )
})


# 14 — Reject invalid inside_pct -------------------------------------------

test_that("rg_points rejects invalid inside_pct", {

  study <- rg_study_area()

  expect_error(
    rg_points(
      study,
      inside_pct = -0.1
    ),
    "inside_pct must be between 0 and 1"
  )

  expect_error(
    rg_points(
      study,
      inside_pct = 1.1
    ),
    "inside_pct must be between 0 and 1"
  )

  expect_error(
    rg_points(
      study,
      inside_pct = NA_real_
    ),
    "inside_pct must be between 0 and 1"
  )
})


# 15 — Boundary inside_pct values ------------------------------------------

test_that("rg_points accepts inside_pct boundary values", {

  study <- rg_study_area()

  pts_inside <- rg_points(
    study,
    n = 20,
    inside_pct = 1,
    seed = 123
  )

  pts_outside <- rg_points(
    study,
    n = 20,
    inside_pct = 0,
    outside_distance = 1000,
    seed = 123
  )

  expect_equal(
    sum(pts_inside$inside_flag),
    20L
  )

  expect_equal(
    sum(!pts_inside$inside_flag),
    0L
  )

  expect_equal(
    sum(pts_outside$inside_flag),
    0L
  )

  expect_equal(
    sum(!pts_outside$inside_flag),
    20L
  )
})


# 16 — Reject invalid outside distance -------------------------------------

test_that("rg_points rejects invalid outside_distance", {

  study <- rg_study_area()

  expect_error(
    rg_points(
      study,
      outside_distance = 0
    ),
    "outside_distance must be greater than 0"
  )

  expect_error(
    rg_points(
      study,
      outside_distance = -100
    ),
    "outside_distance must be greater than 0"
  )

  expect_error(
    rg_points(
      study,
      outside_distance = NA_real_
    ),
    "outside_distance must be greater than 0"
  )
})


# 17 — Reject non-sf study area --------------------------------------------

test_that("rg_points rejects non-sf study area", {

  expect_error(
    rg_points(
      data.frame(
        x = 1,
        y = 1
      )
    ),
    "study_area must be an sf object"
  )
})


# 18 — Output geometry validity --------------------------------------------

test_that("rg_points returns valid non-empty point geometry", {

  study <- rg_study_area()

  pts <- rg_points(
    study,
    n = 100,
    inside_pct = 0.8,
    seed = 123
  )

  expect_true(
    all(sf::st_is_valid(pts))
  )

  expect_false(
    any(sf::st_is_empty(pts))
  )
})