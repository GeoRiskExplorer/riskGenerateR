# 01 — Point summary --------------------------------------------------------

test_that("rg_summary returns expected summary for point object", {

  x <- rg_scenario(
    scenario = "basic",
    seed = 123
  )

  s <- rg_summary(
    x$points_risk
  )

  expect_s3_class(
    s,
    "rg_summary"
  )

  expect_equal(
    s$rows,
    nrow(x$points_risk)
  )

  expect_equal(
    s$total_event_count,
    100
  )

  expect_equal(
    s$invalid_count,
    0
  )

  expect_equal(
    s$empty_count,
    0
  )

  expect_true(
    "risk_class_count" %in% names(s)
  )

  expect_true(
    "hazard_type_count" %in% names(s)
  )
})


# 02 — Hex tessellation summary --------------------------------------------

test_that("rg_summary returns expected summary for hex object", {

  x <- rg_scenario(
    scenario = "basic",
    seed = 123
  )

  s <- rg_summary(
    x$hex_risk
  )

  expect_s3_class(
    s,
    "rg_summary"
  )

  expect_true(
    s$total_event_count > 0
  )

  expect_true(
    s$total_exposure_count > 0
  )

  expect_true(
    s$total_area > 0
  )

  expect_true(
    "dominant_hazard_type_count" %in% names(s)
  )

  expect_true(
    "area_summary" %in% names(s)
  )

  expect_true(
    "rate_per_1000_summary" %in% names(s)
  )
})


# 03 — Square tessellation summary -----------------------------------------

test_that("rg_summary returns expected summary for square object", {

  x <- rg_scenario(
    scenario = "basic",
    seed = 123
  )

  s <- rg_summary(
    x$grid_risk
  )

  expect_s3_class(
    s,
    "rg_summary"
  )

  expect_true(
    s$total_event_count > 0
  )

  expect_true(
    s$total_exposure_count > 0
  )

  expect_true(
    s$total_area > 0
  )

  expect_true(
    "area_summary" %in% names(s)
  )
})


# 04 — Neutral synthetic XY area -------------------------------------------

test_that("rg_summary supports area in neutral synthetic XY", {

  study <- rg_study_area()

  x <- rg_tessellate(
    study,
    cell_size = 500,
    shape = "square"
  )

  s <- rg_summary(
    x
  )

  expect_s3_class(
    s,
    "rg_summary"
  )

  expect_true(
    is.na(s$crs_epsg)
  )

  expect_true(
    s$total_area > 0
  )

  expect_equal(
    s$total_area,
    sum(x$area)
  )

  expect_true(
    "area_summary" %in% names(s)
  )

  expect_false(
    "total_area_km2" %in% names(s)
  )
})


# 05 — Projected area -------------------------------------------------------

test_that("rg_summary supports area for projected spatial objects", {

  study <- rg_study_area(
    bbox = c(
      xmin = 300000,
      ymin = 5800000,
      xmax = 305000,
      ymax = 5805000
    ),
    crs = 7855
  )

  x <- rg_tessellate(
    study,
    cell_size = 500,
    shape = "square"
  )

  s <- rg_summary(
    x
  )

  expect_equal(
    s$crs_epsg,
    7855
  )

  expect_equal(
    s$total_area,
    sum(x$area)
  )

  expect_true(
    s$total_area > 0
  )
})


# 06 — Geometry QA ----------------------------------------------------------

test_that("rg_summary reports geometry QA correctly", {

  study <- rg_study_area()

  x <- rg_tessellate(
    study,
    cell_size = 500,
    shape = "hex"
  )

  s <- rg_summary(
    x
  )

  expect_equal(
    s$rows,
    nrow(x)
  )

  expect_equal(
    s$valid_count,
    nrow(x)
  )

  expect_equal(
    s$invalid_count,
    0
  )

  expect_equal(
    s$empty_count,
    0
  )

  expect_true(
    "POLYGON" %in% s$geometry_types
  )
})


# 07 — Optional attributes --------------------------------------------------

test_that("rg_summary only reports recognised attributes when present", {

  study <- rg_study_area()

  s <- rg_summary(
    study
  )

  expect_false(
    "total_event_count" %in% names(s)
  )

  expect_false(
    "total_exposure_count" %in% names(s)
  )

  expect_false(
    "total_expected_count" %in% names(s)
  )

  expect_false(
    "total_area" %in% names(s)
  )

  expect_false(
    "risk_class_count" %in% names(s)
  )
})


# 08 — Invalid input --------------------------------------------------------

test_that("rg_summary rejects non-sf objects", {

  expect_error(
    rg_summary(
      data.frame(
        x = 1
      )
    ),
    "x must be an sf object"
  )
})