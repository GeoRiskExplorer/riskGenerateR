# 01 - Test incident summary

test_that("rgr_summary returns expected summary for incident object", {

  x <- rgr_scenario(
    scenario = "basic",
    seed = 123
  )

  s <- rgr_summary(
    x$incidents
  )

  expect_s3_class(
    s,
    "rgr_summary"
  )

  expect_equal(
    s$rows,
    nrow(x$incidents)
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
    "attribute_type_count" %in% names(s)
  )

  expect_true(
    "hazard_category_count" %in% names(s)
  )

  expect_true(
    "event_type_count" %in% names(s)
  )
})


# 02 - Test generic hex summary --------------------------------------------

test_that("rgr_summary returns expected summary for generic hex object", {

  x <- rgr_scenario(
    scenario = "basic",
    seed = 123
  )

  s <- rgr_summary(
    x$hex_attributes
  )

  expect_s3_class(
    s,
    "rgr_summary"
  )

  expect_equal(
    s$rows,
    nrow(x$hex_attributes)
  )

  expect_true(
    s$total_count >= 0
  )

  expect_true(
    s$total_area > 0
  )

  expect_true(
    "count_summary" %in% names(s)
  )

  expect_true(
    "area_summary" %in% names(s)
  )

  expect_true(
    "category_count" %in% names(s)
  )

  expect_true(
    "attribute_type_count" %in% names(s)
  )
})


# 03 - Test generic square summary -----------------------------------------

test_that("rgr_summary returns expected summary for generic square object", {

  x <- rgr_scenario(
    scenario = "basic",
    seed = 123
  )

  s <- rgr_summary(
    x$grid_attributes
  )

  expect_s3_class(
    s,
    "rgr_summary"
  )

  expect_equal(
    s$rows,
    nrow(x$grid_attributes)
  )

  expect_equal(
    s$total_count,
    sum(
      x$grid_attributes[["count"]]
    )
  )

  expect_equal(
    s$total_area,
    sum(
      x$grid_attributes[["area"]]
    )
  )

  expect_true(
    "count_summary" %in% names(s)
  )

  expect_true(
    "area_summary" %in% names(s)
  )
})


# 04 - Test neutral synthetic XY area --------------------------------------

test_that("rgr_summary supports area in neutral synthetic XY", {

  study <- rgr_study_area()

  x <- rgr_tessellate(
    study,
    cell_size = 500,
    shape = "square"
  )

  s <- rgr_summary(
    x
  )

  expect_s3_class(
    s,
    "rgr_summary"
  )

  expect_true(
    is.na(s$crs_epsg)
  )

  expect_true(
    s$total_area > 0
  )

  expect_equal(
    s$total_area,
    sum(
      x[["area"]]
    )
  )

  expect_true(
    "area_summary" %in% names(s)
  )

  expect_false(
    "total_area_km2" %in% names(s)
  )
})


# 05 - Test projected area --------------------------------------------------

test_that("rgr_summary supports area for projected spatial objects", {

  study <- rgr_study_area(
    bbox = c(
      xmin = 300000,
      ymin = 5800000,
      xmax = 305000,
      ymax = 5805000
    ),
    crs = 7855
  )

  x <- rgr_tessellate(
    study,
    cell_size = 500,
    shape = "square"
  )

  s <- rgr_summary(
    x
  )

  expect_equal(
    s$crs_epsg,
    7855
  )

  expect_equal(
    s$total_area,
    sum(
      x[["area"]]
    )
  )

  expect_true(
    s$total_area > 0
  )
})


# 06 - Test geometry QA -----------------------------------------------------

test_that("rgr_summary reports geometry QA correctly", {

  study <- rgr_study_area()

  x <- rgr_tessellate(
    study,
    cell_size = 500,
    shape = "hex"
  )

  s <- rgr_summary(
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


# 07 - Test optional attributes --------------------------------------------

test_that("rgr_summary only reports generated attributes when present", {

  study <- rgr_study_area()

  s <- rgr_summary(
    study
  )

  expect_false(
    "total_event_count" %in% names(s)
  )

  expect_false(
    "total_count" %in% names(s)
  )

  expect_false(
    "total_area" %in% names(s)
  )

  expect_false(
    "attribute_type_count" %in% names(s)
  )

  expect_false(
    "hazard_category_count" %in% names(s)
  )
})


# 08 - Test contextual hazard assessment -----------------------------------

test_that("rgr_summary recognises hazard assessment attributes", {

  study <- rgr_study_area()

  points <- rgr_points(
    study,
    n = 50,
    seed = 123
  )

  x <- rgr_add_attributes(
    points,
    type = "hazard_assessment",
    seed = 123
  )

  s <- rgr_summary(
    x
  )

  expect_equal(
    s$rows,
    50
  )

  expect_true(
    "attribute_type_count" %in% names(s)
  )

  expect_true(
    "hazard_category_count" %in% names(s)
  )

  expect_equal(
    unname(
      s$attribute_type_count[["hazard_assessment"]]
    ),
    50
  )
})


# 09 - Test observation attributes -----------------------------------------

test_that("rgr_summary recognises observation attributes", {

  study <- rgr_study_area()

  points <- rgr_points(
    study,
    n = 50,
    seed = 123
  )

  x <- rgr_add_attributes(
    points,
    type = "observation",
    seed = 123
  )

  s <- rgr_summary(
    x
  )

  expect_true(
    "observation_type_count" %in% names(s)
  )

  expect_equal(
    sum(
      s$observation_type_count
    ),
    50
  )
})


# 10 - Test risk-register attributes ---------------------------------------

test_that("rgr_summary recognises risk register attributes", {

  study <- rgr_study_area()

  points <- rgr_points(
    study,
    n = 50,
    seed = 123
  )

  x <- rgr_add_attributes(
    points,
    type = "risk_register",
    seed = 123
  )

  s <- rgr_summary(
    x
  )

  expect_true(
    "risk_category_count" %in% names(s)
  )

  expect_equal(
    sum(
      s$risk_category_count
    ),
    50
  )
})


# 11 - Test absence of analytical summaries --------------------------------

test_that("rgr_summary does not generate analytical risk summaries", {

  x <- rgr_scenario(
    scenario = "basic",
    seed = 123
  )

  s <- rgr_summary(
    x$incidents
  )

  analytical_outputs <- c(
    "total_exposure_count",
    "exposure_count_summary",
    "total_expected_count",
    "expected_count_summary",
    "rate_per_1000_summary",
    "risk_class_count",
    "dominant_hazard_type_count"
  )

  expect_false(
    any(
      analytical_outputs %in% names(s)
    )
  )
})


# 12 - Test invalid input ---------------------------------------------------

test_that("rgr_summary rejects non-sf objects", {

  expect_error(
    rgr_summary(
      data.frame(
        x = 1
      )
    ),
    "x must be an sf object"
  )
})
