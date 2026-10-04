# 01 - rgr_add_attributes tests

test_that("rgr_add_attributes requires an sf object", {

  expect_error(
    rgr_add_attributes(
      data.frame(x = 1:3),
      type = "hazard_assessment"
    ),
    "x must be an sf object"
  )
})


test_that("rgr_add_attributes validates type", {

  study <- rgr_study_area()

  x <- rgr_points(
    study,
    n = 5,
    seed = 1
  )

  expect_error(
    rgr_add_attributes(
      x,
      type = "not_a_type"
    ),
    "Unsupported attribute type"
  )

  expect_error(
    rgr_add_attributes(
      x,
      type = c("hazard_assessment", "incident")
    ),
    "type must be a single character value"
  )

  expect_error(
    rgr_add_attributes(
      x,
      type = NA_character_
    ),
    "type must be a single character value"
  )
})


test_that("rgr_add_attributes validates seed", {

  study <- rgr_study_area()

  x <- rgr_points(
    study,
    n = 5,
    seed = 1
  )

  expect_error(
    rgr_add_attributes(
      x,
      type = "hazard_assessment",
      seed = NA_real_
    ),
    "seed must be NULL or a single finite numeric value"
  )

  expect_error(
    rgr_add_attributes(
      x,
      type = "hazard_assessment",
      seed = c(1, 2)
    ),
    "seed must be NULL or a single finite numeric value"
  )

  expect_error(
    rgr_add_attributes(
      x,
      type = "hazard_assessment",
      seed = Inf
    ),
    "seed must be NULL or a single finite numeric value"
  )
})


# 02 - Output structure -----------------------------------------------------

test_that("hazard_assessment preserves sf structure and row count", {

  study <- rgr_study_area()

  x <- rgr_points(
    study,
    n = 25,
    seed = 1
  )

  out <- rgr_add_attributes(
    x,
    type = "hazard_assessment",
    seed = 2
  )

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), nrow(x))
  expect_equal(sf::st_geometry(out), sf::st_geometry(x))
  expect_equal(sf::st_crs(out), sf::st_crs(x))
})


test_that("hazard_assessment adds required contextual fields", {

  study <- rgr_study_area()

  x <- rgr_points(
    study,
    n = 10,
    seed = 1
  )

  out <- rgr_add_attributes(
    x,
    type = "hazard_assessment",
    seed = 2
  )

  required_fields <- c(
    "assessment_id",
    "hazard_category",
    "hazard_type",
    "hazard_source",
    "hazard_description",
    "exposed_group",
    "exposure_context",
    "existing_control",
    "control_effectiveness",
    "consequence",
    "likelihood",
    "assessment_status",
    "rgr_attribute_type"
  )

  expect_true(
    all(required_fields %in% names(out))
  )

  expect_true(
    all(out$rgr_attribute_type == "hazard_assessment")
  )
})


test_that("hazard_assessment preserves existing attributes", {

  study <- rgr_study_area()

  x <- rgr_points(
    study,
    n = 10,
    seed = 1
  )

  original_point_id <- x$point_id
  original_inside_flag <- x$inside_flag
  original_generation_method <- x$generation_method

  out <- rgr_add_attributes(
    x,
    type = "hazard_assessment",
    seed = 2
  )

  expect_equal(
    out$point_id,
    original_point_id
  )

  expect_equal(
    out$inside_flag,
    original_inside_flag
  )

  expect_equal(
    out$generation_method,
    original_generation_method
  )
})


# 03 - Reproducibility ------------------------------------------------------

test_that("hazard_assessment is reproducible with seed", {

  study <- rgr_study_area()

  x <- rgr_points(
    study,
    n = 25,
    seed = 1
  )

  out_1 <- rgr_add_attributes(
    x,
    type = "hazard_assessment",
    seed = 123
  )

  out_2 <- rgr_add_attributes(
    x,
    type = "hazard_assessment",
    seed = 123
  )

  expect_equal(
    sf::st_drop_geometry(out_1),
    sf::st_drop_geometry(out_2)
  )
})


# 04 - Hazard coherence -----------------------------------------------------

test_that("hazard categories map to the correct hazard types", {

  study <- rgr_study_area()

  x <- rgr_points(
    study,
    n = 500,
    seed = 1
  )

  out <- rgr_add_attributes(
    x,
    type = "hazard_assessment",
    seed = 123
  )

  expected_types <- c(
    Terrain = "Fall from height",
    Water = "Deep or moving water",
    Weather = "Extreme heat",
    Traffic = "Vehicle interaction",
    Vegetation = "Falling vegetation",
    Wildlife = "Wildlife interaction",
    Fire = "Bushfire",
    Infrastructure = "Infrastructure failure"
  )

  expected <- unname(
    expected_types[out$hazard_category]
  )

  expect_equal(
    out$hazard_type,
    expected
  )
})


test_that("hazard_assessment generates plausible exposed groups", {

  study <- rgr_study_area()

  x <- rgr_points(
    study,
    n = 500,
    seed = 1
  )

  out <- rgr_add_attributes(
    x,
    type = "hazard_assessment",
    seed = 123
  )

  allowed_groups <- list(
    Terrain = c(
      "General visitors",
      "Walkers and hikers",
      "Children and families",
      "Remote-area visitors"
    ),
    Water = c(
      "General visitors",
      "Water users",
      "Children and families",
      "Walkers and hikers"
    ),
    Weather = c(
      "General visitors",
      "Walkers and hikers",
      "Children and families",
      "Cyclists",
      "Remote-area visitors"
    ),
    Traffic = c(
      "General visitors",
      "Walkers and hikers",
      "Children and families",
      "Cyclists",
      "Drivers and passengers"
    ),
    Vegetation = c(
      "General visitors",
      "Walkers and hikers",
      "Children and families",
      "Cyclists",
      "Remote-area visitors"
    ),
    Wildlife = c(
      "General visitors",
      "Walkers and hikers",
      "Children and families",
      "Water users",
      "Remote-area visitors"
    ),
    Fire = c(
      "General visitors",
      "Walkers and hikers",
      "Children and families",
      "Remote-area visitors"
    ),
    Infrastructure = c(
      "General visitors",
      "Walkers and hikers",
      "Children and families",
      "Cyclists"
    )
  )

  valid <- vapply(
    seq_len(nrow(out)),
    function(i) {
      out$exposed_group[[i]] %in%
        allowed_groups[[out$hazard_category[[i]]]]
    },
    logical(1)
  )

  expect_true(all(valid))
})


test_that("hazard_assessment generates plausible controls", {

  study <- rgr_study_area()

  x <- rgr_points(
    study,
    n = 500,
    seed = 1
  )

  out <- rgr_add_attributes(
    x,
    type = "hazard_assessment",
    seed = 123
  )

  allowed_controls <- list(
    Terrain = c(
      "Warning signage",
      "Physical barrier",
      "Defined access route",
      "Visitor information",
      "Temporary closure",
      "Operational monitoring"
    ),
    Water = c(
      "Warning signage",
      "Physical barrier",
      "Visitor information",
      "Defined access route",
      "Temporary closure",
      "Operational monitoring"
    ),
    Weather = c(
      "Warning signage",
      "Visitor information",
      "Temporary closure",
      "Operational monitoring"
    ),
    Traffic = c(
      "Warning signage",
      "Physical barrier",
      "Defined access route",
      "Visitor information",
      "Operational monitoring"
    ),
    Vegetation = c(
      "Warning signage",
      "Defined access route",
      "Inspection and maintenance",
      "Temporary closure",
      "Operational monitoring"
    ),
    Wildlife = c(
      "Warning signage",
      "Visitor information",
      "Defined access route",
      "Temporary closure",
      "Operational monitoring"
    ),
    Fire = c(
      "Warning signage",
      "Visitor information",
      "Temporary closure",
      "Operational monitoring"
    ),
    Infrastructure = c(
      "Warning signage",
      "Physical barrier",
      "Defined access route",
      "Inspection and maintenance",
      "Temporary closure"
    )
  )

  valid <- vapply(
    seq_len(nrow(out)),
    function(i) {
      out$existing_control[[i]] %in%
        allowed_controls[[out$hazard_category[[i]]]]
    },
    logical(1)
  )

  expect_true(all(valid))
})


# 05 - Assessment inputs ----------------------------------------------------

test_that("hazard_assessment uses valid assessment values", {

  study <- rgr_study_area()

  x <- rgr_points(
    study,
    n = 100,
    seed = 1
  )

  out <- rgr_add_attributes(
    x,
    type = "hazard_assessment",
    seed = 123
  )

  expect_true(
    all(
      out$control_effectiveness %in% c(
        "Effective",
        "Mostly effective",
        "Partially effective",
        "Limited",
        "Unknown"
      )
    )
  )

  expect_true(
    all(
      out$consequence %in% c(
        "Minimal",
        "Minor",
        "Moderate",
        "Major",
        "Severe"
      )
    )
  )

  expect_true(
    all(
      out$likelihood %in% c(
        "Rare",
        "Unlikely",
        "Possible",
        "Likely",
        "Almost Certain"
      )
    )
  )

  expect_true(
    all(
      out$assessment_status %in% c(
        "Draft",
        "Under review",
        "Current",
        "Review required"
      )
    )
  )
})


# 06 - Package boundary -----------------------------------------------------

test_that("hazard_assessment does not calculate risk outputs", {

  study <- rgr_study_area()

  x <- rgr_points(
    study,
    n = 25,
    seed = 1
  )

  out <- rgr_add_attributes(
    x,
    type = "hazard_assessment",
    seed = 123
  )

  analytical_outputs <- c(
    "risk_score",
    "risk_class",
    "expected_count",
    "rate_per_1000",
    "smr",
    "priority"
  )

  expect_false(
    any(analytical_outputs %in% names(out))
  )
})


# 07 - Empty input ----------------------------------------------------------

test_that("hazard_assessment handles zero-row sf input", {

  study <- rgr_study_area()

  x <- rgr_points(
    study,
    n = 5,
    seed = 1
  )

  x <- x[0, ]

  out <- rgr_add_attributes(
    x,
    type = "hazard_assessment",
    seed = 123
  )

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 0L)

  expect_true(
    all(
      c(
        "assessment_id",
        "hazard_category",
        "hazard_type",
        "hazard_source",
        "hazard_description",
        "exposed_group",
        "exposure_context",
        "existing_control",
        "control_effectiveness",
        "consequence",
        "likelihood",
        "assessment_status",
        "rgr_attribute_type"
      ) %in% names(out)
    )
  )
})
