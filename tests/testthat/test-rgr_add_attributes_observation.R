# 01 - Test observation attribute generation

test_that("observation attributes preserve sf structure", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 25,
    inside_pct = 1,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "observation",
    seed = 456
  )

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), nrow(x))
  expect_equal(sf::st_geometry(out), sf::st_geometry(x))
  expect_equal(sf::st_crs(out), sf::st_crs(x))
})


# 02 - Test required fields -------------------------------------------------

test_that("observation attributes contain required fields", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 25,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "observation",
    seed = 456
  )

  required_fields <- c(
    "observation_id",
    "observation_type",
    "condition",
    "issue_type",
    "severity_indicator",
    "action_required",
    "action_status",
    "observation_date",
    "rgr_attribute_type"
  )

  expect_true(
    all(required_fields %in% names(out))
  )

  expect_true(
    all(out[["rgr_attribute_type"]] == "observation")
  )
})


# 03 - Test preservation of existing attributes ----------------------------

test_that("observation generation preserves existing attributes", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 10,
    seed = 123
  )

  x[["existing_value"]] <- seq_len(nrow(x))

  out <- rgr_add_attributes(
    x,
    type = "observation",
    seed = 456
  )

  expect_equal(
    out[["existing_value"]],
    x[["existing_value"]]
  )
})


# 04 - Test reproducibility -------------------------------------------------

test_that("observation generation is reproducible", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 50,
    seed = 123
  )

  out_1 <- rgr_add_attributes(
    x,
    type = "observation",
    seed = 456
  )

  out_2 <- rgr_add_attributes(
    x,
    type = "observation",
    seed = 456
  )

  expect_equal(out_1, out_2)
})


# 05 - Test observation types ----------------------------------------------

test_that("observation types use supported values", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 500,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "observation",
    seed = 456
  )

  valid_types <- c(
    "Access",
    "Infrastructure",
    "Environment",
    "Hazard",
    "Visitor use"
  )

  expect_true(
    all(out[["observation_type"]] %in% valid_types)
  )
})


# 06 - Test issue coherence -------------------------------------------------

test_that("observation issues are coherent with observation type", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 500,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "observation",
    seed = 456
  )

  valid_issues <- list(
    Access = c(
      "Track surface deterioration",
      "Access obstruction",
      "Erosion",
      "Drainage issue",
      "Wayfinding issue"
    ),
    Infrastructure = c(
      "Asset deterioration",
      "Damaged structure",
      "Barrier defect",
      "Signage defect",
      "Maintenance required"
    ),
    Environment = c(
      "Vegetation damage",
      "Erosion",
      "Waste or litter",
      "Habitat disturbance",
      "Water quality concern"
    ),
    Hazard = c(
      "Unstable terrain",
      "Falling vegetation",
      "Hazardous water conditions",
      "Fire-related condition",
      "Weather-related condition"
    ),
    "Visitor use" = c(
      "High visitor use",
      "User conflict",
      "Informal access",
      "Unsafe visitor behaviour",
      "Capacity pressure"
    )
  )

  valid <- vapply(
    seq_len(nrow(out)),
    function(i) {
      out[["issue_type"]][[i]] %in%
        valid_issues[[out[["observation_type"]][[i]]]]
    },
    logical(1)
  )

  expect_true(all(valid))
})


# 07 - Test condition and severity coherence -------------------------------

test_that("observation conditions map to coherent severity indicators", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 500,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "observation",
    seed = 456
  )

  expected_severity <- c(
    Good = "Low",
    Acceptable = "Low",
    Stable = "Low",
    Normal = "Low",
    Changed = "Moderate",
    Elevated = "Moderate",
    Degraded = "Moderate",
    Deteriorating = "High",
    Congested = "High",
    Poor = "High",
    Unsafe = "Critical",
    Problematic = "Critical"
  )

  expected <- unname(
    expected_severity[out[["condition"]]]
  )

  expect_equal(
    out[["severity_indicator"]],
    expected
  )
})


# 08 - Test action requirement coherence -----------------------------------

test_that("observation severity determines action requirement", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 500,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "observation",
    seed = 456
  )

  expected <- out[["severity_indicator"]] %in%
    c(
      "Moderate",
      "High",
      "Critical"
    )

  expect_equal(
    out[["action_required"]],
    expected
  )
})


# 09 - Test action status coherence ----------------------------------------

test_that("observation action status matches action requirement", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 500,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "observation",
    seed = 456
  )

  no_action <- !out[["action_required"]]
  action <- out[["action_required"]]

  expect_true(
    all(
      out[["action_status"]][no_action] ==
        "No action required"
    )
  )

  valid_action_status <- c(
    "Open",
    "Assigned",
    "In progress",
    "Completed"
  )

  expect_true(
    all(
      out[["action_status"]][action] %in%
        valid_action_status
    )
  )
})


# 10 - Test observation dates ----------------------------------------------

test_that("observation dates are valid and within generation range", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 100,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "observation",
    seed = 456
  )

  expect_s3_class(
    out[["observation_date"]],
    "Date"
  )

  expect_true(
    all(
      out[["observation_date"]] >=
        as.Date("2021-01-01")
    )
  )

  expect_true(
    all(
      out[["observation_date"]] <=
        as.Date("2025-12-31")
    )
  )

  expect_false(
    anyNA(out[["observation_date"]])
  )
})


# 11 - Test identifiers -----------------------------------------------------

test_that("observation identifiers are complete and unique", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 100,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "observation",
    seed = 456
  )

  expect_false(
    anyNA(out[["observation_id"]])
  )

  expect_equal(
    length(unique(out[["observation_id"]])),
    nrow(out)
  )

  expect_equal(
    out[["observation_id"]][[1]],
    "observation_000001"
  )
})


# 12 - Test absence of analytical outputs ----------------------------------

test_that("observation generation does not calculate risk outputs", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 25,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "observation",
    seed = 456
  )

  analytical_fields <- c(
    "risk_score",
    "risk_class",
    "risk_rating",
    "likelihood",
    "consequence",
    "expected_count",
    "rate_per_1000",
    "smr",
    "priority"
  )

  expect_false(
    any(analytical_fields %in% names(out))
  )
})


# 13 - Test zero-row input --------------------------------------------------

test_that("observation generation handles zero-row sf input", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 1,
    seed = 123
  )

  x <- x[0, ]

  out <- rgr_add_attributes(
    x,
    type = "observation",
    seed = 456
  )

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 0L)

  required_fields <- c(
    "observation_id",
    "observation_type",
    "condition",
    "issue_type",
    "severity_indicator",
    "action_required",
    "action_status",
    "observation_date",
    "rgr_attribute_type"
  )

  expect_true(
    all(required_fields %in% names(out))
  )

  expect_equal(
    length(out[["rgr_attribute_type"]]),
    0L
  )
})
