# 01 - Test risk-register attribute generation

test_that("risk-register attributes preserve sf structure", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 25,
    inside_pct = 1,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "risk_register",
    seed = 456
  )

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), nrow(x))
  expect_equal(sf::st_geometry(out), sf::st_geometry(x))
  expect_equal(sf::st_crs(out), sf::st_crs(x))
})


# 02 - Test required fields -------------------------------------------------

test_that("risk-register attributes contain required fields", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 25,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "risk_register",
    seed = 456
  )

  required_fields <- c(
    "risk_id",
    "objective",
    "risk_category",
    "cause",
    "risk_event",
    "consequence_description",
    "existing_control",
    "control_effectiveness",
    "owner_role",
    "review_status",
    "risk_statement",
    "rgr_attribute_type"
  )

  expect_true(
    all(required_fields %in% names(out))
  )

  expect_true(
    all(out[["rgr_attribute_type"]] == "risk_register")
  )
})


# 03 - Test preservation of existing attributes ----------------------------

test_that("risk-register generation preserves existing attributes", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 10,
    seed = 123
  )

  x[["existing_value"]] <- seq_len(nrow(x))

  out <- rgr_add_attributes(
    x,
    type = "risk_register",
    seed = 456
  )

  expect_equal(
    out[["existing_value"]],
    x[["existing_value"]]
  )
})


# 04 - Test reproducibility -------------------------------------------------

test_that("risk-register generation is reproducible", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 50,
    seed = 123
  )

  out_1 <- rgr_add_attributes(
    x,
    type = "risk_register",
    seed = 456
  )

  out_2 <- rgr_add_attributes(
    x,
    type = "risk_register",
    seed = 456
  )

  expect_equal(out_1, out_2)
})


# 05 - Test category and objective coherence -------------------------------

test_that("risk-register categories map to coherent objectives", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 500,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "risk_register",
    seed = 456
  )

  expected_objectives <- c(
    "Visitor safety" =
      "Provide safe and accessible visitor experiences",
    "Environment" =
      "Protect environmental and ecological values",
    "Infrastructure" =
      "Maintain safe and serviceable infrastructure",
    "Operations" =
      "Maintain effective and resilient operations",
    "Emergency management" =
      "Maintain effective emergency preparedness and response"
  )

  expected <- unname(
    expected_objectives[out[["risk_category"]]]
  )

  expect_equal(
    out[["objective"]],
    expected
  )
})


# 06 - Test category and owner coherence -----------------------------------

test_that("risk-register owners are coherent with risk category", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 500,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "risk_register",
    seed = 456
  )

  valid_owners <- list(
    "Visitor safety" = c(
      "Visitor safety manager",
      "Operations manager",
      "Area manager",
      "Site manager"
    ),
    "Environment" = c(
      "Environmental manager",
      "Operations manager",
      "Area manager",
      "Site manager"
    ),
    "Infrastructure" = c(
      "Asset manager",
      "Operations manager",
      "Area manager",
      "Site manager"
    ),
    "Operations" = c(
      "Operations manager",
      "Area manager",
      "Duty manager",
      "Program manager"
    ),
    "Emergency management" = c(
      "Emergency management coordinator",
      "Operations manager",
      "Area manager",
      "Duty manager"
    )
  )

  valid <- vapply(
    seq_len(nrow(out)),
    function(i) {
      out[["owner_role"]][[i]] %in%
        valid_owners[[out[["risk_category"]][[i]]]]
    },
    logical(1)
  )

  expect_true(all(valid))
})


# 07 - Test category and control coherence ---------------------------------

test_that("risk-register controls are coherent with risk category", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 500,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "risk_register",
    seed = 456
  )

  valid_controls <- list(
    "Visitor safety" = c(
      "Warning signage",
      "Visitor information",
      "Defined access routes",
      "Physical barriers",
      "Operational monitoring",
      "Temporary closure"
    ),
    "Environment" = c(
      "Defined access routes",
      "Environmental monitoring",
      "Visitor information",
      "Access restrictions",
      "Site rehabilitation",
      "Temporary closure"
    ),
    "Infrastructure" = c(
      "Routine inspection",
      "Preventive maintenance",
      "Physical barriers",
      "Temporary closure",
      "Asset condition monitoring",
      "Warning signage"
    ),
    "Operations" = c(
      "Operational procedures",
      "Staff training",
      "Incident response arrangements",
      "Operational monitoring",
      "Contingency planning",
      "Escalation procedures"
    ),
    "Emergency management" = c(
      "Emergency response plans",
      "Evacuation procedures",
      "Warning systems",
      "Temporary closure",
      "Operational monitoring",
      "Emergency service liaison"
    )
  )

  valid <- vapply(
    seq_len(nrow(out)),
    function(i) {
      out[["existing_control"]][[i]] %in%
        valid_controls[[out[["risk_category"]][[i]]]]
    },
    logical(1)
  )

  expect_true(all(valid))
})


# 08 - Test management values ----------------------------------------------

test_that("risk-register management fields use valid values", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 100,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "risk_register",
    seed = 456
  )

  valid_effectiveness <- c(
    "Effective",
    "Mostly effective",
    "Partially effective",
    "Limited",
    "Unknown"
  )

  valid_status <- c(
    "Current",
    "Review required",
    "Under review",
    "Draft"
  )

  expect_true(
    all(out[["control_effectiveness"]] %in% valid_effectiveness)
  )

  expect_true(
    all(out[["review_status"]] %in% valid_status)
  )
})


# 09 - Test identifiers -----------------------------------------------------

test_that("risk-register identifiers are complete and unique", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 100,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "risk_register",
    seed = 456
  )

  expect_false(
    anyNA(out[["risk_id"]])
  )

  expect_equal(
    length(unique(out[["risk_id"]])),
    nrow(out)
  )

  expect_equal(
    out[["risk_id"]][[1]],
    "risk_000001"
  )
})


# 10 - Test risk-statement construction ------------------------------------

test_that("risk statements are constructed from register components", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 100,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "risk_register",
    seed = 456
  )

  expected <- paste0(
    "Due to ",
    tolower(out[["cause"]]),
    ", there is a risk that ",
    tolower(out[["risk_event"]]),
    ", resulting in ",
    tolower(out[["consequence_description"]]),
    "."
  )

  expect_equal(
    out[["risk_statement"]],
    expected
  )

  expect_false(
    anyNA(out[["risk_statement"]])
  )
})


# 11 - Test absence of analytical outputs ----------------------------------

test_that("risk-register generation does not calculate risk ratings", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 25,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "risk_register",
    seed = 456
  )

  analytical_fields <- c(
    "risk_score",
    "risk_class",
    "risk_rating",
    "residual_risk",
    "expected_count",
    "rate_per_1000",
    "smr",
    "priority"
  )

  expect_false(
    any(analytical_fields %in% names(out))
  )
})


# 12 - Test zero-row input --------------------------------------------------

test_that("risk-register generation handles zero-row sf input", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 1,
    seed = 123
  )

  x <- x[0, ]

  out <- rgr_add_attributes(
    x,
    type = "risk_register",
    seed = 456
  )

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 0L)

  required_fields <- c(
    "risk_id",
    "objective",
    "risk_category",
    "cause",
    "risk_event",
    "consequence_description",
    "existing_control",
    "control_effectiveness",
    "owner_role",
    "review_status",
    "risk_statement",
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
