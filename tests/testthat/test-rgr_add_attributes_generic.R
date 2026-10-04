# 01 - Test generic attribute generation

test_that("generic attributes preserve sf structure", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 25,
    inside_pct = 1,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "generic",
    seed = 456
  )

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), nrow(x))
  expect_equal(sf::st_geometry(out), sf::st_geometry(x))
  expect_equal(sf::st_crs(out), sf::st_crs(x))
})


# 02 - Test required fields -------------------------------------------------

test_that("generic attributes contain required fields", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 25,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "generic",
    seed = 456
  )

  required_fields <- c(
    "record_id",
    "category",
    "group",
    "status",
    "value",
    "count",
    "record_date",
    "rgr_attribute_type"
  )

  expect_true(
    all(required_fields %in% names(out))
  )

  expect_true(
    all(out[["rgr_attribute_type"]] == "generic")
  )
})


# 03 - Test preservation of existing attributes ----------------------------

test_that("generic generation preserves existing attributes", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 10,
    seed = 123
  )

  x[["existing_value"]] <- seq_len(nrow(x))

  out <- rgr_add_attributes(
    x,
    type = "generic",
    seed = 456
  )

  expect_equal(
    out[["existing_value"]],
    x[["existing_value"]]
  )
})


# 04 - Test reproducibility -------------------------------------------------

test_that("generic generation is reproducible", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 50,
    seed = 123
  )

  out_1 <- rgr_add_attributes(
    x,
    type = "generic",
    seed = 456
  )

  out_2 <- rgr_add_attributes(
    x,
    type = "generic",
    seed = 456
  )

  expect_equal(out_1, out_2)
})


# 05 - Test categorical values ---------------------------------------------

test_that("generic categorical attributes use supported values", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 500,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "generic",
    seed = 456
  )

  expect_true(
    all(
      out[["category"]] %in%
        c(
          "Category A",
          "Category B",
          "Category C",
          "Category D"
        )
    )
  )

  expect_true(
    all(
      out[["group"]] %in%
        c(
          "Group 1",
          "Group 2",
          "Group 3"
        )
    )
  )

  expect_true(
    all(
      out[["status"]] %in%
        c(
          "Active",
          "Inactive",
          "Pending",
          "Complete"
        )
    )
  )
})


# 06 - Test numeric values --------------------------------------------------

test_that("generic numeric attributes use valid ranges and types", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 500,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "generic",
    seed = 456
  )

  expect_type(
    out[["value"]],
    "double"
  )

  expect_true(
    all(out[["value"]] >= 0)
  )

  expect_true(
    all(out[["value"]] <= 100)
  )

  expect_false(
    anyNA(out[["value"]])
  )

  expect_type(
    out[["count"]],
    "integer"
  )

  expect_true(
    all(out[["count"]] >= 0)
  )

  expect_false(
    anyNA(out[["count"]])
  )
})


# 07 - Test record dates ----------------------------------------------------

test_that("generic record dates are valid and within generation range", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 100,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "generic",
    seed = 456
  )

  expect_s3_class(
    out[["record_date"]],
    "Date"
  )

  expect_true(
    all(
      out[["record_date"]] >=
        as.Date("2021-01-01")
    )
  )

  expect_true(
    all(
      out[["record_date"]] <=
        as.Date("2025-12-31")
    )
  )

  expect_false(
    anyNA(out[["record_date"]])
  )
})


# 08 - Test identifiers -----------------------------------------------------

test_that("generic identifiers are complete and unique", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 100,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "generic",
    seed = 456
  )

  expect_false(
    anyNA(out[["record_id"]])
  )

  expect_equal(
    length(unique(out[["record_id"]])),
    nrow(out)
  )

  expect_equal(
    out[["record_id"]][[1]],
    "record_000001"
  )
})


# 09 - Test absence of risk semantics --------------------------------------

test_that("generic generation does not introduce risk outputs", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 25,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "generic",
    seed = 456
  )

  risk_fields <- c(
    "hazard_category",
    "consequence",
    "likelihood",
    "risk_score",
    "risk_class",
    "risk_rating",
    "expected_count",
    "rate_per_1000",
    "smr",
    "priority"
  )

  expect_false(
    any(risk_fields %in% names(out))
  )
})


# 10 - Test zero-row input --------------------------------------------------

test_that("generic generation handles zero-row sf input", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 1,
    seed = 123
  )

  x <- x[0, ]

  out <- rgr_add_attributes(
    x,
    type = "generic",
    seed = 456
  )

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 0L)

  required_fields <- c(
    "record_id",
    "category",
    "group",
    "status",
    "value",
    "count",
    "record_date",
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
