# 01 - Test incident attribute generation

test_that("incident attributes preserve sf structure", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 25,
    inside_pct = 1,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "incident",
    seed = 456
  )

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), nrow(x))
  expect_equal(sf::st_geometry(out), sf::st_geometry(x))
  expect_equal(sf::st_crs(out), sf::st_crs(x))
})


# 02 - Test required incident fields ---------------------------------------

test_that("incident attributes contain required fields", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 25,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "incident",
    seed = 456
  )

  required_fields <- c(
    "event_id",
    "event_type",
    "hazard_category",
    "mechanism",
    "activity",
    "consequence",
    "event_date",
    "event_hour",
    "event_count",
    "rgr_attribute_type"
  )

  expect_true(
    all(required_fields %in% names(out))
  )

  expect_true(
    all(out[["rgr_attribute_type"]] == "incident")
  )
})


# 03 - Test preservation of existing attributes ----------------------------

test_that("incident generation preserves existing attributes", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 10,
    seed = 123
  )

  x[["existing_value"]] <- seq_len(nrow(x))

  out <- rgr_add_attributes(
    x,
    type = "incident",
    seed = 456
  )

  expect_equal(
    out[["existing_value"]],
    x[["existing_value"]]
  )
})


# 04 - Test reproducibility -------------------------------------------------

test_that("incident generation is reproducible", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 50,
    seed = 123
  )

  out_1 <- rgr_add_attributes(
    x,
    type = "incident",
    seed = 456
  )

  out_2 <- rgr_add_attributes(
    x,
    type = "incident",
    seed = 456
  )

  expect_equal(out_1, out_2)
})


# 05 - Test event and hazard coherence -------------------------------------

test_that("incident event types map to coherent hazard categories", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 500,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "incident",
    seed = 456
  )

  expected_categories <- c(
    Fall = "Terrain",
    Water = "Water",
    Vehicle = "Traffic",
    Weather = "Weather",
    Vegetation = "Vegetation",
    Wildlife = "Wildlife",
    Fire = "Fire",
    Infrastructure = "Infrastructure"
  )

  expected <- unname(
    expected_categories[out[["event_type"]]]
  )

  expect_equal(
    out[["hazard_category"]],
    expected
  )
})

# 06 - Test mechanism coherence --------------------------------------------

test_that("incident mechanisms are coherent with event type", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 500,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "incident",
    seed = 456
  )

  valid_mechanisms <- list(
    Fall = c(
      "Slip, trip or stumble",
      "Fall on same level",
      "Fall from height"
    ),
    Water = c(
      "Immersion",
      "Submersion",
      "Caught in moving water"
    ),
    Vehicle = c(
      "Vehicle collision",
      "Vehicle rollover",
      "Pedestrian interaction",
      "Cyclist interaction"
    ),
    Weather = c(
      "Heat exposure",
      "Cold exposure",
      "Severe weather exposure"
    ),
    Vegetation = c(
      "Struck by falling branch",
      "Struck by falling tree",
      "Contact with vegetation"
    ),
    Wildlife = c(
      "Bite or sting",
      "Animal contact",
      "Wildlife interaction"
    ),
    Fire = c(
      "Fire exposure",
      "Smoke exposure",
      "Burn"
    ),
    Infrastructure = c(
      "Contact with structure",
      "Infrastructure failure",
      "Entrapment"
    )
  )

  valid <- vapply(
    seq_len(nrow(out)),
    function(i) {
      out[["mechanism"]][[i]] %in%
        valid_mechanisms[[out[["event_type"]][[i]]]]
    },
    logical(1)
  )

  expect_true(all(valid))
})

# 07 - Test activity coherence ---------------------------------------------

test_that("incident activities are coherent with event type", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 500,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "incident",
    seed = 456
  )

  valid_activities <- list(
    Fall = c(
      "Walking",
      "Hiking",
      "Sightseeing",
      "General recreation"
    ),
    Water = c(
      "Swimming",
      "Boating",
      "Fishing",
      "Walking near water",
      "General recreation"
    ),
    Vehicle = c(
      "Driving",
      "Cycling",
      "Walking",
      "Parking or access"
    ),
    Weather = c(
      "Walking",
      "Hiking",
      "Cycling",
      "Camping",
      "General recreation"
    ),
    Vegetation = c(
      "Walking",
      "Hiking",
      "Camping",
      "General recreation"
    ),
    Wildlife = c(
      "Walking",
      "Hiking",
      "Camping",
      "Swimming",
      "General recreation"
    ),
    Fire = c(
      "Walking",
      "Hiking",
      "Camping",
      "General recreation"
    ),
    Infrastructure = c(
      "Walking",
      "Cycling",
      "Using visitor facilities",
      "General recreation"
    )
  )

  valid <- vapply(
    seq_len(nrow(out)),
    function(i) {
      out[["activity"]][[i]] %in%
        valid_activities[[out[["event_type"]][[i]]]]
    },
    logical(1)
  )

  expect_true(all(valid))
})

# 08 - Test consequence values ---------------------------------------------

test_that("incident consequences use valid values", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 100,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "incident",
    seed = 456
  )

  valid_consequences <- c(
    "Near miss",
    "Minimal",
    "Minor",
    "Moderate",
    "Major",
    "Severe",
    "Fatality"
  )

  expect_true(
    all(out[["consequence"]] %in% valid_consequences)
  )
})


# 09 - Test event dates -----------------------------------------------------

test_that("incident dates are valid and within generation range", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 100,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "incident",
    seed = 456
  )

  expect_s3_class(
    out[["event_date"]],
    "Date"
  )

  expect_true(
    all(out[["event_date"]] >= as.Date("2021-01-01"))
  )

  expect_true(
    all(out[["event_date"]] <= as.Date("2025-12-31"))
  )

  expect_false(
    anyNA(out[["event_date"]])
  )
})


# 10 - Test event hours -----------------------------------------------------

test_that("incident hours are valid", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 100,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "incident",
    seed = 456
  )

  expect_true(
    all(out[["event_hour"]] >= 0L)
  )

  expect_true(
    all(out[["event_hour"]] <= 23L)
  )

  expect_true(
    all(out[["event_hour"]] == as.integer(out[["event_hour"]]))
  )
})


# 11 - Test event counts ----------------------------------------------------

test_that("incident records represent individual events", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 100,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "incident",
    seed = 456
  )

  expect_true(
    all(out[["event_count"]] == 1L)
  )
})


# 12 - Test identifiers -----------------------------------------------------

test_that("incident identifiers are complete and unique", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 100,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "incident",
    seed = 456
  )

  expect_false(
    anyNA(out[["event_id"]])
  )

  expect_equal(
    length(unique(out[["event_id"]])),
    nrow(out)
  )

  expect_equal(
    out[["event_id"]][[1]],
    "event_000001"
  )
})


# 13 - Test absence of analytical outputs ----------------------------------

test_that("incident generation does not calculate analytical outputs", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 25,
    seed = 123
  )

  out <- rgr_add_attributes(
    x,
    type = "incident",
    seed = 456
  )

  analytical_fields <- c(
    "risk_score",
    "risk_class",
    "expected_count",
    "rate_per_1000",
    "smr",
    "priority"
  )

  expect_false(
    any(analytical_fields %in% names(out))
  )
})


# 14 - Test zero-row input --------------------------------------------------

test_that("incident generation handles zero-row sf input", {

  study <- rgr_study_area()

  x <- rgr_points(
    study_area = study,
    n = 1,
    seed = 123
  )

  x <- x[0, ]

  out <- rgr_add_attributes(
    x,
    type = "incident",
    seed = 456
  )

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 0L)

  required_fields <- c(
    "event_id",
    "event_type",
    "hazard_category",
    "mechanism",
    "activity",
    "consequence",
    "event_date",
    "event_hour",
    "event_count",
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
