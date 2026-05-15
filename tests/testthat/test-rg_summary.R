test_that("rg_summary returns expected summary for point case object", {
  x <- rg_scenario(
    scenario = "basic",
    seed = 123
  )

  s <- rg_summary(x$points_risk)

  expect_s3_class(s, "rg_summary")
  expect_equal(s$rows, nrow(x$points_risk))
  expect_equal(s$total_event_count, 100)
  expect_true(s$invalid_count == 0)
  expect_true(s$empty_count == 0)
  expect_true("risk_class_count" %in% names(s))
  expect_true("hazard_type_count" %in% names(s))
})

test_that("rg_summary returns expected summary for hex count object", {
  x <- rg_scenario(
    scenario = "basic",
    seed = 123
  )

  s <- rg_summary(x$hex_risk)

  expect_s3_class(s, "rg_summary")
  expect_true(s$total_event_count > 0)
  expect_true(s$total_exposure_count > 0)
  expect_true(s$total_area_km2 > 0)
  expect_true("dominant_hazard_type_count" %in% names(s))
  expect_true("area_km2_summary" %in% names(s))
  expect_true("rate_per_1000_summary" %in% names(s))
})

test_that("rg_summary returns expected summary for grid count object", {
  x <- rg_scenario(
    scenario = "basic",
    seed = 123
  )

  s <- rg_summary(x$grid_risk)

  expect_s3_class(s, "rg_summary")
  expect_true(s$total_event_count > 0)
  expect_true(s$total_exposure_count > 0)
  expect_true(s$total_area_km2 > 0)
  expect_true("area_km2_summary" %in% names(s))
})

test_that("rg_summary rejects non-sf objects", {
  expect_error(
    rg_summary(data.frame(x = 1)),
    "x must be an sf object"
  )
})