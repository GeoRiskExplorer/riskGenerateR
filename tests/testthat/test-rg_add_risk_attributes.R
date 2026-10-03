test_that("rg_add_risk_attributes adds case attributes to points", {
  ex <- rg_bbox_example("wa_outback")

  study_area <- rg_study_area(
    bbox = ex$bbox,
    crs = ex$crs
  )

  pts <- rg_points(
    study_area,
    n = 25,
    inside_pct = 0.9,
    seed = 123
  )

  out <- rg_add_risk_attributes(
    pts,
    seed = 123
  )

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 25)

  expect_true("hazard_type" %in% names(out))
  expect_true("consequence" %in% names(out))
  expect_true("likelihood" %in% names(out))

  expect_false("dominant_hazard_type" %in% names(out))
  expect_false("dominant_consequence" %in% names(out))
  expect_false("dominant_likelihood" %in% names(out))

  expect_true("risk_score" %in% names(out))
  expect_true("risk_class" %in% names(out))
  expect_true("event_id" %in% names(out))
  expect_true("case_date" %in% names(out))
  expect_true("hour" %in% names(out))

  expect_equal(unique(out$rg_target), "points")
  expect_equal(unique(out$rg_mode), "case")
  expect_true(all(out$event_count == 1))
  expect_equal(sum(out$event_count), nrow(out))
})

test_that("rg_add_risk_attributes adds count attributes to polygons", {
  ex <- rg_bbox_example("wa_outback")

  study_area <- rg_study_area(
    bbox = ex$bbox,
    crs = ex$crs
  )

grid <- rg_tessellate(
  study_area,
  cell_size = 500,
  shape = "square"
)

  out <- rg_add_risk_attributes(
    grid,
    seed = 123
  )

  expect_s3_class(out, "sf")

  expect_true("dominant_hazard_type" %in% names(out))
  expect_true("dominant_consequence" %in% names(out))
  expect_true("dominant_likelihood" %in% names(out))

  expect_false("hazard_type" %in% names(out))
  expect_false("consequence" %in% names(out))
  expect_false("likelihood" %in% names(out))

  expect_true("event_count" %in% names(out))
  expect_true("exposure_count" %in% names(out))
  expect_true("expected_count" %in% names(out))
  expect_true("rate_per_1000" %in% names(out))

  expect_equal(unique(out$rg_target), "polygons")
  expect_equal(unique(out$rg_mode), "count")
  expect_true(all(out$event_count >= 0))
  expect_true(all(out$exposure_count >= out$event_count + 1))
})

test_that("rg_add_risk_attributes allows count attributes on points", {
  ex <- rg_bbox_example("wa_outback")

  study_area <- rg_study_area(
    bbox = ex$bbox,
    crs = ex$crs
  )

  pts <- rg_points(
    study_area,
    n = 25,
    inside_pct = 0.9,
    seed = 123
  )

  out <- rg_add_risk_attributes(
    pts,
    target = "points",
    mode = "count",
    seed = 123
  )

  expect_equal(unique(out$rg_target), "points")
  expect_equal(unique(out$rg_mode), "count")

  expect_true("dominant_hazard_type" %in% names(out))
  expect_true("event_count" %in% names(out))
  expect_true("exposure_count" %in% names(out))
})