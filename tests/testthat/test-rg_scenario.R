test_that("rg_scenario basic returns expected objects", {
  x <- rg_scenario(
    scenario = "basic",
    seed = 123
  )

  expect_type(x, "list")
  expect_equal(x$scenario, "basic")

  expect_s3_class(x$study_area, "sf")
  expect_s3_class(x$points, "sf")
  expect_s3_class(x$points_risk, "sf")
  expect_s3_class(x$grid, "sf")
  expect_s3_class(x$grid_risk, "sf")
  expect_s3_class(x$hex, "sf")
  expect_s3_class(x$hex_risk, "sf")

  expect_equal(nrow(x$points), 100)
  expect_equal(sum(x$points$inside_flag), 90)
})

test_that("rg_scenario grid_counts excludes hex outputs", {
  x <- rg_scenario(
    scenario = "grid_counts",
    seed = 123
  )

  expect_true("grid" %in% names(x))
  expect_true("grid_risk" %in% names(x))
  expect_false("hex" %in% names(x))
  expect_false("hex_risk" %in% names(x))
})

test_that("rg_scenario hex_counts excludes grid outputs", {
  x <- rg_scenario(
    scenario = "hex_counts",
    seed = 123
  )

  expect_true("hex" %in% names(x))
  expect_true("hex_risk" %in% names(x))
  expect_false("grid" %in% names(x))
  expect_false("grid_risk" %in% names(x))
})

test_that("rg_scenario rejects unknown scenario", {
  expect_error(
    rg_scenario("banana"),
    "scenario must be one of"
  )
})