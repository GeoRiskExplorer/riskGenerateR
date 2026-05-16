test_that("rg_example_data returns WA outback study area", {
  x <- rg_example_data("wa_outback")

  expect_s3_class(x, "sf")
  expect_equal(nrow(x), 1)
  expect_equal(sf::st_crs(x)$epsg, 7851)
})

test_that("rg_example_data returns basic scenario list", {
  x <- rg_example_data("basic_scenario")

  expect_type(x, "list")
  expect_true("study_area" %in% names(x))
  expect_true("points_risk" %in% names(x))
  expect_true("grid_risk" %in% names(x))
  expect_true("hex_risk" %in% names(x))
})

test_that("rg_example_data rejects unavailable Cottesloe demo", {
  expect_error(
    rg_example_data("cottesloe_demo"),
    "not yet bundled"
  )
})