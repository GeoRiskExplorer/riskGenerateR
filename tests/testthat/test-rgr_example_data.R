# 01 - Test built-in example data

test_that("rgr_example_data returns WA outback study area", {

  x <- rgr_example_data(
    "wa_outback"
  )

  expect_s3_class(
    x,
    "sf"
  )

  expect_equal(
    nrow(x),
    1
  )

  expect_equal(
    sf::st_crs(x)$epsg,
    7851
  )
})


# 02 - Test basic scenario --------------------------------------------------

test_that("rgr_example_data returns basic scenario", {

  x <- rgr_example_data(
    "basic_scenario"
  )

  expect_type(
    x,
    "list"
  )

  expect_equal(
    x$scenario,
    "basic"
  )

  expect_true(
    all(
      c(
        "study_area",
        "points",
        "incidents",
        "grid",
        "grid_attributes",
        "hex",
        "hex_attributes"
      ) %in% names(x)
    )
  )

  expect_s3_class(
    x$study_area,
    "sf"
  )

  expect_s3_class(
    x$points,
    "sf"
  )

  expect_s3_class(
    x$incidents,
    "sf"
  )

  expect_s3_class(
    x$grid,
    "sf"
  )

  expect_s3_class(
    x$grid_attributes,
    "sf"
  )

  expect_s3_class(
    x$hex,
    "sf"
  )

  expect_s3_class(
    x$hex_attributes,
    "sf"
  )
})


# 03 - Test contextual attribute types -------------------------------------

test_that("basic scenario contains contextual synthetic attributes", {

  x <- rgr_example_data(
    "basic_scenario"
  )

  expect_true(
    all(
      x$incidents[["rgr_attribute_type"]] ==
        "incident"
    )
  )

  expect_true(
    all(
      x$grid_attributes[["rgr_attribute_type"]] ==
        "generic"
    )
  )

  expect_true(
    all(
      x$hex_attributes[["rgr_attribute_type"]] ==
        "generic"
    )
  )
})


# 04 - Test absence of legacy risk objects ---------------------------------

test_that("basic scenario does not contain legacy risk objects", {

  x <- rgr_example_data(
    "basic_scenario"
  )

  legacy_objects <- c(
    "points_risk",
    "grid_risk",
    "hex_risk",
    "polygons_risk"
  )

  expect_false(
    any(
      legacy_objects %in% names(x)
    )
  )
})


# 05 - Test reproducibility -------------------------------------------------

test_that("basic scenario example is reproducible", {

  x1 <- rgr_example_data(
    "basic_scenario"
  )

  x2 <- rgr_example_data(
    "basic_scenario"
  )

  expect_equal(
    x1,
    x2
  )
})


# 06 - Test unavailable Cottesloe demo -------------------------------------

test_that("rgr_example_data rejects unavailable Cottesloe demo", {

  expect_error(
    rgr_example_data(
      "cottesloe_demo"
    ),
    "not yet bundled"
  )
})
