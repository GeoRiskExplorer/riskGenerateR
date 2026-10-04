# 01 - Test bundled synthetic scenarios

test_that("rgr_scenario basic returns expected objects", {

  x <- rgr_scenario(
    scenario = "basic",
    seed = 123
  )

  expect_type(x, "list")
  expect_equal(x$scenario, "basic")

  expect_s3_class(x$study_area, "sf")
  expect_s3_class(x$points, "sf")
  expect_s3_class(x$incidents, "sf")
  expect_s3_class(x$grid, "sf")
  expect_s3_class(x$grid_attributes, "sf")
  expect_s3_class(x$hex, "sf")
  expect_s3_class(x$hex_attributes, "sf")

  expect_equal(nrow(x$points), 100)
  expect_equal(sum(x$points[["inside_flag"]]), 90)

  expect_equal(
    nrow(x$incidents),
    nrow(x$points)
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


# 02 - Test square-grid scenario -------------------------------------------

test_that("rgr_scenario square_grid returns grid fixture only", {

  x <- rgr_scenario(
    scenario = "square_grid",
    seed = 123
  )

  expect_equal(
    x$scenario,
    "square_grid"
  )

  expect_true("grid" %in% names(x))
  expect_true("grid_attributes" %in% names(x))

  expect_false("hex" %in% names(x))
  expect_false("hex_attributes" %in% names(x))

  expect_false("polygons" %in% names(x))
  expect_false("polygon_attributes" %in% names(x))

  expect_s3_class(x$grid, "sf")
  expect_s3_class(x$grid_attributes, "sf")

  expect_equal(
    nrow(x$grid),
    nrow(x$grid_attributes)
  )

  expect_true(
    all(
      x$grid_attributes[["rgr_attribute_type"]] ==
        "generic"
    )
  )
})


# 03 - Test hex-grid scenario ----------------------------------------------

test_that("rgr_scenario hex_grid returns hex fixture only", {

  x <- rgr_scenario(
    scenario = "hex_grid",
    seed = 123
  )

  expect_equal(
    x$scenario,
    "hex_grid"
  )

  expect_true("hex" %in% names(x))
  expect_true("hex_attributes" %in% names(x))

  expect_false("grid" %in% names(x))
  expect_false("grid_attributes" %in% names(x))

  expect_false("polygons" %in% names(x))
  expect_false("polygon_attributes" %in% names(x))

  expect_s3_class(x$hex, "sf")
  expect_s3_class(x$hex_attributes, "sf")

  expect_equal(
    nrow(x$hex),
    nrow(x$hex_attributes)
  )

  expect_true(
    all(
      x$hex_attributes[["rgr_attribute_type"]] ==
        "generic"
    )
  )
})


# 04 - Test irregular-polygon scenario -------------------------------------

test_that("rgr_scenario returns irregular polygon fixture", {

  x <- rgr_scenario(
    scenario = "irregular_polygons",
    seed = 123
  )

  expect_type(x, "list")

  expect_equal(
    x$scenario,
    "irregular_polygons"
  )

  expect_true("study_area" %in% names(x))
  expect_true("points" %in% names(x))
  expect_true("incidents" %in% names(x))
  expect_true("polygons" %in% names(x))
  expect_true("polygon_attributes" %in% names(x))

  expect_false("grid" %in% names(x))
  expect_false("grid_attributes" %in% names(x))
  expect_false("hex" %in% names(x))
  expect_false("hex_attributes" %in% names(x))

  expect_s3_class(x$polygons, "sf")
  expect_s3_class(x$polygon_attributes, "sf")

  expect_true(
    nrow(x$polygons) > 1
  )

  expect_true(
    all(
      sf::st_is_valid(x$polygons)
    )
  )

  expect_equal(
    sum(
      sf::st_is_empty(x$polygons)
    ),
    0
  )

  expect_true(
    all(
      sf::st_geometry_type(x$polygons) %in%
        c(
          "POLYGON",
          "MULTIPOLYGON"
        )
    )
  )

  expect_equal(
    nrow(x$polygons),
    nrow(x$polygon_attributes)
  )

  expect_true(
    all(
      x$polygon_attributes[["rgr_attribute_type"]] ==
        "generic"
    )
  )
})


# 05 - Test incident fixture ------------------------------------------------

test_that("rgr_scenario always includes coherent incident records", {

  x <- rgr_scenario(
    scenario = "basic",
    n_points = 50,
    seed = 123
  )

  expect_equal(
    nrow(x$incidents),
    50
  )

  expect_equal(
    nrow(x$incidents),
    nrow(x$points)
  )

  expect_true(
    all(
      c(
        "event_id",
        "event_type",
        "hazard_category",
        "mechanism",
        "activity",
        "consequence",
        "event_date",
        "event_hour",
        "event_count"
      ) %in% names(x$incidents)
    )
  )

  expect_true(
    all(
      x$incidents[["rgr_attribute_type"]] ==
        "incident"
    )
  )

  expect_true(
    all(
      x$incidents[["event_count"]] == 1L
    )
  )
})


# 06 - Test spatial geometry preservation ----------------------------------

test_that("rgr_scenario contextual fixtures preserve source geometry", {

  x <- rgr_scenario(
    scenario = "basic",
    seed = 123
  )

  expect_equal(
    sf::st_geometry(x$incidents),
    sf::st_geometry(x$points)
  )

  expect_equal(
    sf::st_geometry(x$grid_attributes),
    sf::st_geometry(x$grid)
  )

  expect_equal(
    sf::st_geometry(x$hex_attributes),
    sf::st_geometry(x$hex)
  )

  expect_equal(
    sf::st_crs(x$incidents),
    sf::st_crs(x$points)
  )

  expect_equal(
    sf::st_crs(x$grid_attributes),
    sf::st_crs(x$grid)
  )

  expect_equal(
    sf::st_crs(x$hex_attributes),
    sf::st_crs(x$hex)
  )
})


# 07 - Test absence of legacy objects --------------------------------------

test_that("rgr_scenario does not return legacy risk objects", {

  x <- rgr_scenario(
    scenario = "basic",
    seed = 123
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


# 08 - Test absence of analytical outputs ----------------------------------

test_that("rgr_scenario does not generate analytical risk outputs", {

  x <- rgr_scenario(
    scenario = "basic",
    seed = 123
  )

  analytical_fields <- c(
    "risk_score",
    "risk_class",
    "risk_rating",
    "expected_count",
    "rate_per_1000",
    "smr",
    "priority"
  )

  scenario_sf <- x[
    vapply(
      x,
      inherits,
      logical(1),
      what = "sf"
    )
  ]

  has_analytical_fields <- vapply(
    scenario_sf,
    function(obj) {
      any(
        analytical_fields %in% names(obj)
      )
    },
    logical(1)
  )

  expect_false(
    any(has_analytical_fields)
  )
})


# 09 - Test reproducibility -------------------------------------------------

test_that("rgr_scenario is reproducible", {

  x1 <- rgr_scenario(
    scenario = "basic",
    seed = 123
  )

  x2 <- rgr_scenario(
    scenario = "basic",
    seed = 123
  )

  expect_equal(
    x1,
    x2
  )
})


# 10 - Test scenario names --------------------------------------------------

test_that("rgr_scenario supports the documented scenario names", {

  scenario_names <- c(
    "basic",
    "square_grid",
    "hex_grid",
    "irregular_polygons"
  )

  for (scenario_name in scenario_names) {

    expect_no_error(
      rgr_scenario(
        scenario = scenario_name,
        seed = 123
      )
    )
  }
})


# 11 - Test invalid scenario ------------------------------------------------

test_that("rgr_scenario rejects unknown scenario", {

  expect_error(
    rgr_scenario(
      scenario = "banana"
    ),
    "scenario must be one of"
  )
})
