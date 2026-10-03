# 01 — rg_study_area() default synthetic XY -------------------------------

test_that("rg_study_area creates default synthetic XY study area", {

  x <- rg_study_area()

  expect_s3_class(x, "sf")
  expect_equal(nrow(x), 1L)
  expect_true(is.na(sf::st_crs(x)))

  bbox <- sf::st_bbox(x)

  expect_equal(unname(bbox["xmin"]), 0)
  expect_equal(unname(bbox["ymin"]), 0)
  expect_equal(unname(bbox["xmax"]), 5000)
  expect_equal(unname(bbox["ymax"]), 5000)

  expect_equal(x$area_id, "study_area_001")
  expect_equal(x$area_name, "Synthetic Study Area")
})


# 02 — Custom numeric bbox -------------------------------------------------

test_that("rg_study_area accepts a custom named numeric bbox", {

  x <- rg_study_area(
    bbox = c(
      xmin = -2000,
      ymin = -1000,
      xmax = 8000,
      ymax = 6000
    ),
    area_id = "custom_001",
    area_name = "Custom Synthetic Study Area"
  )

  bbox <- sf::st_bbox(x)

  expect_equal(unname(bbox["xmin"]), -2000)
  expect_equal(unname(bbox["ymin"]), -1000)
  expect_equal(unname(bbox["xmax"]), 8000)
  expect_equal(unname(bbox["ymax"]), 6000)

  expect_true(is.na(sf::st_crs(x)))

  expect_equal(x$area_id, "custom_001")
  expect_equal(
    x$area_name,
    "Custom Synthetic Study Area"
  )
})


# 03 — sf bbox input -------------------------------------------------------

test_that("rg_study_area accepts sf bbox input and retains its CRS", {

  input_bbox <- sf::st_bbox(
    c(
      xmin = 300000,
      ymin = 5800000,
      xmax = 305000,
      ymax = 5805000
    ),
    crs = sf::st_crs(7855)
  )

  x <- rg_study_area(
    bbox = input_bbox
  )

  expect_s3_class(x, "sf")
  expect_equal(nrow(x), 1L)

  expect_equal(
    sf::st_crs(x)$epsg,
    7855
  )

  output_bbox <- sf::st_bbox(x)

  expect_equal(unname(output_bbox["xmin"]), 300000)
  expect_equal(unname(output_bbox["ymin"]), 5800000)
  expect_equal(unname(output_bbox["xmax"]), 305000)
  expect_equal(unname(output_bbox["ymax"]), 5805000)
})


# 04 — Explicit CRS --------------------------------------------------------

test_that("rg_study_area assigns an explicit CRS to numeric bbox input", {

  x <- rg_study_area(
    bbox = c(
      xmin = 300000,
      ymin = 5800000,
      xmax = 305000,
      ymax = 5805000
    ),
    crs = 7855
  )

  expect_equal(
    sf::st_crs(x)$epsg,
    7855
  )
})


# 05 — Explicit CRS overrides bbox CRS ------------------------------------

test_that("rg_study_area explicit CRS overrides bbox CRS", {

  input_bbox <- sf::st_bbox(
    c(
      xmin = 300000,
      ymin = 5800000,
      xmax = 305000,
      ymax = 5805000
    ),
    crs = sf::st_crs(7855)
  )

  x <- rg_study_area(
    bbox = input_bbox,
    crs = 7856
  )

  expect_equal(
    sf::st_crs(x)$epsg,
    7856
  )
})


# 06 — Geometry ------------------------------------------------------------

test_that("rg_study_area returns polygon geometry", {

  x <- rg_study_area()

  expect_true(
    all(
      as.character(sf::st_geometry_type(x)) == "POLYGON"
    )
  )

  expect_true(
    all(sf::st_is_valid(x))
  )

  expect_false(
    any(sf::st_is_empty(x))
  )
})


# 07 — Reproducibility -----------------------------------------------------

test_that("rg_study_area is deterministic", {

  x1 <- rg_study_area(
    bbox = c(
      xmin = 10,
      ymin = 20,
      xmax = 100,
      ymax = 200
    )
  )

  x2 <- rg_study_area(
    bbox = c(
      xmin = 10,
      ymin = 20,
      xmax = 100,
      ymax = 200
    )
  )

  expect_equal(x1, x2)
})


# 08 — Reject unnamed numeric bbox ----------------------------------------

test_that("rg_study_area rejects unnamed numeric bbox", {

  expect_error(
    rg_study_area(
      bbox = c(0, 0, 5000, 5000)
    ),
    "bbox must contain exactly four named values"
  )
})


# 09 — Reject incomplete bbox ---------------------------------------------

test_that("rg_study_area rejects incomplete bbox", {

  expect_error(
    rg_study_area(
      bbox = c(
        xmin = 0,
        ymin = 0,
        xmax = 5000
      )
    ),
    "bbox must contain exactly four named values"
  )
})


# 10 — Reject incorrectly named bbox --------------------------------------

test_that("rg_study_area rejects incorrectly named bbox", {

  expect_error(
    rg_study_area(
      bbox = c(
        left = 0,
        bottom = 0,
        right = 5000,
        top = 5000
      )
    ),
    "bbox must contain exactly four named values"
  )
})


# 11 — Reject non-numeric bbox --------------------------------------------

test_that("rg_study_area rejects non-numeric bbox", {

  expect_error(
    rg_study_area(
      bbox = c(
        xmin = "0",
        ymin = "0",
        xmax = "5000",
        ymax = "5000"
      )
    ),
    "bbox must be a named numeric vector or an sf bbox object"
  )
})


# 12 — Reject non-finite values -------------------------------------------

test_that("rg_study_area rejects non-finite bbox values", {

  expect_error(
    rg_study_area(
      bbox = c(
        xmin = 0,
        ymin = 0,
        xmax = Inf,
        ymax = 5000
      )
    ),
    "bbox values must all be finite numeric values"
  )

  expect_error(
    rg_study_area(
      bbox = c(
        xmin = 0,
        ymin = NA_real_,
        xmax = 5000,
        ymax = 5000
      )
    ),
    "bbox values must all be finite numeric values"
  )
})


# 13 — Reject invalid x extent --------------------------------------------

test_that("rg_study_area rejects invalid x extent", {

  expect_error(
    rg_study_area(
      bbox = c(
        xmin = 5000,
        ymin = 0,
        xmax = 0,
        ymax = 5000
      )
    ),
    "bbox xmax/ymax must be greater than xmin/ymin"
  )

  expect_error(
    rg_study_area(
      bbox = c(
        xmin = 5000,
        ymin = 0,
        xmax = 5000,
        ymax = 5000
      )
    ),
    "bbox xmax/ymax must be greater than xmin/ymin"
  )
})


# 14 — Reject invalid y extent --------------------------------------------

test_that("rg_study_area rejects invalid y extent", {

  expect_error(
    rg_study_area(
      bbox = c(
        xmin = 0,
        ymin = 5000,
        xmax = 5000,
        ymax = 0
      )
    ),
    "bbox xmax/ymax must be greater than xmin/ymin"
  )

  expect_error(
    rg_study_area(
      bbox = c(
        xmin = 0,
        ymin = 5000,
        xmax = 5000,
        ymax = 5000
      )
    ),
    "bbox xmax/ymax must be greater than xmin/ymin"
  )
})


# 15 — Numeric bbox name order --------------------------------------------

test_that("rg_study_area handles bbox names supplied in different order", {

  x <- rg_study_area(
    bbox = c(
      ymax = 5000,
      xmin = 0,
      xmax = 4000,
      ymin = 1000
    )
  )

  bbox <- sf::st_bbox(x)

  expect_equal(unname(bbox["xmin"]), 0)
  expect_equal(unname(bbox["ymin"]), 1000)
  expect_equal(unname(bbox["xmax"]), 4000)
  expect_equal(unname(bbox["ymax"]), 5000)
})