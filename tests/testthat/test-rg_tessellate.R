# 01 — Square tessellation --------------------------------------------------

test_that("rg_tessellate generates square cells in synthetic XY space", {

  study <- rg_study_area()

  x <- rg_tessellate(
    study,
    cell_size = 500,
    shape = "square"
  )

  expect_s3_class(x, "sf")
  expect_equal(nrow(x), 100L)
  expect_true(is.na(sf::st_crs(x)))

  expect_true(
    all(as.character(sf::st_geometry_type(x)) == "POLYGON")
  )

  expect_true(all(x$shape == "square"))
  expect_true(all(x$cell_size == 500))
  expect_true(all(x$area > 0))
})


# 02 — Hex tessellation -----------------------------------------------------

test_that("rg_tessellate generates hexagonal cells in synthetic XY space", {

  study <- rg_study_area()

  x <- rg_tessellate(
    study,
    cell_size = 500,
    shape = "hex"
  )

  expect_s3_class(x, "sf")
  expect_true(nrow(x) > 0L)
  expect_true(is.na(sf::st_crs(x)))

  expect_true(
    all(as.character(sf::st_geometry_type(x)) == "POLYGON")
  )

  expect_true(all(x$shape == "hex"))
  expect_true(all(x$cell_size == 500))
  expect_true(all(x$area > 0))
})


# 03 — Square cell dimensions ----------------------------------------------

test_that("square tessellation uses requested cell size", {

  study <- rg_study_area()

  x <- rg_tessellate(
    study,
    cell_size = 500,
    shape = "square"
  )

  expect_equal(
    unique(x$area),
    250000
  )
})


# 04 — Cell identifiers -----------------------------------------------------

test_that("rg_tessellate creates unique sequential identifiers", {

  study <- rg_study_area()

  square <- rg_tessellate(
    study,
    cell_size = 500,
    shape = "square"
  )

  hex <- rg_tessellate(
    study,
    cell_size = 500,
    shape = "hex"
  )

  expect_equal(
    square$cell_id,
    sprintf(
      "square_%06d",
      seq_len(nrow(square))
    )
  )

  expect_equal(
    hex$cell_id,
    sprintf(
      "hex_%06d",
      seq_len(nrow(hex))
    )
  )

  expect_equal(
    length(unique(square$cell_id)),
    nrow(square)
  )

  expect_equal(
    length(unique(hex$cell_id)),
    nrow(hex)
  )
})


# 05 — Default overhang -----------------------------------------------------

test_that("unclipped tessellation retains complete boundary cells", {

  study_geom <- sf::st_buffer(
    sf::st_sfc(
      sf::st_point(c(2500, 2500))
    ),
    dist = 2200
  )

  study <- sf::st_sf(
    area_id = "study_001",
    geometry = study_geom
  )

  x <- rg_tessellate(
    study,
    cell_size = 500,
    shape = "square",
    clip = FALSE
  )

  cell_areas <- x$area

  expect_equal(
    length(unique(cell_areas)),
    1L
  )

  expect_equal(
    unique(cell_areas),
    250000
  )
})


# 06 — Clipped square tessellation -----------------------------------------

test_that("square tessellation supports clipped output", {

  study_geom <- sf::st_buffer(
    sf::st_sfc(
      sf::st_point(c(2500, 2500))
    ),
    dist = 2200
  )

  study <- sf::st_sf(
    area_id = "study_001",
    geometry = study_geom
  )

  x <- rg_tessellate(
    study,
    cell_size = 500,
    shape = "square",
    clip = TRUE
  )

  expect_s3_class(x, "sf")
  expect_true(nrow(x) > 0L)
  expect_true(all(sf::st_is_valid(x)))
  expect_false(any(sf::st_is_empty(x)))

  expect_true(
    all(x$area > 0)
  )

  expect_true(
    any(x$area < 250000)
  )
})


# 07 — Clipped hex tessellation --------------------------------------------

test_that("hex tessellation supports clipped output", {

  study_geom <- sf::st_buffer(
    sf::st_sfc(
      sf::st_point(c(2500, 2500))
    ),
    dist = 2200
  )

  study <- sf::st_sf(
    area_id = "study_001",
    geometry = study_geom
  )

  x <- rg_tessellate(
    study,
    cell_size = 500,
    shape = "hex",
    clip = TRUE
  )

  expect_s3_class(x, "sf")
  expect_true(nrow(x) > 0L)
  expect_true(all(sf::st_is_valid(x)))
  expect_false(any(sf::st_is_empty(x)))

  expect_true(
    all(x$area > 0)
  )
})


# 08 — Projected CRS --------------------------------------------------------

test_that("rg_tessellate preserves projected CRS", {

  study <- rg_study_area(
    bbox = c(
      xmin = 300000,
      ymin = 5800000,
      xmax = 305000,
      ymax = 5805000
    ),
    crs = 7855
  )

  x <- rg_tessellate(
    study,
    cell_size = 500,
    shape = "hex"
  )

  expect_equal(
    sf::st_crs(x)$epsg,
    7855
  )
})


# 09 — Geometry validity ----------------------------------------------------

test_that("rg_tessellate returns valid non-empty geometry", {

  study <- rg_study_area()

  square <- rg_tessellate(
    study,
    shape = "square"
  )

  hex <- rg_tessellate(
    study,
    shape = "hex"
  )

  expect_true(all(sf::st_is_valid(square)))
  expect_true(all(sf::st_is_valid(hex)))

  expect_false(any(sf::st_is_empty(square)))
  expect_false(any(sf::st_is_empty(hex)))
})


# 10 — Output schema --------------------------------------------------------

test_that("rg_tessellate returns standard output fields", {

  study <- rg_study_area()

  x <- rg_tessellate(study)

  expect_equal(
    names(x),
    c(
      "cell_id",
      "shape",
      "cell_size",
      "area",
      "geometry"
    )
  )
})


# 11 — Reject invalid cell size --------------------------------------------

test_that("rg_tessellate rejects invalid cell_size", {

  study <- rg_study_area()

  expect_error(
    rg_tessellate(
      study,
      cell_size = 0
    ),
    "cell_size must be greater than 0"
  )

  expect_error(
    rg_tessellate(
      study,
      cell_size = -500
    ),
    "cell_size must be greater than 0"
  )

  expect_error(
    rg_tessellate(
      study,
      cell_size = NA_real_
    ),
    "cell_size must be greater than 0"
  )
})


# 12 — Reject invalid shape -------------------------------------------------

test_that("rg_tessellate rejects unsupported shapes", {

  study <- rg_study_area()

  expect_error(
    rg_tessellate(
      study,
      shape = "triangle"
    )
  )
})


# 13 — Reject invalid clip --------------------------------------------------

test_that("rg_tessellate rejects invalid clip values", {

  study <- rg_study_area()

  expect_error(
    rg_tessellate(
      study,
      clip = NA
    ),
    "clip must be TRUE or FALSE"
  )

  expect_error(
    rg_tessellate(
      study,
      clip = "yes"
    ),
    "clip must be TRUE or FALSE"
  )
})


# 14 — Reject non-sf study area --------------------------------------------

test_that("rg_tessellate rejects non-sf study areas", {

  expect_error(
    rg_tessellate(
      data.frame(
        x = 1,
        y = 1
      )
    ),
    "study_area must be an sf object"
  )
})