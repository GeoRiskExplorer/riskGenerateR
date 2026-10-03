# 01 — Generate regular tessellations

#' Generate a regular synthetic tessellation
#'
#' Generates square or hexagonal polygon cells covering a supplied study
#' area. Cells may either retain their complete geometry or be clipped to
#' the study-area boundary.
#'
#' The function supports both neutral synthetic XY coordinate space and
#' study areas with a defined coordinate reference system.
#'
#' @param study_area An sf polygon object.
#' @param cell_size Cell size in coordinate units. For projected data these
#'   are the map units of the CRS. For neutral synthetic XY data these are
#'   synthetic coordinate units.
#' @param shape Tessellation shape. One of `"square"` or `"hex"`.
#' @param clip Logical. Should cells be clipped to the study-area boundary?
#'   Defaults to `FALSE`.
#'
#' @return An sf polygon object containing tessellation cells.
#'
#' @export
rg_tessellate <- function(
  study_area,
  cell_size = 500,
  shape = c("square", "hex"),
  clip = FALSE
) {

  # 01 — Validate inputs -----------------------------------------------------

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.", call. = FALSE)
  }

  if (!inherits(study_area, "sf")) {
    stop("study_area must be an sf object.", call. = FALSE)
  }

  if (
    length(cell_size) != 1L ||
    !is.numeric(cell_size) ||
    is.na(cell_size) ||
    !is.finite(cell_size) ||
    cell_size <= 0
  ) {
    stop("cell_size must be greater than 0.", call. = FALSE)
  }

  if (
    length(clip) != 1L ||
    !is.logical(clip) ||
    is.na(clip)
  ) {
    stop("clip must be TRUE or FALSE.", call. = FALSE)
  }

  shape <- match.arg(shape)


  # 02 — Prepare study geometry ---------------------------------------------

  study_area <- sf::st_make_valid(
    study_area
  )

  study_geom <- sf::st_union(
    study_area
  )

  study_geom <- sf::st_make_valid(
    study_geom
  )


  # 03 — Generate tessellation ----------------------------------------------

  tessellation <- sf::st_make_grid(
    study_geom,
    cellsize = cell_size,
    square = identical(shape, "square")
  )

  if (length(tessellation) == 0L) {
    stop(
      "No tessellation cells were generated.",
      call. = FALSE
    )
  }

  out <- sf::st_sf(
    geometry = tessellation
  )

  sf::st_crs(out) <- sf::st_crs(
    study_area
  )


  # 04 — Retain cells intersecting study area -------------------------------

  intersects_study <- lengths(
    sf::st_intersects(
      out,
      study_geom
    )
  ) > 0L

  out <- out[
    intersects_study,
    ,
    drop = FALSE
  ]


  # 05 — Optionally clip cells ----------------------------------------------

  if (clip) {

    out <- suppressWarnings(
      sf::st_intersection(
        out,
        study_geom
      )
    )

    out <- sf::st_make_valid(
      out
    )

    out <- suppressWarnings(
      sf::st_collection_extract(
        out,
        "POLYGON"
      )
    )

    out <- out[
      !sf::st_is_empty(out),
      ,
      drop = FALSE
    ]
  }


  # 06 — Add generation metadata --------------------------------------------

  prefix <- if (
    identical(shape, "square")
  ) {
    "square"
  } else {
    "hex"
  }

  out[["cell_id"]] <- sprintf(
    "%s_%06d",
    prefix,
    seq_len(nrow(out))
  )

  out[["shape"]] <- shape
  out[["cell_size"]] <- cell_size

  out[["area"]] <- as.numeric(
    sf::st_area(out)
  )


  # 07 — Standardise output --------------------------------------------------

  out <- out[
    ,
    c(
      "cell_id",
      "shape",
      "cell_size",
      "area",
      "geometry"
    )
  ]


  # 08 — Return --------------------------------------------------------------

  out
}
