# 01 - Generate random points

#' Generate random synthetic points
#'
#' Generates random points within a study area, with an optional proportion
#' generated outside the study-area boundary. The function supports both
#' neutral synthetic XY coordinate space and study areas with a defined CRS.
#'
#' @param study_area An sf polygon object.
#' @param n Number of points to generate.
#' @param inside_pct Proportion of points generated inside the study area.
#'   Defaults to `1`.
#' @param outside_distance Distance beyond the study-area boundary used when
#'   generating outside points. Units are the coordinate units of neutral
#'   synthetic XY space or the map units of the processing CRS.
#' @param seed Optional random seed.
#' @param processing_crs Optional projected CRS used for point generation when
#'   the input study area has a defined CRS.
#' @param return_input_crs Logical. Return output to the input CRS?
#'   Defaults to `TRUE`.
#'
#' @return An sf point object.
#'
#' @export
rgr_points <- function(
  study_area,
  n = 100,
  inside_pct = 1,
  outside_distance = 1000,
  seed = NULL,
  processing_crs = NULL,
  return_input_crs = TRUE
) {

  # 01 - Validate inputs -----------------------------------------------------

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.", call. = FALSE)
  }

  if (!inherits(study_area, "sf")) {
    stop("study_area must be an sf object.", call. = FALSE)
  }

  if (
    length(n) != 1L ||
    !is.numeric(n) ||
    is.na(n) ||
    !is.finite(n) ||
    n <= 0 ||
    n != as.integer(n)
  ) {
    stop("n must be a positive whole number.", call. = FALSE)
  }

  n <- as.integer(n)

  if (
    length(inside_pct) != 1L ||
    !is.numeric(inside_pct) ||
    is.na(inside_pct) ||
    !is.finite(inside_pct) ||
    inside_pct < 0 ||
    inside_pct > 1
  ) {
    stop("inside_pct must be between 0 and 1.", call. = FALSE)
  }

  if (
    length(outside_distance) != 1L ||
    !is.numeric(outside_distance) ||
    is.na(outside_distance) ||
    !is.finite(outside_distance) ||
    outside_distance <= 0
  ) {
    stop("outside_distance must be greater than 0.", call. = FALSE)
  }


  # 02 - Determine spatial mode ---------------------------------------------

  input_crs <- sf::st_crs(study_area)
  synthetic_xy <- is.na(input_crs)

  if (synthetic_xy && !is.null(processing_crs)) {
    stop(
      "processing_crs cannot be supplied when study_area has no CRS.",
      call. = FALSE
    )
  }

  if (!synthetic_xy && is.null(processing_crs)) {

    if (sf::st_is_longlat(study_area)) {

      processing_crs <- 3857

      message(
        "Geographic CRS detected. Using EPSG:3857 as temporary ",
        "processing CRS. For better local accuracy, supply ",
        "processing_crs explicitly."
      )

    } else {

      processing_crs <- input_crs
    }
  }


  # 03 - Set random seed -----------------------------------------------------

  if (!is.null(seed)) {
    set.seed(seed)
  }


  # 04 - Prepare study geometry ---------------------------------------------

  if (synthetic_xy) {

    study_area_proc <- sf::st_make_valid(
      study_area
    )

  } else {

    study_area_proc <- sf::st_transform(
      study_area,
      processing_crs
    )

    study_area_proc <- sf::st_make_valid(
      study_area_proc
    )
  }

  study_union <- sf::st_union(
    study_area_proc
  )

  study_union <- sf::st_make_valid(
    study_union
  )


  # 05 - Determine inside/outside counts ------------------------------------

  n_inside <- round(
    n * inside_pct
  )

  n_outside <- n - n_inside


  # 06 - Generate inside points ---------------------------------------------

  inside_pts <- NULL

  if (n_inside > 0L) {

    inside_pts <- sf::st_sample(
      study_union,
      size = n_inside,
      type = "random"
    )
  }


  # 07 - Generate outside points --------------------------------------------

  outside_pts <- NULL

  if (n_outside > 0L) {

    outer_area <- sf::st_buffer(
      study_union,
      dist = outside_distance
    )

    outside_ring <- sf::st_difference(
      outer_area,
      study_union
    )

    outside_pts <- sf::st_sample(
      outside_ring,
      size = n_outside,
      type = "random"
    )
  }


  # 08 - Combine point geometry ---------------------------------------------

  geom <- c(
    inside_pts,
    outside_pts
  )

  out <- sf::st_sf(
    point_id = sprintf(
      "pt_%06d",
      seq_along(geom)
    ),
    inside_flag = c(
      rep(TRUE, n_inside),
      rep(FALSE, n_outside)
    ),
    generation_method = "random",
    geometry = geom
  )

  sf::st_crs(out) <- sf::st_crs(
    study_area_proc
  )


  # 09 - Return to input CRS -------------------------------------------------

  if (!synthetic_xy && return_input_crs) {

    out <- sf::st_transform(
      out,
      input_crs
    )
  }


  # 10 - Return --------------------------------------------------------------

  out
}
