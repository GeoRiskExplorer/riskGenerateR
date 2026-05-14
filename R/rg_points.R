# 01 — Generate random points

#' Generate random points inside and outside a study area
#'
#' @param study_area An sf polygon object.
#' @param n Number of points to generate.
#' @param inside_pct Proportion of points generated inside the study area. Defaults to 0.
#' @param outside_distance Distance in map units used to expand the outside generation area.
#' @param seed Optional random seed.
#'
#' @return An sf point object.
#' @export
rg_points <- function(
  study_area,
  n = 100,
  inside_pct = 0,
  outside_distance = 1000,
  seed = NULL
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.", call. = FALSE)
  }

  if (!inherits(study_area, "sf")) {
    stop("study_area must be an sf object.", call. = FALSE)
  }

  if (n <= 0) {
    stop("n must be greater than 0.", call. = FALSE)
  }

  if (inside_pct < 0 || inside_pct > 1) {
    stop("inside_pct must be between 0 and 1.", call. = FALSE)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  n_inside <- round(n * inside_pct)
  n_outside <- n - n_inside

  study_union <- sf::st_union(study_area)

  inside_pts <- NULL
  outside_pts <- NULL

  if (n_inside > 0) {
    inside_pts <- sf::st_sample(
      study_union,
      size = n_inside,
      type = "random"
    )
  }

  if (n_outside > 0) {
    outer_area <- sf::st_buffer(study_union, dist = outside_distance)
    outside_ring <- sf::st_difference(outer_area, study_union)

    outside_pts <- sf::st_sample(
      outside_ring,
      size = n_outside,
      type = "random"
    )
  }

  geom <- c(inside_pts, outside_pts)

  out <- sf::st_sf(
    point_id = sprintf("pt_%06d", seq_along(geom)),
    inside_flag = c(
      rep(TRUE, n_inside),
      rep(FALSE, n_outside)
    ),
    generation_method = "random",
    geometry = geom
  )

  sf::st_crs(out) <- sf::st_crs(study_area)

  out
}