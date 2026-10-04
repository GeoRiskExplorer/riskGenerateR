# 01 - Modify topology for stress testing

#' Modify polygon topology for QA and stress testing
#'
#' @param x An sf polygon object.
#' @param mode Topology modification mode. Currently only `"overlap"`.
#' @param pct Proportion of features to modify.
#' @param distance Buffer distance in map units.
#' @param seed Optional random seed.
#'
#' @return An sf object with intentionally modified topology.
#' @export
rgr_topology_modify <- function(
  x,
  mode = "overlap",
  pct = 0.1,
  distance = 25,
  seed = NULL
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.", call. = FALSE)
  }

  if (!inherits(x, "sf")) {
    stop("x must be an sf object.", call. = FALSE)
  }

  if (!mode %in% "overlap") {
    stop("Currently only mode = 'overlap' is supported.", call. = FALSE)
  }

  if (pct <= 0 || pct > 1) {
    stop("pct must be greater than 0 and less than or equal to 1.", call. = FALSE)
  }

  if (distance <= 0) {
    stop("distance must be greater than 0.", call. = FALSE)
  }

  geom_type <- unique(as.character(sf::st_geometry_type(x)))

  if (!all(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("x must contain polygon geometries.", call. = FALSE)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  out <- x

  n_modify <- max(1, round(nrow(out) * pct))
  modify_idx <- sample(seq_len(nrow(out)), size = n_modify)

  if (mode == "overlap") {
    geom <- sf::st_geometry(out)

    geom[modify_idx] <- sf::st_buffer(
      geom[modify_idx],
      dist = distance
    )

    sf::st_geometry(out) <- geom
  }

  out$topology_type <- mode
  out$topology_modified <- seq_len(nrow(out)) %in% modify_idx
  out$topology_distance <- ifelse(out$topology_modified, distance, 0)

  out
}

