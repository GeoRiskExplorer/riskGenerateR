# 01 — Generate hexagon polygons

#' Generate a hexagon grid over a study area
#'
#' @param study_area An sf polygon object.
#' @param cell_size Approximate hexagon cell size in map units.
#' @param clip Logical. Should the hex grid be clipped to the study area? Defaults to FALSE.
#'
#' @return An sf polygon hex grid.
#' @export
rg_hex <- function(
  study_area,
  cell_size = 500,
  clip = FALSE
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.", call. = FALSE)
  }

  if (!inherits(study_area, "sf")) {
    stop("study_area must be an sf object.", call. = FALSE)
  }

  if (cell_size <= 0) {
    stop("cell_size must be greater than 0.", call. = FALSE)
  }

  study_geom <- sf::st_union(study_area)

  hex_geom <- sf::st_make_grid(
    study_geom,
    cellsize = cell_size,
    square = FALSE
  )

  hex_sf <- sf::st_sf(
    hex_id = sprintf("hex_%06d", seq_along(hex_geom)),
    geometry = hex_geom
  )

  sf::st_crs(hex_sf) <- sf::st_crs(study_area)

  if (clip) {
    hex_sf <- suppressWarnings(
      sf::st_intersection(
        hex_sf,
        sf::st_geometry(study_geom)
      )
    )

    hex_sf <- suppressWarnings(
      sf::st_collection_extract(hex_sf, "POLYGON")
    )

    hex_sf <- hex_sf[!sf::st_is_empty(hex_sf), ]

    hex_sf$hex_id <- sprintf("hex_%06d", seq_len(nrow(hex_sf)))
  }

  hex_sf$area_m2 <- as.numeric(sf::st_area(hex_sf))
hex_sf$area_km2 <- round(hex_sf$area_m2 / 1e6, 4)

hex_sf
}