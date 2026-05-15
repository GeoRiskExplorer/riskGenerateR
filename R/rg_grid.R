# 01 — Generate square grid polygons

#' Generate a square grid over a study area
#'
#' @param study_area An sf polygon object.
#' @param cell_size Grid cell size in map units.
#' @param clip Logical. Should the grid be clipped to the study area? Defaults to FALSE.
#'
#' @return An sf polygon grid.
#' @export
rg_grid <- function(
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

  grid_geom <- sf::st_make_grid(
    study_geom,
    cellsize = cell_size,
    square = TRUE
  )

  grid_sf <- sf::st_sf(
    grid_id = sprintf("grid_%06d", seq_along(grid_geom)),
    geometry = grid_geom
  )

  sf::st_crs(grid_sf) <- sf::st_crs(study_area)

  if (clip) {
    grid_sf <- suppressWarnings(
      sf::st_intersection(
        grid_sf,
        sf::st_geometry(study_geom)
      )
    )

    grid_sf <- suppressWarnings(
      sf::st_collection_extract(grid_sf, "POLYGON")
    )

    grid_sf <- grid_sf[!sf::st_is_empty(grid_sf), ]

    grid_sf$grid_id <- sprintf("grid_%06d", seq_len(nrow(grid_sf)))
  }

  grid_sf$area_m2 <- as.numeric(sf::st_area(grid_sf))
  grid_sf$area_km2 <- round(grid_sf$area_m2 / 1e6, 4)

 grid_sf
}