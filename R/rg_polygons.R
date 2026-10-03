# 01 — Generate irregular polygon units

#' Generate irregular synthetic polygon units
#'
#' Generates contiguous irregular polygon units within a supplied study area
#' using a seeded fine-grid allocation method.
#'
#' @param study_area An sf polygon object.
#' @param target_n Target number of polygon units.
#' @param cell_size Fine grid cell size in map units of the processing CRS.
#' @param method Polygon generation method. Currently only `"seeded_grid"`.
#' @param clip Logical. Should output polygons be clipped to the study area?
#' @param seed Optional random seed.
#' @param processing_crs Optional projected CRS used for geometry generation.
#' @param return_input_crs Logical. Return output to the input CRS?
#'   Defaults to `TRUE`.
#'
#' @return An sf polygon object.
#'
#' @export
rg_polygons <- function(
  study_area,
  target_n = 20,
  cell_size = 250,
  method = "seeded_grid",
  clip = TRUE,
  seed = NULL,
  processing_crs = NULL,
  return_input_crs = TRUE
) {

  # 01 — Validate inputs -----------------------------------------------------

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.", call. = FALSE)
  }

  if (!inherits(study_area, "sf")) {
    stop("study_area must be an sf object.", call. = FALSE)
  }

  if (!identical(method, "seeded_grid")) {
    stop(
      "Currently only method = 'seeded_grid' is supported.",
      call. = FALSE
    )
  }

  if (
    length(target_n) != 1L ||
    !is.numeric(target_n) ||
    is.na(target_n) ||
    target_n <= 0
  ) {
    stop("target_n must be greater than 0.", call. = FALSE)
  }

  if (
    length(cell_size) != 1L ||
    !is.numeric(cell_size) ||
    is.na(cell_size) ||
    cell_size <= 0
  ) {
    stop("cell_size must be greater than 0.", call. = FALSE)
  }

  target_n <- as.integer(target_n)

  input_crs <- sf::st_crs(study_area)

  if (is.na(input_crs)) {
    stop(
      "study_area must have a CRS for rg_polygons().",
      call. = FALSE
    )
  }


  # 02 — Determine processing CRS -------------------------------------------

  if (is.null(processing_crs)) {

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


  # 03 — Prepare study geometry ---------------------------------------------

  if (!is.null(seed)) {
    set.seed(seed)
  }

  study_area_proc <- sf::st_transform(
    study_area,
    processing_crs
  )

  study_area_proc <- sf::st_make_valid(
    study_area_proc
  )

  study_geom <- sf::st_union(
    study_area_proc
  )

  study_geom <- sf::st_make_valid(
    study_geom
  )


  # 04 — Generate fine polygon grid -----------------------------------------

  fine_grid <- rg_grid(
    sf::st_sf(geometry = study_geom),
    cell_size = cell_size,
    clip = TRUE
  )

  if (nrow(fine_grid) == 0L) {
    stop(
      "No fine-grid cells were generated. Check study_area and cell_size.",
      call. = FALSE
    )
  }


  # 05 — Generate polygon seed points ---------------------------------------

  seed_points <- sf::st_sample(
    study_geom,
    size = target_n,
    type = "random"
  )

  if (length(seed_points) == 0L) {
    stop(
      "No seed points were generated within the study area.",
      call. = FALSE
    )
  }

  seed_points_sf <- sf::st_sf(
    seed_group = seq_len(length(seed_points)),
    geometry = seed_points
  )

  sf::st_crs(seed_points_sf) <- sf::st_crs(
    study_area_proc
  )


  # 06 — Allocate fine-grid cells to nearest seed ---------------------------

  fine_grid_centroids <- sf::st_centroid(
    sf::st_geometry(fine_grid)
  )

  nearest_seed <- sf::st_nearest_feature(
    fine_grid_centroids,
    seed_points_sf
  )

  fine_grid[["seed_group"]] <-
    seed_points_sf[["seed_group"]][nearest_seed]


  # 07 — Dissolve cells into polygon units ----------------------------------

  poly_units <- fine_grid |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of("seed_group")
      )
    ) |>
    dplyr::summarise(
      source_cell_count = dplyr::n(),
      .groups = "drop"
    )

  poly_units <- sf::st_make_valid(
    poly_units
  )


  # 08 — Clip polygon units --------------------------------------------------

  if (clip) {

    poly_units <- suppressWarnings(
      sf::st_intersection(
        poly_units,
        sf::st_geometry(study_geom)
      )
    )

    poly_units <- suppressWarnings(
      sf::st_collection_extract(
        poly_units,
        "POLYGON"
      )
    )

    poly_units <- poly_units[
      !sf::st_is_empty(poly_units),
      ,
      drop = FALSE
    ]
  }


  # 09 — Add generation metadata --------------------------------------------

  poly_units[["poly_id"]] <- sprintf(
    "poly_%06d",
    seq_len(nrow(poly_units))
  )

  poly_units[["generation_method"]] <- method
  poly_units[["topology_type"]] <- "clean"

  poly_units[["area_m2"]] <- as.numeric(
    sf::st_area(poly_units)
  )

  poly_units[["area_km2"]] <- round(
    poly_units[["area_m2"]] / 1e6,
    4
  )


  # 10 — Standardise output fields ------------------------------------------

  poly_units <- poly_units |>
    dplyr::select(
      dplyr::all_of(
        c(
          "poly_id",
          "seed_group",
          "source_cell_count",
          "generation_method",
          "topology_type",
          "area_m2",
          "area_km2",
          "geometry"
        )
      )
    )


  # 11 — Return to input CRS -------------------------------------------------

  if (return_input_crs) {

    poly_units <- sf::st_transform(
      poly_units,
      input_crs
    )

    poly_units <- sf::st_make_valid(
      poly_units
    )

    poly_units <- suppressWarnings(
      sf::st_collection_extract(
        poly_units,
        "POLYGON"
      )
    )

    poly_units <- poly_units[
      !sf::st_is_empty(poly_units),
      ,
      drop = FALSE
    ]
  }


  # 12 — Return --------------------------------------------------------------

  poly_units
}