# 01 — Generate bundled synthetic scenarios

#' Generate a bundled synthetic risk scenario
#'
#' Generates a bundled synthetic scenario containing a study area, points,
#' optional regular tessellations, and synthetic risk attributes.
#'
#' @param scenario Character. One of `"basic"`, `"grid_counts"`,
#'   `"hex_counts"`, or `"irregular_polygons"`.
#' @param bbox_name Example bbox name passed to `rg_bbox_example()`.
#' @param n_points Number of points to generate.
#' @param inside_pct Proportion of points generated inside the study area.
#' @param cell_size Tessellation cell size in map units.
#' @param seed Optional random seed.
#'
#' @return A named list of sf objects and scenario metadata.
#'
#' @export
rg_scenario <- function(
  scenario = "basic",
  bbox_name = "wa_outback",
  n_points = 100,
  inside_pct = 0.9,
  cell_size = 500,
  seed = 123
) {

  # 01 — Validate scenario ---------------------------------------------------

  scenario_options <- c(
    "basic",
    "grid_counts",
    "hex_counts",
    "irregular_polygons"
  )

  if (!scenario %in% scenario_options) {
    stop(
      "scenario must be one of: ",
      paste(
        scenario_options,
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }


  # 02 — Generate study area -------------------------------------------------

  ex <- rg_bbox_example(
    bbox_name
  )

  study_area <- rg_study_area(
    bbox = ex$bbox,
    crs = ex$crs,
    area_id = paste0(
      bbox_name,
      "_study_area"
    ),
    area_name = ex$description
  )


  # 03 — Generate points -----------------------------------------------------

  points <- rg_points(
    study_area = study_area,
    n = n_points,
    inside_pct = inside_pct,
    outside_distance = cell_size * 2,
    seed = seed
  )

  points_risk <- rg_add_risk_attributes(
    points,
    seed = seed
  )


  # 04 — Initialise scenario output -----------------------------------------

  out <- list(
    scenario = scenario,
    bbox = ex,
    study_area = study_area,
    points = points,
    points_risk = points_risk
  )


  # 05 — Generate square tessellation ---------------------------------------

  if (
    scenario %in%
      c(
        "grid_counts",
        "basic"
      )
  ) {

    grid <- rg_tessellate(
      study_area = study_area,
      cell_size = cell_size,
      shape = "square",
      clip = FALSE
    )

    grid_risk <- rg_add_risk_attributes(
      grid,
      seed = seed + 1
    )

    out$grid <- grid
    out$grid_risk <- grid_risk
  }


  # 06 — Generate hexagonal tessellation ------------------------------------

  if (
    scenario %in%
      c(
        "hex_counts",
        "basic"
      )
  ) {

    hex <- rg_tessellate(
      study_area = study_area,
      cell_size = cell_size,
      shape = "hex",
      clip = FALSE
    )

    hex_risk <- rg_add_risk_attributes(
      hex,
      seed = seed + 2
    )

    out$hex <- hex
    out$hex_risk <- hex_risk
  }


  # 07 — Generate irregular polygons ----------------------------------------

  if (
    identical(
      scenario,
      "irregular_polygons"
    )
  ) {

    polygons <- rg_polygons(
      study_area = study_area,
      target_n = 20,
      cell_size = cell_size / 2,
      seed = seed
    )

    polygons_risk <- rg_add_risk_attributes(
      polygons,
      seed = seed + 3
    )

    out$polygons <- polygons
    out$polygons_risk <- polygons_risk
  }


  # 08 — Return --------------------------------------------------------------

  out
}