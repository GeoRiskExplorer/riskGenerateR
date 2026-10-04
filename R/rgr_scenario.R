# 01 - Generate bundled synthetic scenarios

#' Generate a bundled synthetic scenario
#'
#' Generates a reproducible synthetic spatial scenario containing a study
#' area, spatial features, and contextual synthetic attributes suitable for
#' development, testing, demonstration, and downstream analysis.
#'
#' `rgr_scenario()` generates synthetic inputs. It does not calculate risk,
#' rates, expected counts, priorities, or other analytical outputs.
#'
#' @param scenario Character. One of `"basic"`, `"square_grid"`,
#'   `"hex_grid"`, or `"irregular_polygons"`.
#' @param bbox_name Example bbox name passed to `rgr_bbox_example()`.
#' @param n_points Number of points to generate.
#' @param inside_pct Proportion of points generated inside the study area.
#' @param cell_size Tessellation cell size in map units.
#' @param seed Optional random seed.
#'
#' @return A named list containing scenario metadata and synthetic sf objects.
#'
#' @export
rgr_scenario <- function(
  scenario = "basic",
  bbox_name = "wa_outback",
  n_points = 100,
  inside_pct = 0.9,
  cell_size = 500,
  seed = 123
) {

  # 01 - Validate scenario --------------------------------------------------

  scenario_options <- c(
    "basic",
    "square_grid",
    "hex_grid",
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


  # 02 - Generate study area ------------------------------------------------

  ex <- rgr_bbox_example(
    bbox_name
  )

  study_area <- rgr_study_area(
    bbox = ex$bbox,
    crs = ex$crs,
    area_id = paste0(
      bbox_name,
      "_study_area"
    ),
    area_name = ex$description
  )


  # 03 - Generate point support --------------------------------------------

  points <- rgr_points(
    study_area = study_area,
    n = n_points,
    inside_pct = inside_pct,
    outside_distance = cell_size * 2,
    seed = seed
  )


  # 04 - Generate incident fixture -----------------------------------------

  incidents <- rgr_add_attributes(
    points,
    type = "incident",
    seed = seed + 1
  )


  # 05 - Initialise scenario output ----------------------------------------

  out <- list(
    scenario = scenario,
    bbox = ex,
    study_area = study_area,
    points = points,
    incidents = incidents
  )


  # 06 - Generate square tessellation --------------------------------------

  if (
    scenario %in%
      c(
        "square_grid",
        "basic"
      )
  ) {

    grid <- rgr_tessellate(
      study_area = study_area,
      cell_size = cell_size,
      shape = "square",
      clip = FALSE
    )

    grid_attributes <- rgr_add_attributes(
      grid,
      type = "generic",
      seed = seed + 2
    )

    out$grid <- grid
    out$grid_attributes <- grid_attributes
  }


  # 07 - Generate hexagonal tessellation -----------------------------------

  if (
    scenario %in%
      c(
        "hex_grid",
        "basic"
      )
  ) {

    hex <- rgr_tessellate(
      study_area = study_area,
      cell_size = cell_size,
      shape = "hex",
      clip = FALSE
    )

    hex_attributes <- rgr_add_attributes(
      hex,
      type = "generic",
      seed = seed + 3
    )

    out$hex <- hex
    out$hex_attributes <- hex_attributes
  }


  # 08 - Generate irregular polygons ---------------------------------------

  if (
    identical(
      scenario,
      "irregular_polygons"
    )
  ) {

    polygons <- rgr_polygons(
      study_area = study_area,
      target_n = 20,
      cell_size = cell_size / 2,
      seed = seed
    )

    polygon_attributes <- rgr_add_attributes(
      polygons,
      type = "generic",
      seed = seed + 4
    )

    out$polygons <- polygons
    out$polygon_attributes <- polygon_attributes
  }


  # 09 - Return -------------------------------------------------------------

  out
}
