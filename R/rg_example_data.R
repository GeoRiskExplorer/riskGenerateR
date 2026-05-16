# 01 — Built-in example datasets

#' Return built-in example datasets and scenarios
#'
#' @param name Name of the example dataset or scenario.
#'
#' @return An sf object or list depending on the example selected.
#' @export
rg_example_data <- function(name = "wa_outback") {

  name <- match.arg(
    name,
    choices = c(
      "wa_outback",
      "basic_scenario",
      "cottesloe_demo"
    )
  )

  if (name == "wa_outback") {

    ex <- rg_bbox_example("wa_outback")

    return(
      rg_study_area(
        bbox = ex$bbox,
        crs = ex$crs
      )
    )
  }

  if (name == "basic_scenario") {

    return(
      rg_scenario(
        scenario = "basic",
        seed = 123
      )
    )
  }

  if (name == "cottesloe_demo") {

    stop(
      "cottesloe_demo not yet bundled into package.",
      call. = FALSE
    )
  }
}