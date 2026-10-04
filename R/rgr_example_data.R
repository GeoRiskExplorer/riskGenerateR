# 01 - Built-in example datasets

#' Return built-in example datasets and scenarios
#'
#' Provides small built-in synthetic examples for package demonstrations,
#' documentation, development, and testing.
#'
#' @param name Character. Name of the example dataset or scenario. One of
#'   `"wa_outback"`, `"basic_scenario"`, or `"cottesloe_demo"`.
#'
#' @return An sf object or named list depending on the example selected.
#'
#' @export
rgr_example_data <- function(name = "wa_outback") {

  # 01 - Validate example name ---------------------------------------------

  name <- match.arg(
    name,
    choices = c(
      "wa_outback",
      "basic_scenario",
      "cottesloe_demo"
    )
  )


  # 02 - WA outback study area ---------------------------------------------

  if (
    identical(
      name,
      "wa_outback"
    )
  ) {

    ex <- rgr_bbox_example(
      "wa_outback"
    )

    return(
      rgr_study_area(
        bbox = ex$bbox,
        crs = ex$crs
      )
    )
  }


  # 03 - Basic synthetic scenario ------------------------------------------

  if (
    identical(
      name,
      "basic_scenario"
    )
  ) {

    return(
      rgr_scenario(
        scenario = "basic",
        seed = 123
      )
    )
  }


  # 04 - Reserved demonstration dataset ------------------------------------

  if (
    identical(
      name,
      "cottesloe_demo"
    )
  ) {

    stop(
      "cottesloe_demo not yet bundled into package.",
      call. = FALSE
    )
  }
}
