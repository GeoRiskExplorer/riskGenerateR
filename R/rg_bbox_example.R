# 01 — Example bounding boxes

#' Return example bounding boxes for riskGenerateR
#'
#' @param name Example name. One of `"generic"`, `"wa_outback"`, or `"vicgrid"`.
#'
#' @return A list containing bbox, crs, name, and description.
#' @export
rg_bbox_example <- function(name = "generic") {
  examples <- list(
    generic = list(
      name = "generic",
      description = "Neutral synthetic 5 km by 5 km coordinate space with no CRS.",
      bbox = c(xmin = 0, ymin = 0, xmax = 5000, ymax = 5000),
      crs = NA
    ),
    wa_outback = list(
      name = "wa_outback",
      description = "Synthetic 5 km by 5 km projected bbox in inland Western Australia.",
      bbox = c(xmin = 240000, ymin = 7050000, xmax = 245000, ymax = 7055000),
      crs = 7851
    ),
    vicgrid = list(
      name = "vicgrid",
      description = "Synthetic 5 km by 5 km projected bbox using GDA2020 / Vicgrid.",
      bbox = c(xmin = 2500000, ymin = 2400000, xmax = 2505000, ymax = 2405000),
      crs = 7899
    )
  )

  if (!name %in% names(examples)) {
    stop(
      "Unknown bbox example. Use one of: ",
      paste(names(examples), collapse = ", "),
      call. = FALSE
    )
  }

  examples[[name]]
}