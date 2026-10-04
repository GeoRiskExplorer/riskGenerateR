# 01 - Print rgr_summary objects

#' Print rgr_summary objects
#'
#' Prints a compact quality-assurance and descriptive summary produced by
#' `rgr_summary()`.
#'
#' @param x An rgr_summary object.
#' @param ... Unused.
#'
#' @return Invisibly returns x.
#'
#' @export
print.rgr_summary <- function(x, ...) {

  # 01 - Core spatial summary ----------------------------------------------

  cat("\n--- riskGenerateR summary ---\n")
  cat("Rows:", x$rows, "\n")
  cat("Columns:", x$columns, "\n")
  cat(
    "Geometry types:",
    paste(x$geometry_types, collapse = ", "),
    "\n"
  )
  cat("CRS EPSG:", x$crs_epsg, "\n")
  cat("CRS name:", x$crs_name, "\n")
  cat("Valid geometries:", x$valid_count, "\n")
  cat("Invalid geometries:", x$invalid_count, "\n")
  cat("Empty geometries:", x$empty_count, "\n")


  # 02 - Attribute-generation metadata -------------------------------------

  if (!is.null(x$attribute_type_count)) {
    cat("\nAttribute type count:\n")
    print(x$attribute_type_count)
  }


  # 03 - Count summaries ----------------------------------------------------

  if (!is.null(x$total_event_count)) {
    cat(
      "\nTotal event count:",
      x$total_event_count,
      "\n"
    )
  }

  if (!is.null(x$event_count_summary)) {
    cat("\nEvent count summary:\n")
    print(x$event_count_summary)
  }

  if (!is.null(x$total_count)) {
    cat(
      "\nTotal count:",
      x$total_count,
      "\n"
    )
  }

  if (!is.null(x$count_summary)) {
    cat("\nCount summary:\n")
    print(x$count_summary)
  }


  # 04 - Area summary -------------------------------------------------------

  if (!is.null(x$total_area)) {
    cat(
      "\nTotal area:",
      x$total_area,
      "\n"
    )
  }

  if (!is.null(x$area_summary)) {
    cat("\nArea summary:\n")
    print(x$area_summary)
  }


  # 05 - Contextual categorical summaries ----------------------------------

  if (!is.null(x$hazard_category_count)) {
    cat("\nHazard category count:\n")
    print(x$hazard_category_count)
  }

  if (!is.null(x$event_type_count)) {
    cat("\nEvent type count:\n")
    print(x$event_type_count)
  }

  if (!is.null(x$observation_type_count)) {
    cat("\nObservation type count:\n")
    print(x$observation_type_count)
  }

  if (!is.null(x$risk_category_count)) {
    cat("\nRisk category count:\n")
    print(x$risk_category_count)
  }

  if (!is.null(x$category_count)) {
    cat("\nCategory count:\n")
    print(x$category_count)
  }


  # 06 - Return -------------------------------------------------------------

  invisible(x)
}
