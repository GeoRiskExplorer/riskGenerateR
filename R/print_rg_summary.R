# 01 — Print rg_summary objects

#' Print rg_summary objects
#'
#' @param x An rg_summary object.
#' @param ... Unused.
#'
#' @return Invisibly returns x.
#' @export
print.rg_summary <- function(x, ...) {
  cat("\n--- riskGenerateR summary ---\n")
  cat("Rows:", x$rows, "\n")
  cat("Columns:", x$columns, "\n")
  cat("Geometry types:", paste(x$geometry_types, collapse = ", "), "\n")
  cat("CRS EPSG:", x$crs_epsg, "\n")
  cat("CRS name:", x$crs_name, "\n")
  cat("Valid geometries:", x$valid_count, "\n")
  cat("Invalid geometries:", x$invalid_count, "\n")
  cat("Empty geometries:", x$empty_count, "\n")

  if (!is.null(x$total_area_km2)) {
    cat("Total area km2:", round(x$total_area_km2, 4), "\n")
  }

  if (!is.null(x$total_event_count)) {
    cat("Total event count:", x$total_event_count, "\n")
  }

  if (!is.null(x$total_exposure_count)) {
    cat("Total exposure count:", x$total_exposure_count, "\n")
  }

  if (!is.null(x$total_expected_count)) {
    cat("Total expected count:", round(x$total_expected_count, 3), "\n")
  }

  if (!is.null(x$risk_class_count)) {
    cat("\nRisk class count:\n")
    print(x$risk_class_count)
  }

  if (!is.null(x$hazard_type_count)) {
    cat("\nHazard type count:\n")
    print(x$hazard_type_count)
  }

  if (!is.null(x$dominant_hazard_type_count)) {
    cat("\nDominant hazard type count:\n")
    print(x$dominant_hazard_type_count)
  }

  if (!is.null(x$event_count_summary)) {
    cat("\nEvent count summary:\n")
    print(x$event_count_summary)
  }

  if (!is.null(x$exposure_count_summary)) {
    cat("\nExposure count summary:\n")
    print(x$exposure_count_summary)
  }

  if (!is.null(x$rate_per_1000_summary)) {
    cat("\nRate per 1000 summary:\n")
    print(x$rate_per_1000_summary)
  }

  if (!is.null(x$area_km2_summary)) {
    cat("\nArea km2 summary:\n")
    print(x$area_km2_summary)
  }

  invisible(x)
}