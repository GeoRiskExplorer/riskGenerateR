# 01 — Summarise generated spatial object

#' Summarise a generated riskGenerateR spatial object
#'
#' Provides a compact quality-assurance summary of a spatial object generated
#' by riskGenerateR. The summary reports object dimensions, geometry and CRS
#' information, geometry validity, and summaries of recognised generated
#' attributes when present.
#'
#' Area values are reported using the units represented by the object's
#' existing `area` field. No unit conversion is performed.
#'
#' @param x An sf object.
#'
#' @return An object of class `rg_summary`.
#'
#' @export
rg_summary <- function(x) {

  # 01 — Validate input -----------------------------------------------------

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "Package 'sf' is required.",
      call. = FALSE
    )
  }

  if (!inherits(x, "sf")) {
    stop(
      "x must be an sf object.",
      call. = FALSE
    )
  }


  # 02 — Spatial metadata ---------------------------------------------------

  geom_type <- unique(
    as.character(
      sf::st_geometry_type(x)
    )
  )

  crs <- sf::st_crs(x)


  # 03 — Core QA summary ----------------------------------------------------

  out <- list(
    rows = nrow(x),
    columns = ncol(x),
    geometry_types = geom_type,
    crs_epsg = crs$epsg,
    crs_name = crs$Name,
    valid_count = sum(
      sf::st_is_valid(x)
    ),
    invalid_count = sum(
      !sf::st_is_valid(x)
    ),
    empty_count = sum(
      sf::st_is_empty(x)
    ),
    bbox = sf::st_bbox(x)
  )


  # 04 — Generated count attributes ----------------------------------------

  if ("event_count" %in% names(x)) {
    out$total_event_count <- sum(
      x$event_count,
      na.rm = TRUE
    )

    out$event_count_summary <- summary(
      x$event_count
    )
  }

  if ("exposure_count" %in% names(x)) {
    out$total_exposure_count <- sum(
      x$exposure_count,
      na.rm = TRUE
    )

    out$exposure_count_summary <- summary(
      x$exposure_count
    )
  }

  if ("expected_count" %in% names(x)) {
    out$total_expected_count <- sum(
      x$expected_count,
      na.rm = TRUE
    )

    out$expected_count_summary <- summary(
      x$expected_count
    )
  }


  # 05 — Generated analytical attributes -----------------------------------

  if ("rate_per_1000" %in% names(x)) {
    out$rate_per_1000_summary <- summary(
      x$rate_per_1000
    )
  }


  # 06 — Generated categorical attributes ----------------------------------

  if ("risk_class" %in% names(x)) {
    out$risk_class_count <- table(
      x$risk_class,
      useNA = "ifany"
    )
  }

  if ("hazard_type" %in% names(x)) {
    out$hazard_type_count <- table(
      x$hazard_type,
      useNA = "ifany"
    )
  }

  if ("dominant_hazard_type" %in% names(x)) {
    out$dominant_hazard_type_count <- table(
      x$dominant_hazard_type,
      useNA = "ifany"
    )
  }


  # 07 — Generated area attribute ------------------------------------------

  if ("area" %in% names(x)) {
    out$total_area <- sum(
      x$area,
      na.rm = TRUE
    )

    out$area_summary <- summary(
      x$area
    )
  }


  # 08 — Class and return ---------------------------------------------------

  class(out) <- c(
    "rg_summary",
    class(out)
  )

  out
}