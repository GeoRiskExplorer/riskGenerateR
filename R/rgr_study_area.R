# 01 - Generate study area

#' Generate a synthetic study area
#'
#' Creates a rectangular study area from either a named numeric bounding
#' box or an `sf::st_bbox` object. The study area may use a defined CRS
#' or remain in neutral synthetic XY coordinate space.
#'
#' @param bbox Either a named numeric vector containing `xmin`, `ymin`,
#'   `xmax`, and `ymax`, or an object of class `bbox` created by
#'   [sf::st_bbox()].
#' @param crs Optional coordinate reference system. Defaults to `NA` for
#'   neutral synthetic XY space. When `bbox` is an `sf::st_bbox` object,
#'   its CRS is retained unless `crs` is explicitly supplied.
#' @param area_id Area identifier.
#' @param area_name Area name.
#'
#' @return An sf polygon object containing one study-area feature.
#'
#' @export
rgr_study_area <- function(
  bbox = c(
    xmin = 0,
    ymin = 0,
    xmax = 5000,
    ymax = 5000
  ),
  crs = NA,
  area_id = "study_area_001",
  area_name = "Synthetic Study Area"
) {

  # 01 - Validate package availability --------------------------------------

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.", call. = FALSE)
  }


  # 02 - Resolve bounding box input -----------------------------------------

  if (inherits(bbox, "bbox")) {

    bbox_crs <- sf::st_crs(bbox)

    bbox_values <- as.numeric(bbox)

    names(bbox_values) <- c(
      "xmin",
      "ymin",
      "xmax",
      "ymax"
    )

  } else {

    bbox_crs <- sf::st_crs(NA)

    if (!is.numeric(bbox)) {
      stop(
        "bbox must be a named numeric vector or an sf bbox object.",
        call. = FALSE
      )
    }

    required_names <- c(
      "xmin",
      "ymin",
      "xmax",
      "ymax"
    )

    if (
      length(bbox) != 4L ||
      is.null(names(bbox)) ||
      !all(required_names %in% names(bbox))
    ) {
      stop(
        paste0(
          "bbox must contain exactly four named values: ",
          "xmin, ymin, xmax, ymax."
        ),
        call. = FALSE
      )
    }

    bbox_values <- bbox[required_names]
  }


  # 03 - Validate bounding box values ---------------------------------------

  if (any(!is.finite(bbox_values))) {
    stop(
      "bbox values must all be finite numeric values.",
      call. = FALSE
    )
  }

  if (
    bbox_values[["xmax"]] <= bbox_values[["xmin"]] ||
    bbox_values[["ymax"]] <= bbox_values[["ymin"]]
  ) {
    stop(
      "bbox xmax/ymax must be greater than xmin/ymin.",
      call. = FALSE
    )
  }


  # 04 - Resolve output CRS --------------------------------------------------

  crs_supplied <- !(
    length(crs) == 1L &&
    is.atomic(crs) &&
    is.na(crs)
  )

  if (crs_supplied) {
    output_crs <- sf::st_crs(crs)
  } else {
    output_crs <- bbox_crs
  }


  # 05 - Create study-area geometry -----------------------------------------

  bbox_obj <- sf::st_bbox(
    bbox_values,
    crs = output_crs
  )

  geom <- sf::st_as_sfc(
    bbox_obj
  )


  # 06 - Return --------------------------------------------------------------

  sf::st_sf(
    area_id = area_id,
    area_name = area_name,
    geometry = geom
  )
}
