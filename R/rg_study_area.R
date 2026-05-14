# 01 — Generate study area

#' Generate a rectangular synthetic study area
#'
#' @param bbox Named numeric vector with xmin, ymin, xmax, ymax.
#' @param crs Coordinate reference system. Defaults to NA for neutral synthetic space.
#' @param area_id Area identifier.
#' @param area_name Area name.
#'
#' @return An sf polygon object.
#' @export
rg_study_area <- function(
  bbox = c(xmin = 0, ymin = 0, xmax = 5000, ymax = 5000),
  crs = NA,
  area_id = "study_area_001",
  area_name = "Synthetic Study Area"
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.", call. = FALSE)
  }

  required_names <- c("xmin", "ymin", "xmax", "ymax")

  if (!all(required_names %in% names(bbox))) {
    stop("bbox must be a named numeric vector with xmin, ymin, xmax, ymax.", call. = FALSE)
  }

  if (bbox[["xmax"]] <= bbox[["xmin"]] || bbox[["ymax"]] <= bbox[["ymin"]]) {
    stop("bbox xmax/ymax must be greater than xmin/ymin.", call. = FALSE)
  }

  bbox_obj <- sf::st_bbox(bbox, crs = sf::st_crs(crs))
  geom <- sf::st_as_sfc(bbox_obj)

  sf::st_sf(
    area_id = area_id,
    area_name = area_name,
    geometry = geom
  )
}