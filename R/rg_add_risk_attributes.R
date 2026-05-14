# 01 — Add synthetic risk attributes

#' Add synthetic risk attributes to spatial features
#'
#' @param x An sf object.
#' @param target Character. One of `"points"` or `"polygons"`. If NULL, inferred from geometry.
#' @param mode Character. One of `"case"` or `"count"`. If NULL, inferred from geometry.
#' @param seed Optional random seed.
#'
#' @return An sf object with synthetic risk attributes.
#' @export
rg_add_risk_attributes <- function(
  x,
  target = NULL,
  mode = NULL,
  seed = NULL
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.", call. = FALSE)
  }

  if (!inherits(x, "sf")) {
    stop("x must be an sf object.", call. = FALSE)
  }

  geom_type <- unique(as.character(sf::st_geometry_type(x)))
  n <- nrow(x)

  if (is.null(target)) {
    if (all(geom_type %in% c("POINT", "MULTIPOINT"))) {
      target <- "points"
    } else if (all(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
      target <- "polygons"
    } else {
      stop("Could not infer target from geometry type.", call. = FALSE)
    }
  }

  if (!target %in% c("points", "polygons")) {
    stop("target must be one of: points, polygons.", call. = FALSE)
  }

  if (is.null(mode)) {
    mode <- if (target == "points") "case" else "count"
  }

  if (!mode %in% c("case", "count")) {
    stop("mode must be one of: case, count.", call. = FALSE)
  }

  if (target == "polygons" && mode == "case") {
    warning(
      "Polygons usually represent aggregated units. Consider mode = 'count'.",
      call. = FALSE
    )
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  hazard_type <- sample(
    c("Falls", "Water", "Vehicle", "Fire", "Wildlife", "Medical"),
    size = n,
    replace = TRUE
  )

  consequence <- sample(
    c("Minimal", "Minor", "Moderate", "Major", "Severe"),
    size = n,
    replace = TRUE,
    prob = c(0.30, 0.35, 0.20, 0.10, 0.05)
  )

  likelihood <- sample(
    c("Rare", "Unlikely", "Possible", "Likely", "Almost Certain"),
    size = n,
    replace = TRUE,
    prob = c(0.15, 0.25, 0.35, 0.20, 0.05)
  )

  consequence_score <- match(
    consequence,
    c("Minimal", "Minor", "Moderate", "Major", "Severe")
  )

  likelihood_score <- match(
    likelihood,
    c("Rare", "Unlikely", "Possible", "Likely", "Almost Certain")
  )

  risk_score <- consequence_score * likelihood_score

  risk_class <- cut(
    risk_score,
    breaks = c(-Inf, 4, 9, 14, Inf),
    labels = c("Low", "Medium", "High", "Extreme"),
    right = TRUE
  )

  if (mode == "case") {
    x$hazard_type <- hazard_type
    x$consequence <- consequence
    x$likelihood <- likelihood

    x$event_id <- sprintf("event_%06d", seq_len(n))
    x$event_count <- 1L
    x$case_date <- sample(
      seq.Date(
        from = as.Date("2024-01-01"),
        to = as.Date("2024-12-31"),
        by = "day"
      ),
      size = n,
      replace = TRUE
    )
    x$hour <- sample(0:23, size = n, replace = TRUE)
  }

  if (mode == "count") {
    x$dominant_hazard_type <- hazard_type
    x$dominant_consequence <- consequence
    x$dominant_likelihood <- likelihood

    x$event_count <- stats::rpois(n = n, lambda = 5)
    x$exposure_count <- pmax(
      x$event_count + 1L,
      stats::rpois(n = n, lambda = 100)
    )
    x$expected_count <- round(x$exposure_count * 0.05, 3)
    x$rate_per_1000 <- round((x$event_count / x$exposure_count) * 1000, 3)
  }

  x$consequence_score <- consequence_score
  x$likelihood_score <- likelihood_score
  x$risk_score <- risk_score
  x$risk_class <- as.character(risk_class)

  x$rg_target <- target
  x$rg_mode <- mode

  x
}