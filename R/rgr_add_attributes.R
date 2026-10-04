# 01 - Add synthetic contextual attributes

#' Add synthetic contextual attributes
#'
#' Adds coherent synthetic contextual attributes to an sf object for use in
#' developing, testing and demonstrating risk-related analytical workflows.
#'
#' riskGenerateR generates analytical inputs. It does not calculate or infer
#' risk ratings or other analytical outputs.
#'
#' @param x An sf object.
#' @param type Attribute typology. Currently `"hazard_assessment"`,
#'   `"incident"`, `"risk_register"`, `"observation"`, or `"generic"`.
#' @param seed Optional random seed.
#'
#' @return An sf object containing the original features and generated
#'   contextual attributes.
#'
#' @export
rgr_add_attributes <- function(
  x,
  type = "hazard_assessment",
  seed = NULL
) {

  # 01 - Validate input -----------------------------------------------------

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

  if (
    length(type) != 1L ||
    !is.character(type) ||
    is.na(type)
  ) {
    stop(
      "type must be a single character value.",
      call. = FALSE
    )
  }

supported_types <- c(
  "hazard_assessment",
  "incident",
  "risk_register",
  "observation",
  "generic"
)

  if (!type %in% supported_types) {
    stop(
      "Unsupported attribute type: ",
      type,
      ". Currently supported: ",
      paste(supported_types, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (
    !is.null(seed) &&
    (
      length(seed) != 1L ||
      !is.numeric(seed) ||
      is.na(seed) ||
      !is.finite(seed)
    )
  ) {
    stop(
      "seed must be NULL or a single finite numeric value.",
      call. = FALSE
    )
  }


  # 02 - Dispatch attribute generator --------------------------------------

 if (identical(type, "hazard_assessment")) {
  x <- attributes_hazard_assessment(
    x,
    seed = seed
  )
}

  if (identical(type, "incident")) {
    x <- rgr_attributes_incident(
      x,
      seed = seed
    )
  }

  if (identical(type, "risk_register")) {
  x <- attributes_risk_register(
    x,
    seed = seed
  )
}
  if (identical(type, "observation")) {
  x <- attributes_observation(
    x,
    seed = seed
  )
}
  
  if (identical(type, "generic")) {
  x <- attributes_generic(
    x,
    seed = seed
  )
}


  # 03 - Record attribute typology -----------------------------------------

  x[["rgr_attribute_type"]] <- rep(
    type,
    nrow(x)
  )


  # 04 - Return -------------------------------------------------------------

  x
}
