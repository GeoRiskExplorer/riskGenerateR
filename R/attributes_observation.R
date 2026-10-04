# 01 - Generate observation attributes

attributes_observation <- function(
  x,
  seed = NULL
) {

  # 01 - Setup --------------------------------------------------------------

  if (!is.null(seed)) {
    set.seed(seed)
  }

  n <- nrow(x)

  if (n == 0L) {
    x[["observation_id"]] <- character()
    x[["observation_type"]] <- character()
    x[["condition"]] <- character()
    x[["issue_type"]] <- character()
    x[["severity_indicator"]] <- character()
    x[["action_required"]] <- logical()
    x[["action_status"]] <- character()
    x[["observation_date"]] <- as.Date(character())

    return(x)
  }


  # 02 - Define coherent observation contexts ------------------------------

  observation_contexts <- list(

    Access = list(
      conditions = c(
        "Good",
        "Acceptable",
        "Degraded",
        "Poor"
      ),
      issues = c(
        "Track surface deterioration",
        "Access obstruction",
        "Erosion",
        "Drainage issue",
        "Wayfinding issue"
      )
    ),

    Infrastructure = list(
      conditions = c(
        "Good",
        "Acceptable",
        "Degraded",
        "Poor"
      ),
      issues = c(
        "Asset deterioration",
        "Damaged structure",
        "Barrier defect",
        "Signage defect",
        "Maintenance required"
      )
    ),

    Environment = list(
      conditions = c(
        "Good",
        "Acceptable",
        "Degraded",
        "Poor"
      ),
      issues = c(
        "Vegetation damage",
        "Erosion",
        "Waste or litter",
        "Habitat disturbance",
        "Water quality concern"
      )
    ),

    Hazard = list(
      conditions = c(
        "Stable",
        "Changed",
        "Deteriorating",
        "Unsafe"
      ),
      issues = c(
        "Unstable terrain",
        "Falling vegetation",
        "Hazardous water conditions",
        "Fire-related condition",
        "Weather-related condition"
      )
    ),

    Visitor_use = list(
      conditions = c(
        "Normal",
        "Elevated",
        "Congested",
        "Problematic"
      ),
      issues = c(
        "High visitor use",
        "User conflict",
        "Informal access",
        "Unsafe visitor behaviour",
        "Capacity pressure"
      )
    )
  )


  # 03 - Sample observation types ------------------------------------------

  context_name <- sample(
    names(observation_contexts),
    size = n,
    replace = TRUE
  )

  observation_type <- gsub(
    "_",
    " ",
    context_name,
    fixed = TRUE
  )


  # 04 - Generate context-dependent observations ---------------------------

  condition <- character(n)
  issue_type <- character(n)

  for (i in seq_len(n)) {

    context <- observation_contexts[[context_name[[i]]]]

    condition[[i]] <- sample(
      context$conditions,
      size = 1L
    )

    issue_type[[i]] <- sample(
      context$issues,
      size = 1L
    )
  }


  # 05 - Derive severity indicator -----------------------------------------

  severity_indicator <- character(n)

  for (i in seq_len(n)) {

    severity_indicator[[i]] <- switch(
      condition[[i]],

      "Good" = "Low",
      "Acceptable" = "Low",
      "Stable" = "Low",
      "Normal" = "Low",

      "Changed" = "Moderate",
      "Elevated" = "Moderate",

      "Degraded" = "Moderate",
      "Deteriorating" = "High",
      "Congested" = "High",

      "Poor" = "High",
      "Unsafe" = "Critical",
      "Problematic" = "Critical",

      "Moderate"
    )
  }


  # 06 - Derive action requirements ----------------------------------------

  action_required <- severity_indicator %in%
    c(
      "Moderate",
      "High",
      "Critical"
    )

  action_status <- character(n)

  for (i in seq_len(n)) {

    if (!action_required[[i]]) {

      action_status[[i]] <- "No action required"

    } else {

      action_status[[i]] <- sample(
        c(
          "Open",
          "Assigned",
          "In progress",
          "Completed"
        ),
        size = 1L,
        prob = c(
          0.35,
          0.25,
          0.25,
          0.15
        )
      )
    }
  }


  # 07 - Generate observation dates ----------------------------------------

  date_start <- as.Date("2021-01-01")
  date_end <- as.Date("2025-12-31")

  observation_date <- sample(
    seq(
      date_start,
      date_end,
      by = "day"
    ),
    size = n,
    replace = TRUE
  )


  # 08 - Attach generated attributes ---------------------------------------

  x[["observation_id"]] <- sprintf(
    "observation_%06d",
    seq_len(n)
  )

  x[["observation_type"]] <- observation_type
  x[["condition"]] <- condition
  x[["issue_type"]] <- issue_type
  x[["severity_indicator"]] <- severity_indicator
  x[["action_required"]] <- action_required
  x[["action_status"]] <- action_status
  x[["observation_date"]] <- observation_date

  x
}
