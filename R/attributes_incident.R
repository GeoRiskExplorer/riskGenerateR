# 01 — Generate incident attributes

rgr_attributes_incident <- function(
  x,
  seed = NULL
) {

  # 01 — Setup --------------------------------------------------------------

  if (!is.null(seed)) {
    set.seed(seed)
  }

  n <- nrow(x)

  if (n == 0L) {
    x[["event_id"]] <- character()
    x[["event_type"]] <- character()
    x[["hazard_category"]] <- character()
    x[["mechanism"]] <- character()
    x[["activity"]] <- character()
    x[["consequence"]] <- character()
    x[["event_date"]] <- as.Date(character())
    x[["event_hour"]] <- integer()
    x[["event_count"]] <- integer()

    return(x)
  }


  # 02 — Define coherent incident contexts ---------------------------------

  incident_contexts <- list(

    Fall = list(
      hazard_category = "Terrain",
      mechanisms = c(
        "Slip, trip or stumble",
        "Fall on same level",
        "Fall from height"
      ),
      activities = c(
        "Walking",
        "Hiking",
        "Sightseeing",
        "General recreation"
      )
    ),

    Water = list(
      hazard_category = "Water",
      mechanisms = c(
        "Immersion",
        "Submersion",
        "Caught in moving water"
      ),
      activities = c(
        "Swimming",
        "Boating",
        "Fishing",
        "Walking near water",
        "General recreation"
      )
    ),

    Vehicle = list(
      hazard_category = "Traffic",
      mechanisms = c(
        "Vehicle collision",
        "Vehicle rollover",
        "Pedestrian interaction",
        "Cyclist interaction"
      ),
      activities = c(
        "Driving",
        "Cycling",
        "Walking",
        "Parking or access"
      )
    ),

    Weather = list(
      hazard_category = "Weather",
      mechanisms = c(
        "Heat exposure",
        "Cold exposure",
        "Severe weather exposure"
      ),
      activities = c(
        "Walking",
        "Hiking",
        "Cycling",
        "Camping",
        "General recreation"
      )
    ),

    Vegetation = list(
      hazard_category = "Vegetation",
      mechanisms = c(
        "Struck by falling branch",
        "Struck by falling tree",
        "Contact with vegetation"
      ),
      activities = c(
        "Walking",
        "Hiking",
        "Camping",
        "General recreation"
      )
    ),

    Wildlife = list(
      hazard_category = "Wildlife",
      mechanisms = c(
        "Bite or sting",
        "Animal contact",
        "Wildlife interaction"
      ),
      activities = c(
        "Walking",
        "Hiking",
        "Camping",
        "Swimming",
        "General recreation"
      )
    ),

    Fire = list(
      hazard_category = "Fire",
      mechanisms = c(
        "Fire exposure",
        "Smoke exposure",
        "Burn"
      ),
      activities = c(
        "Walking",
        "Hiking",
        "Camping",
        "General recreation"
      )
    ),

    Infrastructure = list(
      hazard_category = "Infrastructure",
      mechanisms = c(
        "Contact with structure",
        "Infrastructure failure",
        "Entrapment"
      ),
      activities = c(
        "Walking",
        "Cycling",
        "Using visitor facilities",
        "General recreation"
      )
    )
  )


  # 03 — Sample incident types ---------------------------------------------

  event_type <- sample(
    names(incident_contexts),
    size = n,
    replace = TRUE
  )


  # 04 — Generate incident-dependent context -------------------------------

  hazard_category <- character(n)
  mechanism <- character(n)
  activity <- character(n)

  for (i in seq_len(n)) {

    context <- incident_contexts[[event_type[[i]]]]

    hazard_category[[i]] <- context$hazard_category

    mechanism[[i]] <- sample(
      context$mechanisms,
      size = 1L
    )

    activity[[i]] <- sample(
      context$activities,
      size = 1L
    )
  }


  # 05 — Generate event characteristics ------------------------------------

  consequence <- sample(
    c(
      "Near miss",
      "Minimal",
      "Minor",
      "Moderate",
      "Major",
      "Severe",
      "Fatality"
    ),
    size = n,
    replace = TRUE,
    prob = c(
      0.10,
      0.10,
      0.25,
      0.25,
      0.15,
      0.10,
      0.05
    )
  )

  event_hour <- sample(
    0:23,
    size = n,
    replace = TRUE
  )

  date_start <- as.Date("2021-01-01")
  date_end <- as.Date("2025-12-31")

  event_date <- date_start + sample(
    0:as.integer(date_end - date_start),
    size = n,
    replace = TRUE
  )


  # 06 — Attach generated attributes ---------------------------------------

  x[["event_id"]] <- sprintf(
    "event_%06d",
    seq_len(n)
  )

  x[["event_type"]] <- event_type
  x[["hazard_category"]] <- hazard_category
  x[["mechanism"]] <- mechanism
  x[["activity"]] <- activity
  x[["consequence"]] <- consequence
  x[["event_date"]] <- event_date
  x[["event_hour"]] <- event_hour
  x[["event_count"]] <- rep(1L, n)

  x
}
