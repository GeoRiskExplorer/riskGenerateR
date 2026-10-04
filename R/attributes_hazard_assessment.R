# 01 - Generate hazard-assessment attributes

attributes_hazard_assessment <- function(
  x,
  seed = NULL
) {

  # 01 - Setup --------------------------------------------------------------

  if (!is.null(seed)) {
    set.seed(seed)
  }

  n <- nrow(x)

  if (n == 0L) {
    x[["assessment_id"]] <- character()
    x[["hazard_category"]] <- character()
    x[["hazard_type"]] <- character()
    x[["hazard_source"]] <- character()
    x[["hazard_description"]] <- character()
    x[["exposed_group"]] <- character()
    x[["exposure_context"]] <- character()
    x[["existing_control"]] <- character()
    x[["control_effectiveness"]] <- character()
    x[["consequence"]] <- character()
    x[["likelihood"]] <- character()
    x[["assessment_status"]] <- character()

    return(x)
  }


  # 02 - Define coherent hazard contexts -----------------------------------

  hazard_contexts <- list(

    Terrain = list(
      hazard_type = "Fall from height",
      hazard_source = "Cliff, escarpment or steep terrain",
      hazard_description =
        "Exposure to a fall from steep or elevated terrain.",
      exposed_groups = c(
        "General visitors",
        "Walkers and hikers",
        "Children and families",
        "Remote-area visitors"
      ),
      exposure_contexts = c(
        "Routine access",
        "High visitor use",
        "Remote access",
        "Seasonal use",
        "Activity-specific use"
      ),
      controls = c(
        "Warning signage",
        "Physical barrier",
        "Defined access route",
        "Visitor information",
        "Temporary closure",
        "Operational monitoring"
      )
    ),

    Water = list(
      hazard_type = "Deep or moving water",
      hazard_source = "Coast, river, lake or waterbody",
      hazard_description =
        "Exposure to drowning or injury in deep, moving or hazardous water.",
      exposed_groups = c(
        "General visitors",
        "Water users",
        "Children and families",
        "Walkers and hikers"
      ),
      exposure_contexts = c(
        "High visitor use",
        "Seasonal use",
        "Weather-dependent use",
        "Activity-specific use"
      ),
      controls = c(
        "Warning signage",
        "Physical barrier",
        "Visitor information",
        "Defined access route",
        "Temporary closure",
        "Operational monitoring"
      )
    ),

    Weather = list(
      hazard_type = "Extreme heat",
      hazard_source = "High temperature and exposed environment",
      hazard_description =
        "Exposure to heat stress, dehydration or heat-related illness.",
      exposed_groups = c(
        "General visitors",
        "Walkers and hikers",
        "Children and families",
        "Cyclists",
        "Remote-area visitors"
      ),
      exposure_contexts = c(
        "High visitor use",
        "Remote access",
        "Seasonal use",
        "Weather-dependent use",
        "Activity-specific use"
      ),
      controls = c(
        "Warning signage",
        "Visitor information",
        "Temporary closure",
        "Operational monitoring"
      )
    ),

    Traffic = list(
      hazard_type = "Vehicle interaction",
      hazard_source = "Road, car park or shared access route",
      hazard_description =
        "Exposure to collision or conflict between vehicles and people.",
      exposed_groups = c(
        "General visitors",
        "Walkers and hikers",
        "Children and families",
        "Cyclists",
        "Drivers and passengers"
      ),
      exposure_contexts = c(
        "Routine access",
        "High visitor use",
        "Seasonal use",
        "Activity-specific use"
      ),
      controls = c(
        "Warning signage",
        "Physical barrier",
        "Defined access route",
        "Visitor information",
        "Operational monitoring"
      )
    ),

    Vegetation = list(
      hazard_type = "Falling vegetation",
      hazard_source = "Trees, branches or unstable vegetation",
      hazard_description =
        "Exposure to falling trees, branches or unstable vegetation.",
      exposed_groups = c(
        "General visitors",
        "Walkers and hikers",
        "Children and families",
        "Cyclists",
        "Remote-area visitors"
      ),
      exposure_contexts = c(
        "Routine access",
        "High visitor use",
        "Remote access",
        "Weather-dependent use"
      ),
      controls = c(
        "Warning signage",
        "Defined access route",
        "Inspection and maintenance",
        "Temporary closure",
        "Operational monitoring"
      )
    ),

    Wildlife = list(
      hazard_type = "Wildlife interaction",
      hazard_source = "Wild or hazardous fauna",
      hazard_description =
        "Exposure to injury arising from interaction with wildlife.",
      exposed_groups = c(
        "General visitors",
        "Walkers and hikers",
        "Children and families",
        "Water users",
        "Remote-area visitors"
      ),
      exposure_contexts = c(
        "Routine access",
        "Remote access",
        "Seasonal use",
        "Activity-specific use"
      ),
      controls = c(
        "Warning signage",
        "Visitor information",
        "Defined access route",
        "Temporary closure",
        "Operational monitoring"
      )
    ),

    Fire = list(
      hazard_type = "Bushfire",
      hazard_source = "Vegetation, ignition source and fire conditions",
      hazard_description =
        "Exposure to fire, smoke or rapidly changing bushfire conditions.",
      exposed_groups = c(
        "General visitors",
        "Walkers and hikers",
        "Children and families",
        "Remote-area visitors"
      ),
      exposure_contexts = c(
        "Remote access",
        "Seasonal use",
        "Weather-dependent use"
      ),
      controls = c(
        "Warning signage",
        "Visitor information",
        "Temporary closure",
        "Operational monitoring"
      )
    ),

    Infrastructure = list(
      hazard_type = "Infrastructure failure",
      hazard_source = "Built structure, barrier or visitor facility",
      hazard_description =
        "Exposure to failure or poor condition of visitor infrastructure.",
      exposed_groups = c(
        "General visitors",
        "Walkers and hikers",
        "Children and families",
        "Cyclists"
      ),
      exposure_contexts = c(
        "Routine access",
        "High visitor use",
        "Seasonal use",
        "Activity-specific use"
      ),
      controls = c(
        "Warning signage",
        "Physical barrier",
        "Defined access route",
        "Inspection and maintenance",
        "Temporary closure"
      )
    )
  )


  # 03 - Sample hazard categories ------------------------------------------

  hazard_category <- sample(
    names(hazard_contexts),
    size = n,
    replace = TRUE
  )


  # 04 - Generate hazard-dependent context ---------------------------------

  hazard_type <- character(n)
  hazard_source <- character(n)
  hazard_description <- character(n)

  exposed_group <- character(n)
  exposure_context <- character(n)
  existing_control <- character(n)

  for (i in seq_len(n)) {

    context <- hazard_contexts[[hazard_category[[i]]]]

    hazard_type[[i]] <- context$hazard_type
    hazard_source[[i]] <- context$hazard_source
    hazard_description[[i]] <- context$hazard_description

    exposed_group[[i]] <- sample(
      context$exposed_groups,
      size = 1L
    )

    exposure_context[[i]] <- sample(
      context$exposure_contexts,
      size = 1L
    )

    existing_control[[i]] <- sample(
      context$controls,
      size = 1L
    )
  }


  # 05 - Generate assessment inputs ----------------------------------------

  control_effectiveness <- sample(
    c(
      "Effective",
      "Mostly effective",
      "Partially effective",
      "Limited",
      "Unknown"
    ),
    size = n,
    replace = TRUE,
    prob = c(
      0.15,
      0.25,
      0.30,
      0.15,
      0.15
    )
  )

  consequence <- sample(
    c(
      "Minimal",
      "Minor",
      "Moderate",
      "Major",
      "Severe"
    ),
    size = n,
    replace = TRUE,
    prob = c(
      0.10,
      0.20,
      0.30,
      0.25,
      0.15
    )
  )

  likelihood <- sample(
    c(
      "Rare",
      "Unlikely",
      "Possible",
      "Likely",
      "Almost Certain"
    ),
    size = n,
    replace = TRUE,
    prob = c(
      0.15,
      0.25,
      0.35,
      0.20,
      0.05
    )
  )

  assessment_status <- sample(
    c(
      "Draft",
      "Under review",
      "Current",
      "Review required"
    ),
    size = n,
    replace = TRUE,
    prob = c(
      0.15,
      0.15,
      0.55,
      0.15
    )
  )


  # 06 - Attach generated attributes ---------------------------------------

  x[["assessment_id"]] <- sprintf(
    "assessment_%06d",
    seq_len(n)
  )

  x[["hazard_category"]] <- hazard_category
  x[["hazard_type"]] <- hazard_type
  x[["hazard_source"]] <- hazard_source
  x[["hazard_description"]] <- hazard_description

  x[["exposed_group"]] <- exposed_group
  x[["exposure_context"]] <- exposure_context
  x[["existing_control"]] <- existing_control

  x[["control_effectiveness"]] <- control_effectiveness
  x[["consequence"]] <- consequence
  x[["likelihood"]] <- likelihood
  x[["assessment_status"]] <- assessment_status

  x
}
