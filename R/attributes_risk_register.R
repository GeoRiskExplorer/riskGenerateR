# 01 - Generate risk-register attributes

attributes_risk_register <- function(
  x,
  seed = NULL
) {

  # 01 - Setup --------------------------------------------------------------

  if (!is.null(seed)) {
    set.seed(seed)
  }

  n <- nrow(x)

  if (n == 0L) {
    x[["risk_id"]] <- character()
    x[["objective"]] <- character()
    x[["risk_category"]] <- character()
    x[["cause"]] <- character()
    x[["risk_event"]] <- character()
    x[["consequence_description"]] <- character()
    x[["existing_control"]] <- character()
    x[["control_effectiveness"]] <- character()
    x[["owner_role"]] <- character()
    x[["review_status"]] <- character()
    x[["risk_statement"]] <- character()

    return(x)
  }


  # 02 - Define coherent risk contexts -------------------------------------

  risk_contexts <- list(

    Visitor_safety = list(
      objective =
        "Provide safe and accessible visitor experiences",
      risk_category =
        "Visitor safety",
      causes = c(
        "Exposure to hazardous terrain",
        "Exposure to hazardous water",
        "Changing environmental conditions",
        "Unsafe interaction with visitor infrastructure"
      ),
      risk_events = c(
        "A visitor is injured or killed",
        "Visitor becomes stranded or requires rescue",
        "Visitor is exposed to an uncontrolled hazard"
      ),
      consequences = c(
        "Serious harm to visitors",
        "Emergency response and rescue requirements",
        "Reduced visitor confidence and access"
      ),
      controls = c(
        "Warning signage",
        "Visitor information",
        "Defined access routes",
        "Physical barriers",
        "Operational monitoring",
        "Temporary closure"
      ),
      owners = c(
        "Visitor safety manager",
        "Operations manager",
        "Area manager",
        "Site manager"
      )
    ),

    Environment = list(
      objective =
        "Protect environmental and ecological values",
      risk_category =
        "Environment",
      causes = c(
        "High visitor pressure",
        "Unauthorised access",
        "Disturbance of sensitive habitat",
        "Pollution or waste",
        "Extreme environmental conditions"
      ),
      risk_events = c(
        "Environmental values are degraded",
        "Sensitive habitat is disturbed",
        "Protected areas are damaged"
      ),
      consequences = c(
        "Loss or degradation of environmental values",
        "Damage to sensitive habitat",
        "Reduced ecological condition"
      ),
      controls = c(
        "Defined access routes",
        "Environmental monitoring",
        "Visitor information",
        "Access restrictions",
        "Site rehabilitation",
        "Temporary closure"
      ),
      owners = c(
        "Environmental manager",
        "Operations manager",
        "Area manager",
        "Site manager"
      )
    ),

    Infrastructure = list(
      objective =
        "Maintain safe and serviceable infrastructure",
      risk_category =
        "Infrastructure",
      causes = c(
        "Asset deterioration",
        "Deferred maintenance",
        "Extreme weather",
        "Unexpected asset failure",
        "High visitor use"
      ),
      risk_events = c(
        "Visitor infrastructure becomes unsafe",
        "Infrastructure fails or becomes unavailable",
        "Access is disrupted by asset condition"
      ),
      consequences = c(
        "Injury arising from infrastructure failure",
        "Loss of access or service availability",
        "Unplanned maintenance or replacement"
      ),
      controls = c(
        "Routine inspection",
        "Preventive maintenance",
        "Physical barriers",
        "Temporary closure",
        "Asset condition monitoring",
        "Warning signage"
      ),
      owners = c(
        "Asset manager",
        "Operations manager",
        "Area manager",
        "Site manager"
      )
    ),

    Operations = list(
      objective =
        "Maintain effective and resilient operations",
      risk_category =
        "Operations",
      causes = c(
        "Resource constraints",
        "Communication failure",
        "Process failure",
        "Extreme weather",
        "Unexpected demand"
      ),
      risk_events = c(
        "Operational response is delayed",
        "Required services cannot be delivered",
        "Operational controls are not implemented"
      ),
      consequences = c(
        "Reduced service delivery",
        "Delayed response to emerging hazards",
        "Increased operational disruption"
      ),
      controls = c(
        "Operational procedures",
        "Staff training",
        "Incident response arrangements",
        "Operational monitoring",
        "Contingency planning",
        "Escalation procedures"
      ),
      owners = c(
        "Operations manager",
        "Area manager",
        "Duty manager",
        "Program manager"
      )
    ),

    Emergency_management = list(
      objective =
        "Maintain effective emergency preparedness and response",
      risk_category =
        "Emergency management",
      causes = c(
        "Severe weather",
        "Bushfire conditions",
        "Flooding",
        "Remote access",
        "Communication failure"
      ),
      risk_events = c(
        "Emergency response is delayed",
        "Visitors cannot evacuate safely",
        "Emergency conditions exceed local response capacity"
      ),
      consequences = c(
        "Serious harm to visitors or staff",
        "Large-scale emergency response requirements",
        "Loss of access and prolonged closure"
      ),
      controls = c(
        "Emergency response plans",
        "Evacuation procedures",
        "Warning systems",
        "Temporary closure",
        "Operational monitoring",
        "Emergency service liaison"
      ),
      owners = c(
        "Emergency management coordinator",
        "Operations manager",
        "Area manager",
        "Duty manager"
      )
    )
  )


  # 03 - Sample risk contexts ----------------------------------------------

  context_name <- sample(
    names(risk_contexts),
    size = n,
    replace = TRUE
  )


  # 04 - Generate context-dependent attributes -----------------------------

  objective <- character(n)
  risk_category <- character(n)
  cause <- character(n)
  risk_event <- character(n)
  consequence_description <- character(n)
  existing_control <- character(n)
  owner_role <- character(n)

  for (i in seq_len(n)) {

    context <- risk_contexts[[context_name[[i]]]]

    objective[[i]] <- context$objective
    risk_category[[i]] <- context$risk_category

    cause[[i]] <- sample(
      context$causes,
      size = 1L
    )

    risk_event[[i]] <- sample(
      context$risk_events,
      size = 1L
    )

    consequence_description[[i]] <- sample(
      context$consequences,
      size = 1L
    )

    existing_control[[i]] <- sample(
      context$controls,
      size = 1L
    )

    owner_role[[i]] <- sample(
      context$owners,
      size = 1L
    )
  }


  # 05 - Generate register-management attributes ---------------------------

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

  review_status <- sample(
    c(
      "Current",
      "Review required",
      "Under review",
      "Draft"
    ),
    size = n,
    replace = TRUE,
    prob = c(
      0.55,
      0.15,
      0.15,
      0.15
    )
  )


  # 06 - Construct risk statements -----------------------------------------

  risk_statement <- paste0(
    "Due to ",
    tolower(cause),
    ", there is a risk that ",
    tolower(risk_event),
    ", resulting in ",
    tolower(consequence_description),
    "."
  )


  # 07 - Attach generated attributes ---------------------------------------

  x[["risk_id"]] <- sprintf(
    "risk_%06d",
    seq_len(n)
  )

  x[["objective"]] <- objective
  x[["risk_category"]] <- risk_category
  x[["cause"]] <- cause
  x[["risk_event"]] <- risk_event
  x[["consequence_description"]] <- consequence_description
  x[["existing_control"]] <- existing_control
  x[["control_effectiveness"]] <- control_effectiveness
  x[["owner_role"]] <- owner_role
  x[["review_status"]] <- review_status
  x[["risk_statement"]] <- risk_statement

  x
}
