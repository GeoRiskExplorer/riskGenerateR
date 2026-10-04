# 01 - Observation live QA

devtools::load_all()


# 02 - Generate spatial support --------------------------------------------

study <- rgr_study_area()

observation_locations <- rgr_points(
  study_area = study,
  n = 25,
  inside_pct = 1,
  seed = 123
)


# 03 - Generate observation records ----------------------------------------

observations <- rgr_add_attributes(
  observation_locations,
  type = "observation",
  seed = 456
)


# 04 - Console QA -----------------------------------------------------------

cat("\n")
cat("RGR_ADD_ATTRIBUTES - OBSERVATION LIVE QA\n")
cat("========================================\n")

cat("Rows:                    ", nrow(observations), "\n", sep = "")

cat(
  "Attribute type:          ",
  paste(
    unique(observations[["rgr_attribute_type"]]),
    collapse = ", "
  ),
  "\n",
  sep = ""
)

cat(
  "Observation types:       ",
  length(unique(observations[["observation_type"]])),
  "\n",
  sep = ""
)

cat(
  "Missing conditions:      ",
  sum(is.na(observations[["condition"]])),
  "\n",
  sep = ""
)

cat(
  "Missing issues:          ",
  sum(is.na(observations[["issue_type"]])),
  "\n",
  sep = ""
)

cat(
  "Actions required:        ",
  sum(observations[["action_required"]]),
  "\n",
  sep = ""
)

cat(
  "Geometry valid:          ",
  all(sf::st_is_valid(observations)),
  "\n",
  sep = ""
)

cat(
  "Synthetic XY CRS missing:",
  is.na(sf::st_crs(observations)),
  "\n",
  sep = ""
)


# 05 - Inspect generated records -------------------------------------------

cat("\nFIRST 15 RECORDS\n")
cat("----------------\n")

print(
  sf::st_drop_geometry(observations)[
    seq_len(min(15L, nrow(observations))),
    c(
      "observation_id",
      "observation_type",
      "condition",
      "issue_type",
      "severity_indicator",
      "action_required",
      "action_status",
      "observation_date"
    )
  ]
)


# 06 - Distribution QA -----------------------------------------------------

cat("\nOBSERVATION TYPE\n")
cat("----------------\n")
print(
  table(observations[["observation_type"]])
)

cat("\nCONDITION\n")
cat("---------\n")
print(
  table(observations[["condition"]])
)

cat("\nSEVERITY INDICATOR\n")
cat("------------------\n")
print(
  table(observations[["severity_indicator"]])
)

cat("\nACTION STATUS\n")
cat("-------------\n")
print(
  table(observations[["action_status"]])
)


# 07 - Relationship QA -----------------------------------------------------

cat("\nCONDITION / SEVERITY\n")
cat("--------------------\n")
print(
  unique(
    sf::st_drop_geometry(observations)[
      ,
      c(
        "condition",
        "severity_indicator"
      )
    ]
  )
)

cat("\nSEVERITY / ACTION REQUIRED\n")
cat("--------------------------\n")
print(
  table(
    observations[["severity_indicator"]],
    observations[["action_required"]]
  )
)


# 08 - Visual QA ------------------------------------------------------------

plot(
  sf::st_geometry(study),
  main = "Synthetic observation locations"
)

plot(
  sf::st_geometry(observations),
  add = TRUE,
  pch = 19
)
