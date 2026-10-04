# 01 - Risk-register live QA

devtools::load_all()


# 02 - Generate spatial support --------------------------------------------

study <- rgr_study_area()

risk_locations <- rgr_points(
  study_area = study,
  n = 25,
  inside_pct = 1,
  seed = 123
)


# 03 - Generate risk-register records --------------------------------------

risks <- rgr_add_attributes(
  risk_locations,
  type = "risk_register",
  seed = 456
)


# 04 - Console QA -----------------------------------------------------------

cat("\n")
cat("RGR_ADD_ATTRIBUTES - RISK REGISTER LIVE QA\n")
cat("==========================================\n")

cat("Rows:                    ", nrow(risks), "\n", sep = "")
cat(
  "Attribute type:          ",
  paste(unique(risks[["rgr_attribute_type"]]), collapse = ", "),
  "\n",
  sep = ""
)
cat(
  "Risk categories:         ",
  length(unique(risks[["risk_category"]])),
  "\n",
  sep = ""
)
cat(
  "Missing causes:          ",
  sum(is.na(risks[["cause"]])),
  "\n",
  sep = ""
)
cat(
  "Missing risk events:     ",
  sum(is.na(risks[["risk_event"]])),
  "\n",
  sep = ""
)
cat(
  "Missing consequences:    ",
  sum(is.na(risks[["consequence_description"]])),
  "\n",
  sep = ""
)
cat(
  "Missing controls:        ",
  sum(is.na(risks[["existing_control"]])),
  "\n",
  sep = ""
)
cat(
  "Geometry valid:          ",
  all(sf::st_is_valid(risks)),
  "\n",
  sep = ""
)
cat(
  "Synthetic XY CRS missing:",
  is.na(sf::st_crs(risks)),
  "\n",
  sep = ""
)

cat("\nFIRST 10 RECORDS\n")
cat("----------------\n")

print(
  sf::st_drop_geometry(risks)[
    seq_len(min(10L, nrow(risks))),
    c(
      "risk_id",
      "risk_category",
      "cause",
      "risk_event",
      "consequence_description",
      "existing_control",
      "control_effectiveness",
      "owner_role",
      "review_status"
    )
  ]
)

cat("\nRISK CATEGORY\n")
cat("-------------\n")
print(table(risks[["risk_category"]]))

cat("\nCONTROL EFFECTIVENESS\n")
cat("---------------------\n")
print(table(risks[["control_effectiveness"]]))

cat("\nREVIEW STATUS\n")
cat("-------------\n")
print(table(risks[["review_status"]]))

cat("\nRISK STATEMENTS\n")
cat("---------------\n")
print(
  head(
    risks[["risk_statement"]],
    10
  )
)


# 05 - Visual QA ------------------------------------------------------------

plot(
  sf::st_geometry(study),
  main = "Synthetic risk-register locations"
)

plot(
  sf::st_geometry(risks),
  add = TRUE,
  pch = 19
)
