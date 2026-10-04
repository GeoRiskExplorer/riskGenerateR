# 01 - Generic attribute live QA

devtools::load_all()


# 02 - Generate spatial support --------------------------------------------

study <- rgr_study_area()

locations <- rgr_points(
  study_area = study,
  n = 25,
  inside_pct = 1,
  seed = 123
)


# 03 - Generate generic attributes -----------------------------------------

records <- rgr_add_attributes(
  locations,
  type = "generic",
  seed = 456
)


# 04 - Console QA -----------------------------------------------------------

cat("\n")
cat("RGR_ADD_ATTRIBUTES - GENERIC LIVE QA\n")
cat("====================================\n")

cat(
  "Rows:                    ",
  nrow(records),
  "\n",
  sep = ""
)

cat(
  "Attribute type:          ",
  paste(
    unique(records[["rgr_attribute_type"]]),
    collapse = ", "
  ),
  "\n",
  sep = ""
)

cat(
  "Categories:              ",
  length(unique(records[["category"]])),
  "\n",
  sep = ""
)

cat(
  "Groups:                  ",
  length(unique(records[["group"]])),
  "\n",
  sep = ""
)

cat(
  "Missing values:          ",
  sum(is.na(records[["value"]])),
  "\n",
  sep = ""
)

cat(
  "Missing counts:          ",
  sum(is.na(records[["count"]])),
  "\n",
  sep = ""
)

cat(
  "Geometry valid:          ",
  all(sf::st_is_valid(records)),
  "\n",
  sep = ""
)

cat(
  "Synthetic XY CRS missing:",
  is.na(sf::st_crs(records)),
  "\n",
  sep = ""
)


# 05 - Inspect records ------------------------------------------------------

cat("\nFIRST 15 RECORDS\n")
cat("----------------\n")

print(
  sf::st_drop_geometry(records)[
    seq_len(min(15L, nrow(records))),
    c(
      "record_id",
      "category",
      "group",
      "status",
      "value",
      "count",
      "record_date"
    )
  ]
)


# 06 - Distribution QA -----------------------------------------------------

cat("\nCATEGORY\n")
cat("--------\n")
print(
  table(records[["category"]])
)

cat("\nGROUP\n")
cat("-----\n")
print(
  table(records[["group"]])
)

cat("\nSTATUS\n")
cat("------\n")
print(
  table(records[["status"]])
)

cat("\nVALUE SUMMARY\n")
cat("-------------\n")
print(
  summary(records[["value"]])
)

cat("\nCOUNT SUMMARY\n")
cat("-------------\n")
print(
  summary(records[["count"]])
)


# 07 - Visual QA ------------------------------------------------------------

plot(
  sf::st_geometry(study),
  main = "Synthetic generic records"
)

plot(
  sf::st_geometry(records),
  add = TRUE,
  pch = 19
)
