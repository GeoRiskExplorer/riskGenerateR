# =========================================================
# 01 — Prepare ABS LGA test polygons for riskGenerateR
# =========================================================

library(sf)
library(dplyr)
library(qs2)

devtools::load_all(".")

# =========================================================
# 02 — Paths
# =========================================================

gpkg_path <- "E:/ABS_Geography/ASGS July 2021 - June 2026/ASGS_Ed3_Non_ABS_Structures_GDA2020_updated_2025/ASGS_Ed3_Non_ABS_Structures_GDA2020_updated_2025.gpkg"

layer_name <- "LGA_2025_AUST_GDA2020"

out_dir <- "dev/02_geometry_checks/data_lga_test_polygons"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# =========================================================
# 03 — Read full LGA layer
# =========================================================

lga_2025 <- sf::st_read(
  dsn = gpkg_path,
  layer = layer_name,
  quiet = FALSE
)

# =========================================================
# 04 — Basic geometry normalisation for dev testing
# =========================================================

lga_2025 <- sf::st_make_valid(lga_2025)

# =========================================================
# 05 — Create state subsets
# =========================================================

lga_vic_2025 <- lga_2025 |>
  dplyr::filter(STATE_NAME_2021 == "Victoria")

lga_wa_2025 <- lga_2025 |>
  dplyr::filter(STATE_NAME_2021 == "Western Australia")

# =========================================================
# 06 — Find Cottesloe robustly
# =========================================================

possible_lga_name_cols <- c(
  "LGA_NAME_2025",
  "LGA_NAME_2021",
  "LGA_NAME",
  "LGA_NAME_2024"
)

lga_name_col <- possible_lga_name_cols[
  possible_lga_name_cols %in% names(lga_2025)
][1]

if (is.na(lga_name_col)) {
  stop("Could not find an LGA name column.", call. = FALSE)
}

cottesloe_lga_2025 <- lga_2025 |>
  dplyr::filter(
    STATE_NAME_2021 == "Western Australia",
    grepl("Cottesloe", .data[[lga_name_col]], ignore.case = TRUE)
  )

if (nrow(cottesloe_lga_2025) != 1) {
  stop(
    "Expected exactly one Cottesloe LGA row, but found: ",
    nrow(cottesloe_lga_2025),
    call. = FALSE
  )
}

# =========================================================
# 07 — Add area fields for local QA
# =========================================================

add_area_fields <- function(x) {
  x$area_m2 <- as.numeric(sf::st_area(x))
  x$area_km2 <- round(x$area_m2 / 1e6, 4)
  x
}

lga_2025 <- add_area_fields(lga_2025)
lga_vic_2025 <- add_area_fields(lga_vic_2025)
lga_wa_2025 <- add_area_fields(lga_wa_2025)
cottesloe_lga_2025 <- add_area_fields(cottesloe_lga_2025)

# =========================================================
# 08 — Save local dev test objects
# =========================================================

qs2::qs_save(
  lga_2025,
  file = file.path(out_dir, "lga_2025_aust_gda2020.qs2")
)

qs2::qs_save(
  lga_vic_2025,
  file = file.path(out_dir, "lga_2025_vic_gda2020.qs2")
)

qs2::qs_save(
  lga_wa_2025,
  file = file.path(out_dir, "lga_2025_wa_gda2020.qs2")
)

qs2::qs_save(
  cottesloe_lga_2025,
  file = file.path(out_dir, "lga_2025_cottesloe_gda2020.qs2")
)

# =========================================================
# 09 — QA
# =========================================================

cat("\n--- LGA TEST POLYGON QA ---\n")
cat("Full LGA rows:", nrow(lga_2025), "\n")
cat("Victoria LGA rows:", nrow(lga_vic_2025), "\n")
cat("WA LGA rows:", nrow(lga_wa_2025), "\n")
cat("Cottesloe rows:", nrow(cottesloe_lga_2025), "\n")
cat("LGA name column used:", lga_name_col, "\n")
cat("CRS EPSG:", sf::st_crs(lga_2025)$epsg, "\n")

cat("\n--- COTTESLOE QA ---\n")
print(
  cottesloe_lga_2025 |>
    sf::st_drop_geometry() |>
    dplyr::select(
      dplyr::any_of(c(
        "STATE_NAME_2021",
        "STATE_CODE_2021",
        "STATE_CODE_2025",
        lga_name_col,
        "area_km2"
      ))
    )
)

cat("\nGeometry valid:", all(sf::st_is_valid(cottesloe_lga_2025)), "\n")
cat("Geometry empty:", any(sf::st_is_empty(cottesloe_lga_2025)), "\n")

cat("\nSaved to:\n")
cat(out_dir, "\n")